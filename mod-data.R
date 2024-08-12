# upload data module 

data_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Upload Data",
    value = ns("tab"),
    tagList(
      useShinyFeedback(),
      div(
        style = "margin-bottom: 50px;",
        h2("Upload Data"),
        fileInput(ns("upload"), NULL, buttonLabel = "Browse", accept = ".csv"),
        DT::dataTableOutput(ns("raw_data")),
        uiOutput(ns("next_varselect")),
        uiOutput(ns("varselect")),
        uiOutput(ns("next_varverify")),
        uiOutput(ns("varverify")),
        DT::dataTableOutput(ns("varverify_table")),
        uiOutput(ns("next_emprevalence")),
        uiOutput(ns("emprevalence")),
        DT::dataTableOutput(ns("emprevalence_table")),
        uiOutput(ns("next_bootstrap"))
      )
    )
  )
}

data_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # load data from file upload
    data <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(
        ext,
        csv = vroom::vroom(input$upload$datapath, delim=","),
        validate("Invalid file; please upload a .csv file!")
      )
    })
    
    # add raw data to the store
    observeEvent(data(), {
      store$data <- NULL
      store$data <- data() # might do additional cleaning?
      # store$data <- data_1()[sample(nrow(data_1()), 10000, replace=F), ]
    })
    
    # show data table of raw data
    output$raw_data <- DT::renderDataTable(DT::datatable(store$data, rownames=F, options=list(scrollX=T, scrollY="200px")))
    
    # start variable selection!
    output$next_varselect <- renderUI({
      if (is.null(store$data)) return()
      actionButton(
        session$ns("next_varselect"), 
        "Next: Variable Selection", 
        icon("hand-pointer"), 
        class = "btn-outline-primary"
      )
    })
    
    # variable selection renders once button is clicked
    output$varselect <- renderUI({
      tagList(
        h3("Variable Selection"),
        fluidRow(
          column(
            6,
            selectInput(session$ns("select_outcome"), "Select outcome variable:", choices=c("", colnames(store$data)), multiple=F),
            conditionalPanel(
              "output.outcome_type == 'cat_binary'",
              ns = ns,
              selectInput(session$ns("select_outcome_level"), "Select a level to represent success in the outcome:", choices=NULL, multiple=F)
            )
          ),
          column(
            6,
            selectInput(session$ns("select_treatment"), "Select treatment variable:", choices=c("", colnames(store$data)), multiple=F),
            conditionalPanel(
              "output.treatment_type == 'cat_binary'",
              ns = ns,
              selectInput(session$ns("select_treatment_level"), "Select a level to represent the treated group:", choices=NULL, multiple=F)
            )
          )
        ),
        fluidRow(
          column(
            6,
            pickerInput(session$ns("select_covariates"), "Select covariates:", choices=colnames(store$data), multiple=T, options=list(`actions-box`=T))
          ),
          column(
            6,
            pickerInput(session$ns("select_candidates"), "Select candidate effect modifiers:", choices=colnames(store$data), multiple=T, options=list(`actions-box`=T))
          )
        )
      )
    }) |> bindEvent(input$next_varselect)
    
    # setup outcome level
    output$outcome_type <- reactive({
      req(input$select_outcome)
      is_binary <- check_binary(store$data[[input$select_outcome]]) 
      is_binary_numeric <- check_binary_numeric(store$data[[input$select_outcome]])
      if (is_binary && is_binary_numeric) {
        return("numeric_binary")
      }
      if (is_binary && !is_binary_numeric) {
        return("cat_binary")
      }
      else return("other")
    })
    
    outputOptions(output, "outcome_type", suspendWhenHidden=F)
    
    observeEvent(input$select_outcome, {
      if (check_binary(store$data[[input$select_outcome]])) {
        updateSelectInput(
          inputId = "select_outcome_level",
          choices = c("", paste(input$select_outcome, "=", unique(na.omit(store$data[[input$select_outcome]]))))
        )
      }
    })
    
    # setup treatment level 
    output$treatment_type <- reactive({
      req(input$select_treatment)
      if (check_binary_numeric(store$data[[input$select_treatment]])) {
        return("numeric_binary")
      }
      else return("cat_binary")
    })
    
    outputOptions(output, "treatment_type", suspendWhenHidden=F)
    
    observeEvent(input$select_treatment, {
      if (check_binary(store$data[[input$select_treatment]])) {
        updateSelectInput(
          inputId = "select_treatment_level",
          choices = c("", paste(input$select_treatment, "=", unique(na.omit(store$data[[input$select_treatment]]))))
        )
      }
    })
    
    # verify outcome variable is numeric (continuous or binary)
    observe(feedback_outcome())
    feedback_outcome <- reactive({
      req(input$select_outcome)
      
      shinyFeedback::hideFeedback("select_outcome")
      if (input$select_outcome == input$select_treatment) {
        shinyFeedback::showFeedbackDanger(inputId="select_outcome", text="The treatment and outcome variables must be different.")
      }
      
      current_level <- sub(paste(input$select_outcome, "= "), "", input$select_outcome_level)
      possible_levels <- unique(na.omit(store$data[[input$select_outcome]]))
      
      is_binary <- check_binary(store$data[[input$select_outcome]])
      is_binary_numeric <- check_binary_numeric(store$data[[input$select_outcome]])
      is_continuous <- check_continuous(store$data[[input$select_outcome]])
      level_selected <- current_level %in% possible_levels
      
      status <- dplyr::case_when(
        isTRUE(is_continuous) ~ "pass",
        isTRUE(is_binary) & isTRUE(is_binary_numeric) ~ "pass",
        isTRUE(is_binary) & isFALSE(is_binary_numeric) & isTRUE(level_selected) ~ "pass",
        isTRUE(is_binary) & isFALSE(is_binary_numeric) ~ "warn",
        T ~ "fail"
      )
      
      if (status == "pass") {
        shinyFeedback::showFeedbackSuccess(inputId="select_outcome", text=paste(input$select_outcome, "is a binary variable."))
      }
      
      if (status == "warn") {
        shinyFeedback::showFeedbackWarning(inputId="select_outcome", text=paste("It appears that", input$select_outcome, "is a categorical variable with 2 levels. Please specify which level corresponds to treatment."))
      }
      
      if (status == "fail") {
        shinyFeedback::showFeedbackDanger(inputId="select_outcome", text="Please choose a binary treatment variable.")
      }
      
      shinyFeedback::hideFeedback("select_outcome_level")
      if(level_selected) {
        shinyFeedback::showFeedbackSuccess(inputId="select_outcome_level", text=paste(input$select_outcome, "will be automatically recoded so that", current_level, "= TRUE and", possible_levels[possible_levels != current_level], "= FALSE."))
      }
    })
    
    # verify treatment variable is binary
    observe(feedback_treatment())
    feedback_treatment <- reactive({
      req(input$select_treatment)
      
      shinyFeedback::hideFeedback("select_treatment")
      if (input$select_treatment == input$select_outcome) {
        shinyFeedback::showFeedbackDanger(inputId="select_treatment", text="The treatment and outcome variables must be different.")
      }
      
      current_level <- sub(paste(input$select_treatment, "= "), "", input$select_treatment_level)
      possible_levels <- unique(na.omit(store$data[[input$select_treatment]]))
      
      is_binary <- check_binary(store$data[[input$select_treatment]])
      is_numeric <- check_binary_numeric(store$data[[input$select_treatment]])
      level_selected <- current_level %in% possible_levels
      
      status <- dplyr::case_when(
        isTRUE(is_binary) & isTRUE(is_numeric) ~ "pass",
        isTRUE(is_binary) & isFALSE(is_numeric) & isTRUE(level_selected) ~ "pass",
        isTRUE(is_binary) & isFALSE(is_numeric) ~ "warn",
        T ~ "fail"
      )
      
      if (status == "pass") {
        shinyFeedback::showFeedbackSuccess(inputId="select_treatment", text=paste(input$select_treatment, "is a binary variable."))
      }
      
      if (status == "warn") {
          shinyFeedback::showFeedbackWarning(inputId="select_treatment", text=paste("It appears that", input$select_treatment, "is a categorical variable with 2 levels. Please specify which level corresponds to treatment."))
      }

      if (status == "fail") {
        shinyFeedback::showFeedbackDanger(inputId="select_treatment", text="Please choose a binary treatment variable.")
      }
      
      shinyFeedback::hideFeedback("select_treatment_level")
      if(level_selected) {
        shinyFeedback::showFeedbackSuccess(inputId="select_treatment_level", text=paste(input$select_treatment, "will be automatically recoded so that", current_level, "= TRUE and", possible_levels[possible_levels != current_level], "= FALSE."))
      }
    })
    
    # remove outcome and treatment variable from covariate variable and candidate variables' pickerInput choices 🟢
    observe({
      if(!is.null(input$select_treatment) & !is.null(input$select_outcome))
        updatePickerInput(inputId="select_covariates",
                          choices=colnames(store$data)[!(colnames(store$data) %in% c(input$select_outcome, input$select_treatment))],
                          selected=isolate(input$select_covariates))
        updatePickerInput(inputId="select_candidates",
                          choices=colnames(store$data)[!(colnames(store$data) %in% c(input$select_outcome, input$select_treatment))],
                          selected=isolate(input$select_covariates))
    })
    
    # add to store
    select_variable_listen <- reactive({
      list(
        input$select_outcome,
        input$select_treatment,
        input$select_covariates,
        input$select_candidates
      )
    })
    
    observeEvent(select_variable_listen(), {
      if(!is.null(input$select_outcome) & !is.null(input$select_outcome) & !is.null(input$select_covariates) & !is.null(input$select_candidates)) {
        store$y_outcome <- input$select_outcome
        store$z_treatment <- input$select_treatment
        store$X_covariates <- input$select_covariates
        store$P_candidates <- input$select_candidates
      }
    })
    
    # start variable verification!
    output$next_varverify <- renderUI({
      if (is.null(store$y_outcome) & is.null(store$z_treatment) & is.null(store$X_covariates) & is.null(store$P_candidates)) return()
      actionButton(
        session$ns("next_varverify"), 
        " Next: Covariate Data Type Verification", 
        icon("hand-pointer"), 
        class = "btn-outline-primary"
      )
    })
    
    # variable verification renders once button is clicked
    output$varverify <- renderUI({
      tagList(
        h3("Covariate Data Type Verification")
      )
    }) |> bindEvent(input$next_varverify)
    
    # display table of variables with assignments, data type, and percent null (and add to store)
    observeEvent(input$next_varverify,{
      out <- store$data
      outcome <- store$y_outcome
      treatment <- store$z_treatment
      covariates <- store$X_covariates
      candidates <- store$P_candidates
      
      percent_na <- colMeans(is.na(out)) * 100
      
      check_data_type <- function(x) {
        if (isTRUE(check_binary(x))) return("binary")
        if (isTRUE(check_decimal(x))) return("continuous")
        return("categorical")
      }
      
      tab1 <- tibble(
        "Variable" = names(out),
        "Role" = sapply(names(out), function(x) {
          if (x == outcome) return("outcome")
          if (x == treatment) return("treatment")
          if (x %in% covariates & x %in% candidates) return("covariate, candidate EM")
          if (x %in% covariates) return("covariate")
          if (x %in% candidates) return("candidate EM")
          return("unassigned")
        }),
        "Data Type" = sapply(out, check_data_type), # options: binary, continuous, categorical
        "Percent NA" = percent_na
      )
      
      # add to store
      store$variable_verification <- tab1
    })
    
    output$varverify_table <- DT::renderDataTable(DT::datatable(store$variable_verification, rownames=F, options=list(scrollX=T, scrollY="200px")))
    
    # start effect modifier prevalence!
    output$next_emprevalence <- renderUI({
      if (is.null(store$P_candidates)) return()
      actionButton(
        session$ns("next_emprevalence"),
        " Next: Confirm Effect Modifier Prevalence",
        icon("hand-pointer"),
        class = "btn-outline-primary"
      )
    }) |> bindEvent(input$next_varverify)
    
    # effect modifier prevalence renders once button is clicked
    output$emprevalence <- renderUI({
      tagList(
        h3("Effect Modifier Prevalence in Data")
      )
    }) |> bindEvent(input$next_emprevalence)
    
    # display table of effect modifier prevalence
    observeEvent(input$next_emprevalence, {
      # input setup
      out <- store$data
      outcome <- store$y_outcome
      treatment <- store$z_treatment
      covariates <- store$X_covariates
      candidates <- store$P_candidates
      y <- pull(out, outcome)
      z <- pull(out, treatment)
      X <- select(out, -c(outcome, treatment))
      P <- select(X, all_of(candidates))
      
      # check that candidate effect modifiers are named
      if (sum(is.na(colnames(P))) != 0) {
        colnames(P)[which(is.na(colnames(P)))] <- paste0("em", 1:sum(is.na(colnames(P))))
      }
      
      # get levels of candidate effect modifiers
      P_levels <- P %>%
        map(unique) %>%
        map(na.omit)
      
      P <- as_tibble(P) %>%
        mutate_all(as_factor)
      
      # get names of multilevel categorical variables
      mlP <- names(which(lengths(P_levels) > 2))
      
      # get names of binary character variables
      cP <- P_levels %>%
        map(as.numeric) %>%
        map(is.na) %>%
        map(all) %>%
        as_vector()
      
      bcP <- names(which(cP[!(names(cP) %in% all_of(mlP))]))
      
      # create dummy indicators for mlP and cP
      for (p in c(mlP, bcP)) {
        mlP_form <- as.formula(paste0("~ -1 + ", p))
        P <- P %>%
          select(-all_of(p)) %>%
          bind_cols(as.data.frame(model.matrix(mlP_form, model.frame(~ ., P, na.action=na.pass))))
      }
      
      duplicate_bcP <- P_levels[bcP] %>%
        map_chr(c(2)) %>%
        paste0(bcP, .)
      
      P <- P %>%
        select(-all_of(duplicate_bcP)) %>%
        rename_with(~ gsub(" ", "_", .x))
      
      # create data frame of EM indicators for all 1 and 2-way interactions
      P <- P %>%
        mutate_if(is.factor, ~ as.numeric(.)-1)
      
      counter_P <- P %>%
        mutate_all(~ case_when(.x == 1 ~ 0,
                               .x == 0 ~ 1))
      names(counter_P) <- paste0(names(counter_P), "_0")
      
      full_P <- P %>%
        rename_with(~ paste0(.x, "_1")) %>%
        cbind(., counter_P)
      
      ems <- as.data.frame(model.matrix(~ .^2 - 1, data=full_P))
      
      remove <- c(paste0(names(P), "_0"), paste0(names(P), "_1"),
                  paste0(names(P), "_1:", names(P), "_0"),
                  paste0(names(P), "_0:", names(P), "_1"))
      
      if (length(mlP) != 0) {
        remove <- c(remove, names(full_P)[str_detect(names(full_P), paste(mlP, collapse="|"))])
      }
      
      ems <- ems %>%
        dplyr::select(-any_of(remove)) %>%
        cbind(P, .)
      
      # check positivity: print prevalence of observations for each effect modifier interaction (and size)
      cat("Checking positivity...\n")
      tab1 <- tibble("Effect Modifier (Interaction)" = names(ems),
                     "Prevalence" = paste0(round(apply(ems, 2, mean, na.rm=T)*100, 2), "%"),
                     "In-Count" = apply(ems, 2, sum, na.rm=T),
                     "Out-Count" = apply(ems, 2, function(x) sum(x==0, na.rm=T)))
      
      colnames(tab1)[2] <- paste0("Proportion in data (N=", nrow(ems), ")")
      
      # throw warning if less than 5% or more than 95% prevalence
      if (any(tab1$`In-Count`/nrow(ems) < 0.05) | any(tab1$`Out-Count`/nrow(ems) < 0.05)) {
        prob_em <- tab1 %>% 
          filter(`In-Count`/nrow(ems) < 0.05 | `Out-Count`/nrow(ems) < 0.05) %>%
          pull(`Effect Modifier (Interaction)`)
        warning("The following interactions have less than 5% or more than 95% prevalence. Bootstrap results may be biased:")
        print(prob_em)
      }
      
      store$y <- y
      store$z <- z
      store$X <- X
      store$P <- P
      store$emprevalence_table <- tab1
      store$ems <- ems
    })
    
    output$emprevalence_table <- DT::renderDataTable(DT::datatable(store$emprevalence_table, rownames=F, options=list(scrollX=TRUE, scrollY="200px")))
    
    # move to bootstrap tab! (add warning before moving ahead to bootstrap)
    output$next_bootstrap <- renderUI({
      if (is.null(store$ems)) return()
      actionButton(
        session$ns("next_bootstrap"),
        " Next: Set Bootstrap Parameters",
        icon("hand-pointer"),
        class = "btn-outline-primary"
      )
    })
    
    switch_page <- function() {
      ret_val <- reactiveValues(count=0)
      observeEvent(input$next_bootstrap, ret_val$count <- ret_val$count + 1)
      return(reactive(ret_val$count))
    }
    switch_page() |> bindEvent(input$next_bootstrap)
  })
}