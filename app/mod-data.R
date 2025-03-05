# upload data module 

data_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Upload Data",
    value = ns("tab"),
    tagList(
      useShinyFeedback(),
      useShinyjs(),
      div(
        style = "margin-bottom: 50px;",
        h2("Upload Data"),
        checkboxInput(ns("upload_sample"), "Load sample data"),
        fileInput(ns("upload"), NULL, buttonLabel = "Browse", accept = ".csv"),
        DT::dataTableOutput(ns("raw_data")),
        uiOutput(ns("next_varselect")),
        uiOutput(ns("varselect")), # add select number of interactions
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
    
    # SAMPLE DATA OR UPLOAD DATA FRAMEWORK
    listen_data <- reactive({
      list(input$upload_sample, input$upload)
    })
    
    rv <- reactiveValues(
      data_sample = FALSE,
      data_upload = FALSE
    )
    
    
    observeEvent(listen_data(), {
      toy_dataset <- "https://raw.githubusercontent.com/michaelwcheung/Conditional-Bootstrap/refs/heads/main/test/data_droughts_malnutrition_10000.csv"
      
      store$data <- NULL
      store$variable_verification <- NULL
      if (input$upload_sample & !rv$data_upload) { # previously no uploaded data, now click sample
        store$data <- read_csv(toy_dataset)
        rv$data_sample <- TRUE
      }
      if (!rv$data_sample & !is.null(input$upload)) { # previously no sample data, now upload data
        ext <- tools::file_ext(input$upload$name)
        store$data <- switch(ext,
                             csv = vroom::vroom(input$upload$datapath, delim = ","),
                             validate("Invalid file. Please upload a csv file."))
        rv$data_upload <- TRUE
      }
      if (rv$data_sample & !is.null(input$upload)) { # previously sample data, now upload data
        ext <- tools::file_ext(input$upload$name)
        store$data <- switch(ext,
                             csv = vroom::vroom(input$upload$datapath, delim = ","),
                             validate("Invalid file. Please upload a csv file."))
        updateCheckboxInput(inputId = "upload_sample", value = F)
        rv$data_sample <- FALSE
        rv$data_upload <- TRUE
      }
      if (input$upload_sample & rv$data_upload) { # previously uploaded data, now click sample
        store$data <- read_csv(toy_dataset)
        shinyjs::reset("upload")
        rv$data_sample <- TRUE
        rv$data_upload <- FALSE
      }
    })
    
    # show data table of raw data
    output$raw_data <- DT::renderDataTable({
      if (is.null(store$data)) return() 
      DT::datatable(store$data, rownames=F, options=list(scrollX=T, scrollY="200px"))
    })
    
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
    
    # reset all store when variable selection starts
    listen_next_varselect <- reactive({input$next_varselect})
    
    observeEvent(listen_next_varselect(), {
      store$y_outcome <- NULL
      store$z_treatment <- NULL
      store$X_covariates <- NULL
      store$P_candidates <- NULL
      store$variable_verification <- NULL
    })
    
    # variable selection renders once button is clicked
    output$varselect <- renderUI({
      tagList(
        h3("Variable Selection"),
        fluidRow(
          column(
            4,
            selectInput(session$ns("select_outcome"), "Select outcome variable:", choices=c("", colnames(store$data)), multiple=F),
            conditionalPanel(
              "output.outcome_type == 'cat_binary'",
              ns = ns,
              selectInput(session$ns("select_outcome_level"), "Select a level to represent success in the outcome:", choices=NULL, multiple=F)
            )
          ),
          column(
            4,
            selectInput(session$ns("select_treatment"), "Select treatment variable:", choices=c("", colnames(store$data)), multiple=F),
            conditionalPanel(
              "output.treatment_type == 'cat_binary'",
              ns = ns,
              selectInput(session$ns("select_treatment_level"), "Select a level to represent the treated group:", choices=NULL, multiple=F)
            )
          ),
          column(4)
        ),
        fluidRow(
          column(
            4,
            pickerInput(session$ns("select_covariates"), "Select covariates:", choices=colnames(store$data), multiple=T, options=list(`actions-box`=T))
          ),
          column(
            4,
            pickerInput(session$ns("select_candidates"), "Select candidate variables:", choices=colnames(store$data), multiple=T, options=list(`actions-box`=T)),
          ),
          column(
            4, 
            sliderInput(session$ns("select_interaction_dim"), "Select the size of the cross-classifications you would like to test:", value=2, min=1, max=5)
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
        # store$variable_verification <- NULL
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
        # store$variable_verification <- NULL
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
        # store$variable_verification <- NULL
    })
    
    # add to store
    select_variable_listen <- reactive({
      list(
        input$select_outcome,
        input$select_treatment,
        input$select_covariates,
        input$select_candidates,
        input$select_interaction_dim
      )
    })
    
    observeEvent(select_variable_listen(), {
      if(!is.null(input$select_outcome) & !is.null(input$select_outcome) & !is.null(input$select_covariates) & !is.null(input$select_candidates)) {
        store$y_outcome <- input$select_outcome
        store$z_treatment <- input$select_treatment
        store$X_covariates <- input$select_covariates
        store$P_candidates <- input$select_candidates
        store$interaction_dim <- input$select_interaction_dim
        store$variable_verification <- NULL
        store$ems <- NULL
      }
    })
    
    # start variable verification!
    output$next_varverify <- renderUI({
      if (is.null(store$y_outcome) & is.null(store$z_treatment) & is.null(store$X_covariates) & is.null(store$P_candidates)) return()
      actionButton(
        session$ns("next_varverify"), 
        " Next: Variable Verification", 
        icon("hand-pointer"), 
        class = "btn-outline-primary"
      )
    })
    
    listen_next_varverify <- reactive({input$next_varverify})
    
    # add table to store
    observeEvent(listen_next_varverify(),{
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
          if (x %in% covariates & x %in% candidates) return("covariate, candidate variable")
          if (x %in% covariates) return("covariate")
          if (x %in% candidates) return("candidate variable")
          return("unassigned")
        }),
        "Data Type" = sapply(out, check_data_type), # options: binary, continuous, categorical
        "Percent NA" = percent_na
      )
      
      # add to store
      store$variable_verification <- tab1
    })
    
    # variable verification renders once button is clicked
    output$varverify <- renderUI({
      if (is.null(store$variable_verification)) return()
      tagList(
        h3("Variable Verification")
      )
    })
    
    # display table 
    output$varverify_table <- DT::renderDataTable({
      if (is.null(store$variable_verification)) return()
      DT::datatable(store$variable_verification, rownames=F, options=list(scrollX=T, scrollY="200px"))
    })
    
    # start effect modifier prevalence!
    output$next_emprevalence <- renderUI({
      if (is.null(store$variable_verification)) return()
      actionButton(
        session$ns("next_emprevalence"),
        " Next: Confirm Potential Effect Modifier Prevalence",
        icon("hand-pointer"),
        class = "btn-outline-primary"
      )
    })
    
    # effect modifier prevalence renders once button is clicked
    output$emprevalence <- renderUI({
      tagList(
        h3("Potential Effect Modifier Prevalence in Data")
      )
    }) |> bindEvent(emprevalence_show(input$next_varverify, input$next_emprevalence))
    
    # hack to remove em prevalence when inputs change
    emprevalence_show <- function(prevbutton, nextbutton) {
      if (is.null(nextbutton)) return()
      if (any(is.null(c(prevbutton, nextbutton)))) return()
      if (any(c(prevbutton, nextbutton) == 0)) return()
      if (prevbutton < nextbutton) return()
      return(1)
    }
    
    # display table of effect modifier prevalence
    observeEvent(input$next_emprevalence, {
      # input setup
      out <- store$data
      outcome <- store$y_outcome
      treatment <- store$z_treatment
      covariates <- store$X_covariates
      candidates <- store$P_candidates
      interaction_dim <- store$interaction_dim
      y <- pull(out, outcome)
      z <- pull(out, treatment)
      X <- select(out, -c(outcome, treatment))
      P <- select(X, all_of(candidates))
      
      withProgress(message = "Computing Potential Effect Modifier Prevalence", value = 0, {
        # check that candidate effect modifiers are named
        if (sum(is.na(colnames(P))) != 0) {
          colnames(P)[which(is.na(colnames(P)))] <- paste0("em", 1:sum(is.na(colnames(P))))
        }
        
        incProgress(0, detail = HTML(paste("Creating effect modifier matrix.")))
        
        # get levels of candidate effect modifiers
        P_levels <- P %>%
          map(unique) %>%
          map(na.omit)
        
        P <- as_tibble(P) %>%
          mutate_all(as_factor)
        
        incProgress(0.25, detail = HTML(paste("Creating effect modifier matrix.")))
        
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
        
        incProgress(0.25, detail = HTML(paste("Creating effect modifier matrix.")))
        
        # create data frame of EM indicators for all 1 and 2-way interactions
        P <- P %>%
          mutate_if(is.factor, ~ as.numeric(.)-1)
        
        counter_P <- P %>%
          mutate_all(~ case_when(.x == 1 ~ 0,
                                 .x == 0 ~ 1))
        names(counter_P) <- paste0(names(counter_P), "_0")
        
        full_P <- P %>%
          rename_with(~ paste0(.x, "_1")) %>%
          cbind(., counter_P) # cumulatively, 1 way interaction (just the variable); based on binary/categorical you know the total number of ems; over generates an interaction that is the same
        
        if (interaction_dim == 1) {
          ems_formula <- formula("~ . - 1")
        }
        else {
          ems_formula <- formula(paste0("~ .^", interaction_dim, " - 1"))
        }
        ems <- as.data.frame(model.matrix(ems_formula, data=full_P)) # change the '2' to a variable (1, 2, 3, 4, 5)
        
        remove <- c(paste0(names(P), "_0"), paste0(names(P), "_1"),
                    paste0(names(P), "_1:", names(P), "_0"),
                    paste0(names(P), "_0:", names(P), "_1")) 
        
        foo <- strsplit(colnames(ems), ':')
        remove <- c()
        for (x in foo) {
          if (n_distinct(str_replace_all(x, '_[:digit:]$', '')) < length(x)) {
            remove <- c(remove, paste0(x, collapse=':'))
          }
        }
        remove <- c(paste0(names(P), "_0"), paste0(names(P), "_1"), remove)
        
        if (length(mlP) != 0) {
          remove <- c(remove, names(full_P)[str_detect(names(full_P), paste(mlP, collapse="|"))])
        }
        
        ems <- ems %>%
          dplyr::select(-any_of(remove)) %>%
          cbind(P, .)
        
        # check positivity: print prevalence of observations for each effect modifier interaction (and size)
        incProgress(0.25, detail = HTML(paste("Checking positivity.")))
        tab1 <- tibble("Potential Effect Modifier (Cross-Classification)" = names(ems),
                       "Prevalence" = paste0(round(apply(ems, 2, mean, na.rm=T)*100, 2), "%"),
                       "In-Count" = apply(ems, 2, sum, na.rm=T),
                       "Out-Count" = apply(ems, 2, function(x) sum(x==0, na.rm=T)),
                       "Sorter" = apply(ems, 2, mean, na.rm=T))
        
        colnames(tab1)[2] <- paste0("Proportion in data (N=", nrow(ems), ")")
        
        # throw warning if less than 5% or more than 95% prevalence
        if (any(tab1$`In-Count`/nrow(ems) < 0.05) | any(tab1$`Out-Count`/nrow(ems) < 0.05)) {
          prob_em <- tab1 %>% 
            filter(`In-Count`/nrow(ems) < 0.05 | `Out-Count`/nrow(ems) < 0.05) %>%
            pull(`Effect Modifier (Interaction)`)
          warning("The following cross-classifications have less than 5% or more than 95% prevalence. Resampling results may be biased:")
          print(prob_em)
        }
        incProgress(0.25, detail = HTML(paste("Complete.")))
      })
      
      store$y <- y
      store$z <- z
      store$X <- X
      store$P <- P
      store$emprevalence_table <- tab1
      store$ems <- ems
    })
    
    output$emprevalence_table <- DT::renderDataTable(DT::datatable(
      store$emprevalence_table, 
      rownames = F, 
      options = list(
        columnDefs = list(
          list(orderData = 4, targets = 1),
          list(visible = FALSE, targets = 4)
        ),
        scrollX = TRUE, 
        scrollY="200px"
      )
    ) %>% DT::formatStyle(
      columns = 1:ncol(store$emprevalence_table),
      target = "cell",
      color = JS("\"unset\""),
      backgroundColor = JS("\"unset\"")
    ) %>% DT::formatStyle(
      2,
      valueColumns = 5,
      target = "row",
      backgroundColor = styleInterval(c(0.05, 0.95), c("#f8d4d5", "white", "#f8d4d5"))
    )) |> bindEvent(emprevalence_show(input$next_varverify, input$next_emprevalence))
    
    # move to bootstrap tab! (add warning before moving ahead to bootstrap)
    output$next_bootstrap <- renderUI({
      if (is.null(store$ems)) return()
      if ((any(store$emprevalence_table$`In-Count`/nrow(store$ems) < 0.05) | any(store$emprevalence_table$`Out-Count`/nrow(store$ems) < 0.05))) {
        return(tagList(
          br(),
          HTML("The interactions in <span style=\"background-color: #f8d4d5;\">red</span> have less than 5% or more than 95% prevalence. Resampled results may be biased."),
          br(),
          br(),
          actionButton(
            session$ns("next_bootstrap"),
            " Next: Set Hyper Parameters",
            icon("hand-pointer"),
            class = "btn-outline-primary"
          )
        ))
      }
      actionButton(
        session$ns("next_bootstrap"),
        " Next: Set Hyper Parameters",
        icon("hand-pointer"),
        class = "btn-outline-primary"
      )
    }) |> bindEvent(emprevalence_show(input$next_varverify, input$next_emprevalence))
    
    switch_page <- function() {
      ret_val <- reactiveValues(count=0)
      observeEvent(input$next_bootstrap, ret_val$count <- ret_val$count + 1)
      return(reactive(ret_val$count))
    }
    switch_page() |> bindEvent(input$next_bootstrap)
  })
}