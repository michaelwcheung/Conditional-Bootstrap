# bootstrap parameter module

bootstrap_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Set Bootstrap Parameters", 
    value = ns("tab"),
    tagList(
      div(
        style = "margin-bottom: 50px;",
        h2("Set Bootstrap Parameters"),
        
        radioButtons(
          ns("quantile"),
          "Select quantile:",
          trad_quantiles,
          width = "100%"
        ),
        textInput(
          ns("custom_quantile"), 
          "Or enter a custom quantile (please ensure 0 and 100 are included!):"
        ),
        actionButton(ns("add"), "Add custom quantile"),
        br(),
        br(),
        radioButtons(
          ns("estimand_type"),
          "Select the type of estimand:",
          c(
            "Risk difference" = "rd",
            "Risk ratio" = "rr",
            "Odds ratio" = "or"
          ),
          width = "100%"
        ),
        radioButtons(
          ns("heterogeneity_test"),
          "Select the heterogeneity test:",
          c(
            "Cochran's Q" = "Q",
            "Bootstrap hypothesis difference of means" = "BH",
            "Confidence interval overlap" = "CI",
            "Pre-bootstrap Cochran's Q" = "PQ"
          ),
          width = "100%"
        ),
        radioButtons(
          ns("estimation_method"),
          "Select the estimation method for causal effect:",
          c(
            "Outcome regression (adjusted)" = "lr",
            "IP weighting" = "iptw",
            "Doubly robust (currently does not work with bootstrap hypothesis test)" = "dr"
          ),
          width = "100%"
        ),
        numericInput(
          ns("bootstrap_samples"), 
          "Number of bootstrap samples:", 
          100,
          min = 1
        ),
        uiOutput(ns("bootstrap")),
        uiOutput(ns("bootstrap_progress")),
        uiOutput(ns("next_results"))
      )
    )
  )
}

bootstrap_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # add custom quantile 
    observeEvent(input$add, {
      # check that quantiles includes 0 and 100, if it doesn't throw error
      # if (any(!(c(0, 100) %in% quantiles))) stop("0 and/or 100 are not included in vector of quantiles")
      
      other_val <- req(input$custom_quantile)
      names(other_val) <- paste0("custom quantile: ", input$custom_quantile)
      updated_values <- append(trad_quantiles, other_val)
      updateRadioButtons(session, "quantile", choices=updated_values)
    })
    
    # start bootstrap!
    output$bootstrap <- renderUI({
      if (is.null(store$P_candidates)) return()
      tagList(
        actionButton(
          session$ns("bootstrap"),
          " Start Bootstrap!",
          icon("hand-pointer"),
          class = "btn-success"
        )#,
        # progressBar(
        #   session$ns("bootstrap_progress"),
        #   value = 0,
        #   display_pct = T
        # )
      )
    })
    
    observeEvent(input$bootstrap, {
      start_shiny_boot_sr <- Sys.time()
      y <- store$y
      z <- store$z
      X <- store$X
      P <- store$P
      ems <- store$ems
      quantiles <- store$quantiles <- as.numeric(unlist(strsplit(req(input$quantile), ",")))
      B <- store$B <- req(input$bootstrap_samples)
      estimand <- store$estimand <- req(input$estimand_type)
      test <- store$heterogeneity_test <- req(input$heterogeneity_test)
      estimation_method <- store$estimation_method <- req(input$estimation_method)
      alpha <- 0.05
      
      # reset store
      store$boots <- NULL
      if (test == "BH") store$bh_cates <- NULL
      store$statistics <- NULL
      store$em_rejectT <- NULL
      
      # instantiate bootstrap storage structure
      boots <- tibble(quantile=numeric(),
                      em=character(),
                      cate=numeric(),
                      cate_se=numeric())
      
      # if conducting bootstrap hypothesis test, instantiate storage structure for observed CATEs
      if (test == "BH") {
        bh_cates <- tibble(quantile = numeric(),
                           em = character(),
                           o_cate = numeric(),
                           o_cate_se = numeric())
      }
      
      # set regression model family based on desired estimand
      if (estimand == "or") {
          family <- "binomial"
      } else if (estimand == "rd") {
          family <- "gaussian"
      } else if (estimand == "rr") {
          family <- "poisson"
      } else {}
      
      # estimate propensity score if estimation method is IPTW or DR
      if (estimation_method == "iptw" | estimation_method == "dr") {
          propensity <- ipweights(store$data, X, z)
      } else if (estimation_method == "lr") {
          propensity <- rep(1, length(z))
      } else {}
      
      withProgress(message = "Performing conditional bootstrap\n", value = 0, {
        incProgress(0, detail = HTML(paste("Computing ATE.")))
        # compute full data ATE
        ate_estimate <- estimate_ate(store$data, y, z, X, estimand, family, estimation_method, propensity)
        ate <- store$ate <- ate_estimate[1]
        ate_confint <- store$ate_confint <- ate_estimate[2:3]
        
        # initialize counters and time storage vectors
        init <- numeric()
        end <- numeric()
        em_counter <- 0
        b_counter <- 0
        n_ems <- ncol(ems)
        n_quantiles <- length(quantiles)
        n_iter <- n_ems * n_quantiles * B
        
        # if performing Cochran's Q test before bootstrapping, create list of effect modifiers
        if (test == "PQ") em_rejectT <- character(0)
        
        # conditional bootstrap
        for (em in names(ems)) {
          em_counter <- em_counter + 1 # counter for em
          q_counter <- 0 # counter for quantile
          adj_X <- generate_adj_X(em, X, P) # create matrix of confounders and effect modifiers that don't belong to current em
          
          # estimate observed CATEs if conducting bootstrap hypothesis test or prior Cochran's Q test
          if (test == "BH" | test == "PQ") {
            
            mod_cate0 <- glm(y ~ z + as.matrix(adj_X), family=family, subset=which(get(em, ems) == 0))
            mod_cate100 <- glm(y ~ z + as.matrix(adj_X), family=family, subset=which(get(em, ems) == 1))
            
            if (test == "BH") {
              
              bh_cates <- bh_cates %>% 
                add_row(quantile = c(0,100),
                        em = em,
                        o_cate = c(mod_cate0$coefficients["z"], mod_cate100$coefficients["z"]),
                        o_cate_se = c(summary(mod_cate0)$coefficients["z", "Std. Error"],
                                      summary(mod_cate100)$coefficients["z", "Std. Error"]))
            } else if (test == "PQ") {
              
              PQ_rejectT <- cochrans_q_het(ate = ate,
                                           cate0 = mod_cate0$coefficients["z"],
                                           cate100 = mod_cate100$coefficients["z"],
                                           se0 = summary(mod_cate0)$coefficients["z", "Std. Error"],
                                           se100 = summary(mod_cate100)$coefficients["z", "Std. Error"],
                                           estimand = estimand)[2] < alpha
              
              if (PQ_rejectT) {
                em_rejectT <- c(em_rejectT, em)
              } else {
                next
              }
            }    
          }
          
          for (q in quantiles) {
            q_counter <- q_counter + 1
            
            for (b in 1:B) {
              b_counter <- b_counter + 1
              
              # time before bootstrap
              init <- c(init, Sys.time()) 
              
              # bootstrap CATE data according to quantiles
              inem <- which(get(em, ems) == 1)
              outem <- which(get(em, ems) == 0)
              ind_inem <- sample(inem, size=round(nrow(P)*q/100), replace=T)
              ind_outem <- sample(outem, size=round(nrow(P)*(100-q)/100), replace=T)
              cate_ind <- c(ind_inem, ind_outem)
              
              # create outcome model and add estimated CATE (and SE) to bootstrap storage structure
              # mod <- glm(y ~ z + as.matrix(adj_X), family=family, subset=cate_ind)
              cate_estimate <- estimate_cate(store$data, y, z, adj_X, cate_ind, estimand, family, estimation_method, propensity)
              
              boots <- boots %>% add_row(quantile = q,
                                         em = em, 
                                         cate = cate_estimate[1],
                                         cate_se = cate_estimate[2])
              
              # time after bootstrap
              end <- c(end, Sys.time())
              
              # estimated remaining time
              time <- round(seconds_to_period(sum(end - init)), 0)
              est <- n_iter*(mean(end[end != 0] - init[init != 0])) - time
              remaining <- round(seconds_to_period(est), 0)
              
              percent <- b_counter/n_iter * 100
              
              cat(paste0(sprintf('\r[%-50s] %d%%',
                                 paste(rep('=', percent / 2), collapse = ''),
                                 floor(percent)),
                         " | Effect Modifier:", em_counter, "/", n_ems,
                         " | Quantile:", q_counter, "/", n_quantiles,
                         " | Bootstrap: ", b, "/", B, "     "))
              
              incProgress(1/n_iter, detail=HTML(paste(
                floor(percent), "%",
                " | Execution Time: ", time,
                " | Estimated Time Remaining: ", remaining,
                " | Effect Modifier: ", em_counter, "/", n_ems,
                " | Quantile: ", q_counter, "/", n_quantiles,
                " | Bootstrap: ", b, "/", B
              )))
            }
          }
        }
      })
      end_shiny_boot_sr <- Sys.time()
      print(end_shiny_boot_sr - start_shiny_boot_sr)
      
      # calculate means/medians, bootstrap confidence intervals
      if (estimand == "or" | estimand == "rr") {
        statistics <- boots %>%
          group_by(quantile, em) %>%
          summarise(mean_cate = mean(exp(cate)),
                    median_cate = median(exp(cate)),
                    cate_lp_se = bse(cate),
                    cate_ci_lwr = quantile(exp(cate), probs = 0.025),
                    cate_ci_upr = quantile(exp(cate), probs = 0.975))
      } else if (estimand == "rd") {
        statistics <- boots %>%
          group_by(quantile, em) %>%
          summarise(mean_cate = mean(cate),
                    median_cate = median(cate),
                    cate_lp_se = bse(cate),
                    cate_ci_lwr = quantile(cate, probs = 0.025),
                    cate_ci_upr = quantile(cate, probs = 0.975))
      } else {}
      
      # make CATEs solid colors on plot if they are outside the ATE 95% CI
      statistics <- statistics %>%
        mutate(cate_visible = case_when(median_cate > ate_confint[2] ~ T,
                                        median_cate < ate_confint[1] ~ T,
                                        T ~ F))
      
      # conduct heterogeneity test
      ## check for overlapping of CIs
      if (test == "CI") {
        
        ### candidates reject null hypothesis of homogeneity if at least one of the 0 or 100 quantile CIs do not overlap with the ATE CI
        em_rejectT <- statistics %>%
          filter(quantile == 0 | quantile == 100) %>%
          mutate(cate_ci_rejectT = case_when(cate_ci_lwr > ate_confint[2] ~ T,
                                             cate_ci_upr < ate_confint[1] ~ T,
                                             T ~ F)) %>%
          filter(cate_ci_rejectT == T) %>%
          pull(em) %>%
          unique()
        
        # conduct Cochran's Q test
      } else if (test == "Q") {
        
        em_rejectT <- statistics %>% 
          group_by(em) %>%
          filter(quantile == min(quantiles) | quantile == max(quantiles)) %>%
          arrange(quantile) %>%
          summarise(Q_rejectT = cochrans_q_het(ate = ate,
                                               cate0 = first(mean_cate),
                                               cate100 = last(mean_cate),
                                               se0 = first(cate_lp_se),
                                               se100 = last(cate_lp_se),
                                               estimand = estimand)[2] < alpha) %>%
          filter(Q_rejectT == T) %>%
          pull(em)
        
        # conduct bootstrap hypothesis one-sample test
      } else if (test == "BH") {
        
        bh_cates <- bh_cates %>%
          mutate(t = case_when(family == "gaussian" ~ (o_cate-ate)/o_cate_se,
                               family == "binomial" ~ (o_cate-log(ate))/o_cate_se))
        
        em_rejectT <- boots %>%
          left_join(., bh_cates) %>%
          filter(quantile == 0 | quantile == 100) %>%
          mutate(boot_t = (cate-o_cate)/cate_se) %>%
          group_by(quantile, em) %>%
          summarise(bh_reject_pval = mean(abs(boot_t) > abs(t))) %>%
          mutate(bh_rejectT = bh_reject_pval < alpha) %>%
          filter(bh_rejectT == T) %>%
          pull(em) %>%
          unique()
        
      } else {}
      
      # add everything to store
      store$boots <- boots
      if (test == "BH") store$bh_cates <- bh_cates
      store$statistics <- statistics
      store$em_rejectT <- em_rejectT
    })
    
    # output$bootstrap_progress <- renderUI({
    #   progressBar(
    #     session$ns("bootstrap_progress"),
    #     value = 0,
    #     display_pct = T
    #   )
    # }) |> bindEvent(input$bootstrap)
    
    # move to results tab!
    output$next_results <- renderUI({
      if (is.null(store$statistics)) return()
      actionButton(
        session$ns("next_results"),
        " Next: See Results",
        icon("hand-pointer"),
        class = "btn-outline-primary"
      )
    })
    
    switch_page <- function() {
      ret_val <- reactiveValues(count=0)
      observeEvent(input$next_results, ret_val$count <- ret_val$count + 1)
      return(reactive(ret_val$count))
    }
    switch_page() |> bindEvent(input$next_results)
  })
}