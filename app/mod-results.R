# results module

results_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Results",
    value = ns("tab"),
    tagList(
      div(
        style = "margin-bottom: 50px;",
        h2("Results"),
        uiOutput(ns("plot_panel")),
        uiOutput(ns("em_quantile")),
        DT::dataTableOutput(ns("em_quantile_table")),
        uiOutput(ns("download_em_quantile_table")),
        br(),
        uiOutput(ns("regression_coefficients")),
        DT::dataTableOutput(ns("regression_coefficients_table")),
        uiOutput(ns("download_regression_coefficients_table")),
        br()
      )
    )
  )
}


results_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    compute_plot_results <- reactive({
      statistics <- store$statistics
      em_rejectT <- store$em_rejectT
      ate <- store$ate
      ate_confint <- store$ate_confint
      quantiles <- store$quantiles
      
      store$plot_statistics <- NULL
      store$em_colors <- NULL
      store$cate_edges <- NULL
      store$em_quantile_table <- NULL
      store$regression_coefficients_table <- NULL
      
      if (length(em_rejectT) > 0) {
        # filter out effect modifiers that failed to reject null of homogeneity
        plot_statistics <- statistics %>%
          filter(em %in% em_rejectT) %>%
          mutate(color = em,
                 alpha = ifelse(cate_visible == F, 0.3, 1.0))
        
        # create line segments on plot
        cate_edges <- plot_statistics %>%
          group_by(em) %>%
          arrange(quantile) %>%
          reframe(x1 = lag(quantile),
                  x2 = quantile,
                  y1 = lag(median_cate),
                  y2 = median_cate,
                  color = em,
                  alpha = case_when(lag(cate_visible) + cate_visible == 2 ~ 1.0,
                                    lag(cate_visible) + cate_visible == 1 ~ 0.5,
                                    T ~ 0.3),
                  linetype = case_when(lag(cate_visible) + cate_visible == 2 ~ "solid",
                                       lag(cate_visible) + cate_visible == 1 ~ "dashed",
                                       T ~ "dotted")) %>%
          filter(!is.na(x1))
        
        # ranking of CATEs by effect modifier and quantile
        em_quantile_table <- statistics %>%
          filter(em %in% em_rejectT) %>%
          select(quantile, em, median_cate, mean_cate, cate_ci_lwr, cate_ci_upr) %>%
          arrange(desc(abs(median_cate))) %>%
          mutate(across(c(median_cate, mean_cate, cate_ci_lwr, cate_ci_upr), \(x) round(x, 3))) %>%
          dplyr::rename(
            "Quantile" = quantile,
            "Effect Modifier" = em,
            "Median CATE" = median_cate,
            "Mean CATE" = mean_cate,
            "Lower 95% CI" = cate_ci_lwr,
            "Upper 95% CI" = cate_ci_upr
          )
        
        em_quantile_table["Median Sorter"] <- abs(em_quantile_table["Median CATE"])
        em_quantile_table["Mean Sorter"] <- abs(em_quantile_table["Mean CATE"])
        em_quantile_table["Lower 95% CI Sorter"] <- abs(em_quantile_table["Lower 95% CI"])
        em_quantile_table["Upper 95% CI Sorter"] <- abs(em_quantile_table["Upper 95% CI"])
        
        # table of regression coefficients
        regression_coefficients_table <- statistics %>%
          filter(em %in% em_rejectT) %>%
          group_by(em) %>%
          group_modify(~ broom::tidy(lm(median_cate ~ quantile, data = .x))) %>%
          filter(term != "(Intercept)") %>%
          select(em, estimate, p.value) %>%
          arrange(desc(abs(estimate))) %>%
          mutate(estimate = round(estimate, 5),
                 p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))) %>%
          dplyr::rename(
            "Effect Modifier" = em,
            "Coefficient Estimate" = estimate,
            "P-value" = p.value
          )
        
        regression_coefficients_table["Coefficient Sorter"] <- abs(regression_coefficients_table["Coefficient Estimate"])
        
        store$plot_statistics <- plot_statistics
        store$cate_edges <- cate_edges
        store$em_quantile_table <- em_quantile_table
        store$regression_coefficients_table <- regression_coefficients_table
      }
    })
    
    base_plot <- reactive({
      store$plot <- NULL
      store$y_min <- NULL
      store$y_max <- NULL
      
      y_min <- min(c(store$plot_statistics$median_cate, store$ate_confint[1]))
      y_max <- max(c(store$plot_statistics$median_cate, store$ate_confint[2]))
      plot <- ggplot() +
        scale_x_continuous(breaks = store$quantiles, limits = c(min(store$quantiles), max(store$quantiles))) +
        scale_y_continuous(limits = c(y_min, y_max)) + 
        theme_bw() + 
        labs(
          x="Quantile",
          title="Resampled CATE Estimates by Quantile",
          color="Effect Modifier"
        )
      
      if (store$estimand == "or") {
        plot <- plot +
          ylab("Median CATE (odds ratio)")
      } else if (store$estimand == "rd") {
        plot <- plot +
          ylab("Median CATE (risk difference)")
      } else if (store$estimand == "rr") {
        plot <- plot +
          ylab("Median CATE (risk ratio)")
      } else {}
      
      store$plot <- plot
      store$y_min <- y_min
      store$y_max <- y_max
    })
    
    output$plot_panel <- renderUI({
      if (is.null(store$statistics)) return()
      compute_plot_results()
      sidebarLayout(
        sidebarPanel(
          uiOutput(session$ns("plot_options"))
        ),
        mainPanel(
          plotlyOutput(session$ns("plot"))
        )
      )
    })
    
    # create sidebar
    output$plot_options <- renderUI({
      if (is.null(store$statistics)) return()
      tagList(
        p(paste0(sprintf("Observed ATE: %s with 95%% CI (%s, %s)", round(store$ate, 5), round(store$ate_confint[1], 5), round(store$ate_confint[2], 5)))),
        checkboxInput(session$ns("show_ate"), "Show ATE on plot", value = TRUE),
        pickerInput(session$ns("select_ems"), "Effect Modifiers: ", choices = unique(store$plot_statistics$em), selected = unique(store$plot_statistics$em), multiple = TRUE, options = list(`actions-box` = TRUE)),
        hr(),
        HTML("Linetype Key:"),
        tags$ul(
          tags$li("Solid: Significant"),
          tags$li("Dashed: Between significance threshold"),
          tags$li("Dotted: Insignificant")
        )
      )
    })
    
    # add to remainder of plot
    output$plot <- renderPlotly({
      if (is.null(store$statistics)) return()
      req(!is.null(input$show_ate))
      # req(!is.null(input$select_ems))
      
      base_plot()
      p <- store$plot
      
      if(input$show_ate) {
        p <- p + 
          geom_rect(aes(
            xmin = min(store$quantiles), xmax = max(store$quantiles),
            ymin = store$ate_confint[1], ymax = store$ate_confint[2],
            text = sprintf("ATE upper 95%% CI: %s\nATE lower 95%% CI: %s", round(store$ate_confint[2], 5), round(store$ate_confint[1], 5))
          ), color = "grey", alpha = 0.2) +
          geom_text(aes(
            x = -5,
            y = store$ate,
            label = "ATE",
            text = sprintf("ATE: %s", round(store$ate, 5))
          )) +
          geom_segment(aes(
            x = 0, xend = 100,
            y = store$ate, yend = store$ate,
            text = sprintf("ATE: %s", round(store$ate, 5))
          )) 
      }
      
      if(length(input$select_ems) > 0) {
        cls <- rep(c(brewer.pal(8,"Dark2"), 
                     brewer.pal(10, "Paired"), 
                     brewer.pal(12, "Set3"), 
                     brewer.pal(8,"Set2"), 
                     brewer.pal(9, "Set1"), 
                     brewer.pal(8, "Accent"),  
                     brewer.pal(9, "Pastel1"),  
                     brewer.pal(8, "Pastel2")),4)
        cls_names <- unique(store$plot_statistics$em)
        em_cols = cls[1:length(cls_names)]
        names(em_cols) = cls_names
        
        selected_plot_statistics <- store$plot_statistics %>%
          filter(em %in% input$select_ems)
        selected_cate_edges <- store$cate_edges %>%
          filter(em %in% input$select_ems)
        
        p <- p + 
          geom_point(data=selected_plot_statistics,
                     aes(x=quantile, y=median_cate, group=em,
                         color=color, alpha=I(alpha),
                         text = sprintf("Quantile: %s \nMedian CATE: %s \nEffect Modifier: %s", quantile, round(median_cate, 5), em))) +
          geom_segment(data=selected_cate_edges,
                       aes(x=x1, xend=x2, y=y1, yend=y2,
                           color=color, alpha=alpha,
                           linetype=linetype, group=em)) +
          scale_color_manual(values = em_cols) + 
          scale_alpha(guide = "none") +
          scale_linetype_identity(guide = "none")
      }
      p <- ggplotly(p, tooltip = "text")

      # remove the redundant legend items
      if (length(input$select_ems) > 0){
        unique_em <- length(input$select_ems)

        for (i in 1:unique_em) {
          i <- if(input$show_ate) i + 3 else i
          curr_em <- strsplit(p$x$data[[i]]$name, ",")[[1]][1]
          for (j in seq(if(input$show_ate) unique_em + 4 else unique_em + 1, length(p$x$data))) {
            loop_em <- strsplit(p$x$data[[j]]$name, ",")[[1]][1]
            if (curr_em == loop_em) {
              p$x$data[[j]]$legendgroup <- p$x$data[[i]]$legendgroup
              p$x$data[[j]]$showlegend <- FALSE
            }
          }
          p$x$data[[i]]$name <- strsplit(curr_em, "\\(")[[1]][2]
        }
      }

      p
    })
    
    # show em_quantile_table
    output$em_quantile <- renderUI({
      if (is.null(store$statistics)) return()
      else if (length(store$em_rejectT) == 0) {
        tagList(
          p("None of the effect modifiers were statistically sigificant!")
        )
      }
      else {
        tagList(
          h3("Resampled CATEs by Quantile and Subgroup")
        )
      }
    })
    
    output$em_quantile_table <- DT::renderDataTable(DT::datatable(
      store$em_quantile_table, 
      rownames = F, 
      options = list(
        columnDefs = list(
          list(orderData = 6, targets = 2),
          list(orderData = 7, targets = 3),
          list(orderData = 8, targets = 4),
          list(orderData = 9, targets = 5),
          list(visible = FALSE, targets = c(6, 7, 8, 9))
        ),
        scrollX = T, 
        scrollY = "200px"
      )
    ))
    
    # download em_quantile_table
    output$download_em_quantile_table <- renderUI({
      if (length(store$em_rejectT) == 0) return()
      downloadButton(ns("download_em_quantile_table_button"), "Download")
      downloadHandler(
        filename = function() {"em_quantile_table.csv"},
        content = function(fname) {
          write.csv(store$em_quantile_table, fname)
        }
      )
    })
    
    # show regression_coefficients_table
    output$regression_coefficients <- renderUI({
      if (is.null(store$regression_coefficients_table)) return()
      tagList(
        h3("Linear Regression Coefficients of CATEs on Quantile by Effect Modifier")
      )
    })
    
    output$regression_coefficients_table <- DT::renderDataTable(DT::datatable(
      store$regression_coefficients_table, 
      rownames = F, 
      options=list(
        columnDefs = list(
          list(orderData = 3, targets = 1),
          list(visible = FALSE, targets = 3)
        ),
        scrollX=T, 
        scrollY="200px",
        rowCallback = JS(
         "function (row, data, displayNum, displayIndex, dataIndex) {",
         "var row_name = 'This value means that a one unit increase in the prevalence of ' + data[0] + ' changes the CATE by ' + data[1] + '.';",
         "$(row).find('td').attr('title', row_name);",
         "}"
        )
      )
    ))
    
    # download regression_coefficients_table
    output$download_regression_coefficients_table <- renderUI({
      if (length(store$em_rejectT) == 0) return()
      downloadButton(ns("download_regression_coefficients_table_button"), "Download")
      downloadHandler(
        filename = function() {"regression_coefficients_table.csv"},
        content = function(fname) {
          write.csv(store$regression_coefficients_table, fname)
        }
      )
    })
  })
}