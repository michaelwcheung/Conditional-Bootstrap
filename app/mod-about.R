# landing page/about module

about_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "About",
    value = ns("tab"),
    tagList(
      div(
        style = "margin-bottom: 50px;",
        h2("Transparent High-dimensional Reproducible Inference through Bootstrap Validation and Exploration (THRIVE)"),
        p("THRIVE is a method to identify effect modifiers by bootstrapping conditional average treatement effects (CATEs) over different quantile levels of candidate effect modifiers."),
        h3("How to use the Shiny App"),
        tags$ul(
          tags$li("In the 'Upload Data' tab, upload your dataset and select variables for analysis."),
          tags$li("In the 'Set parameters' tab, set your parameters for the bootstrap process."),
          tags$li("In the 'Bootstrap results' tab, view and download plots and tables about your effect modifiers by quantiles.")
        ),
        p("Please submit any feedback or questions to ", a(href="https://forms.gle/5e2kCDpNN2VvVkSd7", "this form", .noWS = "outside"), ". Thank you!"),
        actionButton(ns("start"),
                     " Start Analyzing!",
                     icon("hand-pointer"),
                     class = "btn-primary")
      )
    )
  )
}

about_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    switch_page <- function() {
      ret_val <- reactiveValues(count = 0)
      observeEvent(input$start, ret_val$count <- ret_val$count + 1)
      return(reactive(ret_val$count))
    }
    switch_page() |> bindEvent(input$start)
  })
}