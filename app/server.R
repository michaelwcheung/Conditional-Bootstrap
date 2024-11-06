# main server

server <- function(input, output, session) {
  # global store
  store <- reactiveValues(
    session_global = session,
    log = list(as.character(Sys.time()))
  )
  
  about_tab <- about_server(id = "module_about", store)
  data_tab <- data_server(id = "module_data", store)
  bootstrap_tab <- bootstrap_server(id = "module_bootstrap", store)
  results_tab <- results_server(id = "module_results", store)
  
  # functionality for tab switching by clicking button
  observeEvent(about_tab(), {
    updateTabsetPanel(session, "navbar", selected = "module_data-tab")
  })
  
  observeEvent(data_tab(), {
    updateTabsetPanel(session, "navbar", selected = "module_bootstrap-tab")
  })
  
  observeEvent(bootstrap_tab(), {
    updateTabsetPanel(session, "navbar", selected = "module_results-tab")
  })
}