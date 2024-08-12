# main UI

ui <- htmltools::tagQuery(navbarPage(
  title = "THRIVE",
  id = "navbar",
  theme = bs_theme(bootswatch = "zephyr", version = 5),
  collapsible = TRUE,
  padding = 10,
  
  tags$head(
    tags$style(HTML("div.container-fluid { width: 97%; } h3 { padding-top: 40px; }")),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  about_ui(id = "module_about"),
  data_ui(id = "module_data"),
  bootstrap_ui(id = "module_bootstrap"),
  results_ui(id = "module_results")
))$
  find(".navbar")$
  toggleClass("navbar-default bg-primary")$
  addAttrs(`data-bs-theme` = "dark")$
  allTags()