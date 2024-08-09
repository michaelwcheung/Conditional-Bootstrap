# main UI

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  # theme = bs_theme(bootswatch = "flatly", version = 5), # flatly, zephyr
  theme = shinytheme("flatly"),
  useShinyFeedback(),
  tagList(
    titlePanel("Transparent High-dimensional Reproducible Inference through Bootstrap Validation and Exploration (THRIVE)"),
    p("Welcome message!"),
    p("Instructions:"),
    tags$ul(
      tags$li("In the 'Upload data' tab, upload your dataset and select variables for analysis."),
      tags$li("In the 'Set parameters' tab, set your parameters for the bootstrap process."),
      tags$li("In the 'Bootstrap results' tab, view and download plots and tables about your effect modifiers by your chosen quantiles.")
    ),
    p("If you have any feedback for the organization and design of our page, please let us know at ", a(href="https://forms.gle/5e2kCDpNN2VvVkSd7", "this form", .noWS = "outside"), "."),
    tabsetPanel(
      id = "tabs",
      data_ui(id = "module_data"),
      bootstrap_ui(id = "module_bootstrap"),
      results_ui(id = "module_results")
    )
  )
)

# ui <- page_navbar(
#   tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
#   ),
#   
#   theme = bs_theme(bootswatch = "flatly", version = 5),
#   title = "Conditional Bootstrap for EMM",
#   underline = T,
#   nav_panel(
#     title = "Home",
#     p("Welcome message!")
#   ),
#   nav_panel(
#     title = "Step 1: Upload Data",
#     data_ui(id = "module_data")
#   ),
#   nav_panel(
#     title = "Step 2: Set Bootstrap Parameters",
#     bootstrap_ui(id = "module_bootstrap")
#   ),
#   nav_panel(
#     title = "Step 3: Obtain Bootstrap Results",
#     results_ui(id = "module_results")
#   ),
#   nav_spacer()
# 
# )