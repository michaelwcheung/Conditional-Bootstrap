# UI

ui <- fluidPage(
        titlePanel("Conditional Bootstrap for EMM"),
        mainPanel(
            h2("Subgroup Prevalence in Data"),
            DT::dataTableOutput("tab1"),
            h2("Bootstraped CATEs by Quantile and Subgroup"),
            DT::dataTableOutput("tab2"),
            plotOutput("Plot"),
            uiOutput("checkboxes")
        )
    
    #-----------------------------------------------Add sidebar with links to skip to different sections of output?----------------------------
    #     sidebarLayout(
    #         sidebarPanel(
    #             uiOutput("checkboxes")
    #                 # p("<a href='#'>Subgroup Prevalence in Data</a>"), 
    #                 # p("Bootstrap CATEs by Quantile and Subgroup")
    #         ),
    #         mainPanel(
    #             h2("Subgroup Prevalence in Data"),
    #             DT::dataTableOutput("tab1"),
    #             h2("Bootstrap CATEs by Quantile and Subgroup"),
    #             DT::dataTableOutput("tab2"),
    #             plotOutput("plot")
    #         )
    #     )
    # )
    #--------------------------------------------------------------------------------------------------------------------------------------------
)