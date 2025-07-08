# define UI

ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with search and dropdown menu of agency class; default shows all studies
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      selectInput("source_class",
                  label = h3("Sponsor Type"),
                  choices = list("All" = "",
                                 "Ambiguous" = "NA",
                                 "Federal" = "FED",
                                 "Individual" = "INDIV",
                                 "Industry" = "INDUSTRY",
                                 "Network" = "NETWORK",
                                 "NIH" = "NIH",
                                 "Other" = "OTHER",
                                 "Other gov" = "OTHER_GOV",
                                 "Unknown" = "UNKNOWN"),
                  selected = "")
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phases Plot", plotOutput("distPlot")),
        tabPanel("Endpoint Met", plotOutput("endpointPlot")),
        tabPanel("Conditions Examined", plotOutput("conditionsPlot")),
        tabPanel("Study Types", plotOutput("studyTypesPlot")),
        tabPanel("Countries Involved", dataTableOutput("countries_table")),
      ),
      dataTableOutput("trial_table")
    )
  )
)
