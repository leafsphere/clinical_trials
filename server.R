# define server logic

server <- function(input, output) {

  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = FALSE)
    } else {
      ret = studies
    }
    if (input$source_class == "NA") {
      ret <- ret |>
        filter(is.na(source_class))
    }
    else if (input$source_class != "") {  # filter by agency class if one is selected
      ret <- ret |>
        filter(source_class == !!input$source_class)
    }
    ret |> head(max_num_studies)
  }
  )

  output$distPlot <- renderPlot({
    get_studies() |>
      collect() |>
      create_phase_histogram(studies)
  })

  output$endpointPlot <- renderPlot({
    get_studies() |>
      collect() |>
      create_endpoint_histogram(endpoints)
  })

  output$conditionsPlot <- renderPlot({
    get_studies() |>
      collect() |>
      create_conditions_histogram(conditions)
  })

  output$studyTypesPlot <- renderPlot({
    get_studies() |>
      collect() |>
      create_study_type_histogram()
  })

  output$countries_table <- renderDataTable({
    get_studies() |>
      collect() |>
      create_topcountries_table(countries)
  })

  output$observationalPlot <- renderPlot({
    get_studies() |>
      collect() |>
      create_observational_histogram(designs)
  })

  output$trial_table = renderDataTable({
    get_studies() |>
      collect() |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })

}
