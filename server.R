server <- function(input, output) {
  
  output$option_kpi <- renderUI({
    pickerInput("kpi", "Select KPI", choices = KPIs, width = "200px")
  })
  
  output$option_metric <- renderUI({
    req(input$kpi)
    if (input$kpi == "En-Route ATFM Delay") {
      pickerInput("metric", "Select Metric", choices = metrics_ATFM, width = "200px")
    } else if (input$kpi == "Airport Arrival AFTM Delay") {
      pickerInput("metric", "Select Metric", choices = metrics_ATFM_APT, width = "200px")
    } else if (input$kpi %in% c("ASMA Additional Time","Taxi-Out Additional Time")) {
      pickerInput("metric", "Select Metric", choices = metrics_APT, width = "200px")
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      pickerInput("metric", "Select Metric", choices = metrics_PREDEP, width = "200px")
    } else if (input$kpi == "En-Route vs Airport ATFM") {
      pickerInput("metric", "Select Metric", choices = metrics_ATFM_BOTH, width = "200px")
    }
  })
  
  output$option_1 <- renderUI({
    if (input$kpi == "En-Route ATFM Delay") {
      pickerInput("type", "Select Type", choices = choices_ATFM_TYPE, selected="AREA (AUA)", width = "200px")
    } else if (input$kpi == "Airport Arrival AFTM Delay") {
      if (input$metric %!in% metrics_ranking) {
        pickerInput("state", "Select State/FAB", choices = choices_ATFM_APT_STATE, selected="All Countries", width = "200px")
      }
    } else if (input$kpi == "ASMA Additional Time") {
      pickerInput("state", "Select State", choices = choices_ASMA_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "Taxi-Out Additional Time") {
      pickerInput("state", "Select State", choices = choices_TAXI_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      pickerInput("state", "Select State", choices = choices_PREDEP_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      pickerInput("state", "Select State/FAB", choices = choices_BOTH_STATE, selected="All Countries", width = "200px")
    }
  })
  
  output$option_2 <- renderUI({
    if (input$kpi == "En-Route ATFM Delay") {
      if (input$type == "ANSP (AUA)") {
        pickerInput("entity", "Select ANSP", choices = choices_ATFM_ANSP, width = "200px")
      } else if (input$type == "AREA (AUA)") {
        pickerInput("entity", "Select Area", choices = choices_ATFM_AREA1, selected = "SES Area (RP1)", width = "200px")
      } else if (input$type == "AREA (FIR)") {
        pickerInput("entity", "Select Area", choices = choices_ATFM_AREA2, width = "200px")
      } else if (input$type == "COUNTRY (FIR)") {
        pickerInput("entity", "Select State", choices = choices_ATFM_STATE, width = "200px")
      } else if (input$type == "FAB (FIR)") {
        pickerInput("entity", "Select FAB", choices = choices_ATFM_FAB, width = "200px")
      }
    } else if (input$kpi == "Airport Arrival AFTM Delay") {
      if (input$metric %!in% metrics_ranking) {
        choices_ATFM_APT_AIRPORT <- sort(unique(dat$ATFM_APT[STATE %in% input$state]$NAME))
        pickerInput("entity", "Select Airport", choices = choices_ATFM_APT_AIRPORT, selected = grep("^All *", choices_ATFM_APT_AIRPORT, value=T), width = "200px")
      }
    } else if (input$kpi == "ASMA Additional Time") {
      choices_ASMA_AIRPORT <- sort(unique(dat$ASMA[STATE %in% input$state]$NAME))
      pickerInput("entity", "Select Airport", choices = choices_ASMA_AIRPORT, selected = "London/ Gatwick", width = "200px")
    } else if (input$kpi == "Taxi-Out Additional Time") {
      choices_TAXI_AIRPORT <- sort(unique(dat$TAXI[STATE %in% input$state]$NAME))
      pickerInput("entity", "Select Airport", choices = choices_TAXI_AIRPORT, selected = "London/ Gatwick", width = "200px")
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      choices_PREDEP_AIRPORT <- sort(unique(dat$PREDEP[STATE %in% input$state]$NAME))
      pickerInput("entity", "Select Airport", choices = choices_PREDEP_AIRPORT, selected = "London/ Gatwick", width = "200px")
    }
  })
  
  output$option_year <- renderUI({
    pickerInput("year", "Select Year", choices = as.character(years_range), selected = as.character(years_range), multiple = T, options = list("actions-box"=T), width = "200px")
  })
  
  output$option_month <- renderUI({
    if (input$metric %in% metrics_month) {
      pickerInput("month", "Select Month", choices=as.vector(monthsfull), selected="July", width="200px")
    }
  })
  
  output$option_ranking <- renderUI({
    if (input$metric %in% metrics_ranking) {
      div(style="display: table; margin: 0 auto;", numericInput("top", "Display Top", value=10, min=3, max=100, step=1, width="100%"))
    }
  })
  
  output$option_grouping <- renderUI({
    if (input$metric == "Delays per Flight") {
      div(
        style="text-align:center;",
        div(style="height:25px;", checkboxInput("breakdown", "Category Breakdown", value=T)),
        div(style="height:25px;", checkboxInput("annual", "Group By Year", value=T))
      )
    }
  })
  
  draw_plot <- reactive({
    req(input$kpi)
    if (input$kpi == "En-Route ATFM Delay") {
      plot_ATFM(
        metric = input$metric,
        type = input$type,
        entity = input$entity,
        top = input$top,
        breakdown = input$breakdown,
        annual = input$annual,
        fontsize = input$fontsize,
        years = input$year,
        month = input$month
      )
    } else if (input$kpi == "Airport Arrival AFTM Delay") {
      plot_ATFM_APT(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        top = input$top,
        breakdown = input$breakdown,
        annual = input$annual,
        fontsize = input$fontsize,
        years = input$year,
        month = input$month
      )
    } else if (input$kpi == "ASMA Additional Time") {
      plot_ASMA(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = input$year,
        month = input$month
      )
    } else if (input$kpi == "Taxi-Out Additional Time") {
      plot_TAXI(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = input$year,
        month = input$month
      )
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      plot_PREDEP(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = input$year,
        month = input$month
      )
    }
  })
  
  output$plot <- renderPlotly(draw_plot())

  output$download <- downloadHandler(
    filename = function() {
      paste(gsub(" ","_",input$kpi), "_", gsub("-|:| ","",Sys.time()), ".png", sep="")
    },
    content = function(file) {
      plotly_IMAGE(draw_plot(), width=input$exportx, height=input$exporty, format="png", out_file=file)
    }
  )
  
}
