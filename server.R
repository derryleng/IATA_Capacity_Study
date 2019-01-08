server <- function(input, output) {
  
  output$option_kpi <- renderUI({
    pickerInput("kpi", "Select KPI", choices = unique(metrics_list$kpi), width = "200px")
  })
  
  output$option_metric <- renderUI({
    req(input$kpi)
    pickerInput("metric", "Select Metric", choices = metrics_list[kpi == input$kpi]$metric, width = "200px")
  })
  
  output$option_1 <- renderUI({
    if (input$kpi == "En-Route ATFM Delay") {
      choices_ATFM_TYPE <- sort(unique(dat$ATFM$TYPE))
      pickerInput("type", "Select Type", choices = choices_ATFM_TYPE, selected="AREA (AUA)", width = "200px")
    } else if (input$kpi == "Airport Arrival AFTM Delay") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        choices_ATFM_APT_STATE <- sort(unique(dat$ATFM_APT$STATE))
        pickerInput("state", "Select State/FAB", choices = choices_ATFM_APT_STATE, selected="All Countries", width = "200px")
      }
    } else if (input$kpi == "ASMA Additional Time") {
      choices_ASMA_STATE <- sort(unique(dat$ASMA$STATE))
      pickerInput("state", "Select State", choices = choices_ASMA_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "Taxi-Out Additional Time") {
      choices_TAXI_STATE <- sort(unique(dat$TAXI$STATE))
      pickerInput("state", "Select State", choices = choices_TAXI_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      choices_PREDEP_STATE <- sort(unique(dat$PREDEP$STATE))
      pickerInput("state", "Select State", choices = choices_PREDEP_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      choices_BOTH_STATE <- unique(subset(dat$ATFM_APT_ANNUAL, grepl("^All *", NAME) | grepl("^NA$", NAME), select=c(STATE)))
      pickerInput("state", "Select State/FAB", choices = choices_BOTH_STATE, selected="All Countries", width = "200px")
    }
  })
  
  output$option_2 <- renderUI({
    if (input$kpi == "En-Route ATFM Delay") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        if (input$type == "ANSP (AUA)") {
          choices_ATFM_ANSP <- sort(unique(dat$ATFM[TYPE %in% "ANSP (AUA)"]$NAME))
          pickerInput("entity", "Select ANSP", choices = choices_ATFM_ANSP, width = "200px")
        } else if (input$type == "AREA (AUA)") {
          choices_ATFM_AREA1 <- sort(unique(dat$ATFM[TYPE %in% "AREA (AUA)"]$NAME %>% .[. %!in% "All AREA (AUA)"]))
          pickerInput("entity", "Select Area", choices = choices_ATFM_AREA1, selected = "SES Area", width = "200px")
        } else if (input$type == "AREA (FIR)") {
          choices_ATFM_AREA2 <- sort(unique(dat$ATFM[TYPE %in% "AREA (FIR)"]$NAME %>% .[. %!in% "All AREA (FIR)"]))
          pickerInput("entity", "Select Area", choices = choices_ATFM_AREA2, width = "200px")
        } else if (input$type == "COUNTRY (FIR)") {
          choices_ATFM_STATE <- sort(unique(dat$ATFM[dat$ATFM$TYPE == "COUNTRY (FIR)"]$NAME))
          pickerInput("entity", "Select State", choices = choices_ATFM_STATE, width = "200px")
        } else if (input$type == "FAB (FIR)") {
          choices_ATFM_FAB <- sort(unique(dat$ATFM[dat$ATFM$TYPE == "FAB (FIR)"]$NAME))
          pickerInput("entity", "Select FAB", choices = choices_ATFM_FAB, width = "200px")
        }
      }
    } else if (input$kpi == "Airport Arrival AFTM Delay") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
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
    #pickerInput("year", "Select Year", choices = as.character(years_range), selected = as.character(years_range), multiple = T, options = list("actions-box"=T), width = "200px")
    sliderInput("year", "Select Year", value=range(years_range), min=min(years_range), max=max(years_range), step=1, ticks=F, sep="", width="250px")
  })
  
  output$option_month <- renderUI({
    if (input$metric %in% metrics_list[monthly == T]$metric) {
      pickerInput("month", "Select Month", choices=as.vector(monthsfull), selected="July", width="200px")
    }
  })
  
  output$option_ranking <- renderUI({
    if (input$metric %in% metrics_list[ranking == T]$metric) {
      div(style="display: table; margin: 0 auto;", numericInput("top", "Display Top", value=10, min=3, max=100, step=1, width="100%"))
    }
  })
  
  output$option_breakdown <- renderUI({
    if (input$metric %in% metrics_list[breakdown == T]$metric) {
      div(style="text-align:center; height:25px;", checkboxInput("breakdown", "Category Breakdown", value=T))
    }
  })
  
  output$option_annual <- renderUI({
    if (input$metric == "Delays per Flight") {
      div(style="text-align:center; height:25px;", checkboxInput("annual", "Group By Year", value=T))
    }
  })

  draw_plot <- reactive({
    req(input$kpi)
    plt <- if (input$kpi == "En-Route ATFM Delay") {
      plot_ATFM(
        metric = input$metric,
        type = input$type,
        entity = input$entity,
        top = input$top,
        breakdown = input$breakdown,
        annual = input$annual,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
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
        years = seq(input$year[1], input$year[2], 1),
        month = input$month
      )
    } else if (input$kpi == "ASMA Additional Time") {
      plot_ASMA(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month
      )
    } else if (input$kpi == "Taxi-Out Additional Time") {
      plot_TAXI(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month
      )
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      plot_PREDEP(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month
      )
    }
    # Fix title cut off
    plt %>% layout(margin=list(t=input$fontsize*3))
  })
  
  output$plot <- renderPlotly(draw_plot())
  
  output$download <- downloadHandler(
    filename = function() {
      filenom <- metrics_list[kpi == input$kpi & metric == input$metric]$shortname
      if (metrics_list[kpi == input$kpi & metric == input$metric]$ranking == F) {
        filenom <- paste0(filenom,"_",input$entity)
      } else {
        filenom <- paste0(filenom,"_",ifelse(input$kpi == "En-Route ATFM Delay",input$type,input$state))
      }
      if (metrics_list[kpi == input$kpi & metric == input$metric]$monthly == T) filenom <- gsub("MON",months[which(monthsfull==input$month)],filenom)
      if (input$metric == "Delays per Flight" & input$annual == F) filenom <- paste0(filenom,"_Monthly")
      if (input$year[1] != input$year[2]) {
        filenom <- paste0(filenom,"_",paste(input$year,collapse="-"))
      } else {
        filenom <- paste0(filenom,"_",input$year[1])
      }
      return(paste0(filenom,".png"))
    },
    content = function(file) {
      plotly_IMAGE(draw_plot(), width=input$exportx, height=input$exporty, format="png", out_file=file)
    }
  )
  
}
