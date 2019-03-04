server <- function(input, output) {
  
  # Level 1 KPI options
  output$option_kpi <- renderUI({
    pickerInput("kpi", "Select KPI", choices = unique(metrics_list$kpi), width = "200px")
  })
  
  # Level 2 Metric options
  output$option_metric <- renderUI({
    req(input$kpi)
    pickerInput("metric", "Select Metric", choices = metrics_list[kpi == input$kpi]$metric, width = "200px")
  })
  
  # Level 3 options
  output$option_1 <- renderUI({
    if (input$kpi == "En-Route ATFM Delay") {
      if (input$metric %in% metrics_list[grep("^FAB State*", metrics_list$metric)]$metric) {
        choices_ATFM_FABSTATE <- sort(unique(dat$ATFM[dat$ATFM$TYPE %in% "FAB (FIR)" & dat$ATFM$NAME %!in% c("All FAB (FIR)", "FAB CE", "FAB CE (SES RP1)")]$NAME))
        pickerInput("type", "Select FAB", choices = choices_ATFM_FABSTATE, selected="FABEC", width = "200px")
      } else if (input$metric %in% metrics_list[grep("^SES State*", metrics_list$metric)]$metric) {
        # No selection needed here
      } else {
        choices_ATFM_TYPE <- sort(unique(dat$ATFM$TYPE))
        pickerInput("type", "Select Type", choices = choices_ATFM_TYPE, selected="AREA (AUA)", width = "200px")
      }
      
    } else if (input$kpi == "Airport Arrival ATFM Delay") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        choices_ATFM_APT_STATEFAB <- list(
          "State"=sort(unique(dat$ATFM_APT[dat$ATFM_APT$STATE %!in% ATFM_APT_FAB]$STATE)),
          "FAB"=ATFM_APT_FAB
        )
        pickerInput("state", "Select State/FAB", choices = choices_ATFM_APT_STATEFAB, selected="All Countries", width = "200px")
      } else if (grepl("^State Airport *", input$metric)) {
        choice_ATFM_APT_STATE <- sort(unique(dat$ATFM_APT[dat$ATFM_APT$STATE %!in% c(ATFM_APT_FAB, "All Countries")]$STATE))
        pickerInput("state", "Select State", choices = choice_ATFM_APT_STATE, selected="United Kingdom", width = "200px")
      }
      
    } else if (input$kpi == "ASMA Additional Time") {
      choices_ASMA_STATE <- sort(unique(dat$ASMA$STATE))
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        pickerInput("state", "Select State", choices = choices_ASMA_STATE, selected="United Kingdom", width = "200px")
      } else if (input$metric %in% metrics_list[ranking == T]$metric & !grepl("Top 20 Airport Delay Ranking", input$metric)) {
        pickerInput("state", "Select State", choices = c("All Countries", choices_ASMA_STATE), selected="All Countries", width = "200px")
      }
    } else if (input$kpi == "Taxi-Out Additional Time") {
      choices_TAXI_STATE <- sort(unique(dat$TAXI$STATE))
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        pickerInput("state", "Select State", choices = choices_TAXI_STATE, selected="United Kingdom", width = "200px")
      } else if (input$metric %in% metrics_list[ranking == T]$metric & !grepl("Top 20 Airport Delay Ranking", input$metric)) {
        pickerInput("state", "Select State", choices = c("All Countries", choices_TAXI_STATE), selected="All Countries", width = "200px")
      }
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      choices_PREDEP_STATE <- sort(unique(dat$PREDEP$STATE))
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        pickerInput("state", "Select State", choices = choices_PREDEP_STATE, selected="United Kingdom", width = "200px")
      } else if (input$metric %in% metrics_list[ranking == T]$metric & !grepl("Top 20 Airport Delay Ranking", input$metric)) {
        pickerInput("state", "Select State", choices = c("All Countries", choices_PREDEP_STATE), selected="All Countries", width = "200px")
      }
    } else if (input$kpi %in% c("ASMA/Taxi-Out/Pre-Dep Delay", "Airport Delays")) {
      choices_ASMATAXIPREDEP_STATE <- union_all(dat$ASMA$STATE, dat$TAXI$STATE) %>% union_all(., dat$PREDEP$STATE) %>% unique() %>% sort()
      pickerInput("state", "Select State", choices = choices_ASMATAXIPREDEP_STATE, selected="United Kingdom", width = "200px")
      
    } else if (input$kpi == "En-Route vs Airport ATFM") {
      choices_BOTH_STATE <- list(
        "State"=unique(subset(dat$ATFM_APT_ANNUAL, (grepl("^All *", NAME) | grepl("^NA$", NAME)) & STATE %!in% ATFM_APT_FAB, select=c(STATE))),
        "FAB"=ATFM_APT_FAB
      )
      pickerInput("state", "Select State/FAB", choices = choices_BOTH_STATE, selected="United Kingdom", width = "200px")
    } else if (input$kpi == "Traffic Forecast") {
      choices_FORECAST_STATEFAB <- list(
        "State" = dat$TRAFFIC_FORECAST %>% subset(ENTITY_TYPE == "State") %>% .$ENTITY %>% unique() %>% sort(),
        "FAB" = dat$TRAFFIC_FORECAST %>% subset(ENTITY_TYPE == "FAB") %>% .$ENTITY %>% unique() %>% sort(),
        "Other" = dat$TRAFFIC_FORECAST %>% subset(ENTITY_TYPE == "Other") %>% .$ENTITY %>% unique() %>% sort()
      )
      pickerInput("state", "Select State/FAB/Area", choices = choices_FORECAST_STATEFAB, selected="Germany", width = "200px")
    }
  })
  
  # Level 4 options
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
    } else if (input$kpi == "Airport Arrival ATFM Delay") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        choices_ATFM_APT_AIRPORT <- sort(unique(dat$ATFM_APT[STATE %in% input$state]$NAME))
        pickerInput("entity", "Select Airport", choices = choices_ATFM_APT_AIRPORT, selected = grep("^All *", choices_ATFM_APT_AIRPORT, value=T), width = "200px")
      }
    } else if (input$kpi == "ASMA Additional Time") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        choices_ASMA_AIRPORT <- sort(unique(dat$ASMA[STATE %in% input$state]$NAME))
        pickerInput("entity", "Select Airport", choices = choices_ASMA_AIRPORT, selected = "London/ Gatwick", width = "200px")
      }
    } else if (input$kpi == "Taxi-Out Additional Time") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        choices_TAXI_AIRPORT <- sort(unique(dat$TAXI[STATE %in% input$state]$NAME))
        pickerInput("entity", "Select Airport", choices = choices_TAXI_AIRPORT, selected = "London/ Gatwick", width = "200px")
      }
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      if (input$metric %!in% metrics_list[ranking == T]$metric) {
        choices_PREDEP_AIRPORT <- sort(unique(dat$PREDEP[STATE %in% input$state]$NAME))
        pickerInput("entity", "Select Airport", choices = choices_PREDEP_AIRPORT, selected = "London/ Gatwick", width = "200px")
      }
    } else if (input$kpi %in% c("ASMA/Taxi-Out/Pre-Dep Delay", "Airport Delays")) {
      choices_ASMA_TAXI_PREDEP_AIRPORT <- union_all(dat$ASMA[STATE %in% input$state]$NAME, dat$TAXI[STATE %in% input$state]$NAME) %>% 
        union_all(., dat$PREDEP[STATE %in% input$state]$NAME) %>% unique() %>% sort()
      pickerInput("entity", "Select Airport", choices = choices_ASMA_TAXI_PREDEP_AIRPORT, selected = "London/ Gatwick", width = "200px")
    }
  })
  
  # Options for year range selection when available
  output$option_year <- renderUI({
    if (input$kpi == "Traffic Forecast") {
      sliderInput("year", "Select Year", value=c(2011,2021), min=min(years_range_extended), max=max(years_range_extended), step=1, ticks=F, sep="", width="228px")
    } else {
      sliderInput("year", "Select Year", value=c(2015,2018), min=min(years_range), max=max(years_range), step=1, ticks=F, sep="", width="228px")
    }
  })
  
  # Option for selecting which delay category to rank by (ATFM Only)
  output$option_rankingcategories <- renderUI({
    if (input$metric %in% metrics_list[ranking == T]$metric & input$kpi %in% c("En-Route ATFM Delay", "Airport Arrival ATFM Delay")) {
      choice_RANKING <- c("Total Flights", "Total Delay", "Average Delay", ATFM_DELAY_CATEGORIES)
      pickerInput("rank", "Rank By", choices=choice_RANKING, selected="Average Delay", width="200px")
    }
  })
  
  rank <- reactive({
    if (input$rank == "Total Flights") {
      "FLIGHTS_TOTAL"
    } else if (input$rank == "Total Delay") {
      "DELAY"
    }  else if (input$rank == "Average Delay") {
      "DELAY_AVG"
    } else {
      paste0(strsplit(input$rank, split=" - ")[[1]][1], "_AVG")
    }
  })
  
  # Option for month selection when available
  output$option_month <- renderUI({
    if (input$metric %in% metrics_list[monthly == T]$metric) {
      pickerInput("month", "Select Month", choices=as.vector(monthsfull), selected="July", width="200px")
    }
  })
  
  # Option for selecting which delay categories to display in Delays per Flight (& monthly) metrics
  output$option_breakdowncategories <- renderUI({
    if (input$metric %in% c("Delays per Flight", "Delays per Flight (Month)") & input$breakdown == T) {
      pickerInput("category", "Select Categories", 
                  choices = ATFM_DELAY_CATEGORIES, selected = ATFM_DELAY_CATEGORIES, 
                  multiple = T, options = list(`actions-box` = TRUE), width = "200px")
    }
  })
  
  # Option for top X entities to display in ranking metrics
  output$option_ranking <- renderUI({
    if (input$metric %in% metrics_list[ranking == T]$metric & !grepl("Top 20 Airport Delay Ranking", input$metric)) {
      div(style="display: table; margin: 0 auto;", numericInput("top", "Display Top", value=10, min=3, max=100, step=1, width="100%"))
    }
  })
  
  # Option for breaking down delay causes in Delays per Flight (& monthly) metrics
  output$option_breakdown <- renderUI({
    if (input$metric %in% metrics_list[breakdown == T]$metric) {
      div(style="text-align:center; height:25px;", checkboxInput("breakdown", "Category Breakdown", value=T))
    }
  })
  
  # Option for grouping bars by years/month in Delays per Flight metrics
  output$option_annual <- renderUI({
    if (input$metric %in% metrics_list[date_grouping == T]$metric) {
      div(style="text-align:center; height:25px;", checkboxInput("annual", "Group By Year", value=T))
    }
  })
  
  # Option for toggling annual targets on ER ATFM Delays per Flight metric
  output$option_annualtargets <- renderUI({
    if (input$kpi == "En-Route ATFM Delay" & input$metric == "Delays per Flight" & input$annual == T) {
      div(style="text-align:center; height:25px;", checkboxInput("annualtargets", "Display Annual Targets", value=T))
    }
  })
  
  # Option for toggling total flights on ATFM Delays per Flight metric
  output$option_totalflights <- renderUI({
    if (input$metric == "Delays per Flight" | input$kpi == "En-Route vs Airport ATFM") {
      div(style="text-align:center; height:25px;", checkboxInput("totalflights", "Display Total Flights", value=T))
    }
  })
  
  # Option to toggle display of legends
  output$option_legend <- renderUI({
    if (input$metric %in% metrics_list[legend == T]$metric) {
      div(style="text-align:center; height:25px;", checkboxInput("legend", "Display Legend", value=T))
    } else {
      div(style="text-align:center; height:25px;", checkboxInput("legend", "Display Legend", value=F))
    }
  })
  
  # Option for toggling title of plots
  output$option_toggletitle <- renderUI({
    div(style="text-align:center; height:25px;", checkboxInput("title", "Display Title", value=T))
  })
  
  # Option for changing barmode
  output$option_barmode <- renderUI({
    if (input$kpi == "ASMA/Taxi-Out/Pre-Dep Delay") {
      if (input$annual) {
        div(style="padding-top:15px; text-align:center;", radioButtons("barmode", "Barmode", choices=c("Separate","Stacked","Grouped"), inline=T))
      } else {
        div(style="padding-top:15px; text-align:center;", radioButtons("barmode", "Barmode", choices=c("Stacked","Grouped"), inline=T))
      }
    }
  })
  
  # Option for switching between AL and APT PREDEP Delays (ASMA/TAXI-OUT/PREDEP metric only)
  output$option_togglepredep <- renderUI({
    if (input$kpi == "ASMA/Taxi-Out/Pre-Dep Delay") {
      div(style="text-align:center;", radioButtons("predep", "Pre-Dep. Data", choices=c("Airlines","Airports"), inline=T))
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
        category = input$category,
        annual = input$annual,
        annualtargets = input$annualtargets,
        totalflights = input$totalflights,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month,
        rank = ifelse(!is.null(input$rank),rank(),NA),
        rank_title = ifelse(!is.null(input$rank),input$rank,NA)
      )
    } else if (input$kpi == "Airport Arrival ATFM Delay") {
      plot_ATFM_APT(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        top = input$top,
        breakdown = input$breakdown,
        category = input$category,
        annual = input$annual,
        annualtargets = input$annualtargets,
        totalflights = input$totalflights,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month,
        rank = ifelse(!is.null(input$rank),rank(),NA),
        rank_title = ifelse(!is.null(input$rank),input$rank,NA)
      )
    } else if (input$kpi == "ASMA Additional Time") {
      plot_ASMA(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        annual = input$annual,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month,
        top = input$top
      )
    } else if (input$kpi == "Taxi-Out Additional Time") {
      plot_TAXI(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        annual = input$annual,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month,
        top = input$top
      )
    } else if (input$kpi == "ATC Pre-Departure Delay") {
      plot_PREDEP(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        annual = input$annual,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month,
        top = input$top
      )
    } else if (input$kpi == "ASMA/Taxi-Out/Pre-Dep Delay") {
      plot_ASMA_TAXI_PREDEP(
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        annual = input$annual,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        barmode = input$barmode,
        predep_source = input$predep
      )
    } else if (input$kpi == "Airport Delays") {
      subplot(
        subplot(
          plot_ATFM_APT(
            metric = input$metric,
            type = input$state,
            entity = input$entity,
            breakdown = T,
            category = ATFM_DELAY_CATEGORIES,
            annual = input$annual,
            annualtargets = T,
            totalflights = F,
            fontsize = input$fontsize,
            years = seq(input$year[1], input$year[2], 1)
          ) %>% layout(title="Arrival ATFM Delay"),
          plot_ASMA(
            metric = input$metric,
            type = input$state,
            entity = input$entity,
            annual = input$annual,
            fontsize = input$fontsize,
            years = seq(input$year[1], input$year[2], 1)
          ) %>% layout(title="Additional ASMA Time")
        ),
        subplot(
          plot_TAXI(
            metric = input$metric,
            type = input$state,
            entity = input$entity,
            annual = input$annual,
            fontsize = input$fontsize,
            years = seq(input$year[1], input$year[2], 1)
          ) %>% layout(title="Additional Taxi-Out Time"),
          plot_PREDEP(
            metric = input$metric,
            type = input$state,
            entity = input$entity,
            annual = input$annual,
            fontsize = input$fontsize,
            years = seq(input$year[1], input$year[2], 1)
          ) %>% layout(title="Pre-Departure Delay")
        ),
        nrows=2
      ) %>% layout(title=paste("Average Airport Delay Statistics for", input$entity))
    } else if (input$kpi == "En-Route vs Airport ATFM") {
      plot_ATFM_BOTH(
        metric = input$metric,
        entity = input$state,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1),
        month = input$month,
        totalflights = input$totalflights
      )
    } else if (input$kpi == "Traffic Forecast") {
      plot_TRAFFIC_FORECAST(
        entity = input$state,
        fontsize = input$fontsize,
        years = seq(input$year[1], input$year[2], 1)
      )
    }

    # Adjust plot dimension on window resize
    plt <- plt %>% layout(
      width = as.numeric(input$dimension[1])-28,
      height = as.numeric(input$dimension[2])-28
    )
    
    if (input$legend) {
      plt <- plt %>% layout(showlegend = T)
    } else {
      plt <- plt %>% layout(showlegend = F, margin=list(r=30))
    }
    
    # Fix display cutoff issues
    if (input$title) {
      plt <- plt %>% layout(
        margin=list(l=80, t=input$fontsize*3),
        legend=list(x=1.07,y=0.5)
      )
    } else {
      plt <- plt %>% layout(
        title=NA,
        legend=list(x=1.07,y=0.5)
      )
    }

    plt
    
  })
  
  observeEvent(input$dimension,{
    output$plot <- renderPlotly(draw_plot())
  })
  
  output$download <- downloadHandler(
    filename = function() {
      filenom <- metrics_list[kpi == input$kpi & metric == input$metric]$shortname
      if (metrics_list[kpi == input$kpi & metric == input$metric]$ranking == F) {
        filenom <- paste0(filenom,"_",ifelse(input$kpi == "Traffic Forecast",input$state,input$entity))
      } else {
        filenom <- paste0(filenom,"_",ifelse(input$kpi == "En-Route ATFM Delay",input$type,input$state))
      }
      if (metrics_list[kpi == input$kpi & metric == input$metric]$monthly == T) filenom <- gsub("MON",months[which(monthsfull==input$month)],filenom)
      if (input$metric == "Delays per Flight" & input$annual == F) filenom <- paste0(filenom,"_Monthly")
      filenom <- gsub("STATEAPT", input$type, filenom)
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
