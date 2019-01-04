server <- function(input, output) {
  
  output$option_kpi <- renderUI({
    pickerInput("kpi", "Select KPI", choices = KPIs, width="200px")
  })
  
  output$option_metric <- renderUI({
    req(input$kpi)
    if (input$kpi %in% KPIs[1]) {
      pickerInput("metric", "Select Metric", choices=ATFM_metrics, width="200px")
    } else if (input$kpi %in% KPIs[2]) {
      pickerInput("metric", "Select Metric", choices=ATFM_APT_metrics, width="200px")
    } else if (input$kpi %in% KPIs[3:4]) {
      pickerInput("metric", "Select Metric", choices=APT_metrics, width="200px")
    } else if (input$kpi %in% KPIs[5]) {
      pickerInput("metric", "Select Metric", choices=PREDEP_metrics, width="200px")
    }
  })
  
  output$option_type <- renderUI({
    if (input$kpi == KPIs[1]) {
      pickerInput("type", "Select Type", choices=sort(unique(d()$TYPE)), width="200px")
    } else if (input$kpi %in% KPIs[2:5] & input$metric %!in% c("Delay Ranking (Yearly)", "Delay Ranking (Month)")) {
      pickerInput("state", "Select State", choices=sort(unique(d()$STATE)), width="200px")
    }
  })
  
  observeEvent(input$metric, {
    if (input$metric %!in% c("Delay Ranking (Yearly)","Delay Ranking (Month)")) {
      output$option_entity1 <- renderUI({
        if (input$kpi == KPIs[1]) {
          pickerInput("entity", "Select Entity", selected="All ANSP (AUA)", choices=sort(unique(d()$NAME[d()$TYPE %in% input$type])), width="200px")
        }
      })
      output$option_entity2 <- renderUI({
        if (input$kpi %in% KPIs[2:5]) {
          pickerInput("entity", "Select Airport", choices=sort(unique(d()$NAME[d()$STATE %in% input$state])), width="200px")
        }
      })
    } else {
      output$option_ranking <- renderUI({
        div(style="display: table; margin: 0 auto;", numericInput("top", "Display Top", value=10, min=3, max=100, step=1, width="100%"))
      })
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
  
  output$option_month <- renderUI({
    if (input$metric %in% c("Delays per Flight (Month)","Delay Ranking (Month)", "Average Monthly Delays (Month)")) {
      pickerInput("month", "Select Month", choices=as.vector(monthsfull), selected="July", width="200px")
    }
  })
  
  output$option_year <- renderUI({
    pickerInput("year", "Select Year", choices=seq(2011,2018,1), selected=seq(2011,2018,1), width="200px", multiple=T, options=list("actions-box"=T))
  })
  
  d <- reactive({
    if (input$kpi == KPIs[1]) {
      dat$ATFM
    } else if (input$kpi == KPIs[2]) {
      dat$ATFM_APT
    } else if (input$kpi == KPIs[3]) {
      dat$ASMA
    } else if (input$kpi == KPIs[4]) {
      dat$TAXI
    } else if (input$kpi == KPIs[5]) {
      dat$PREDEP
    }
  })
  
  draw_plot <- reactive({
    req(input$kpi)
    if (input$kpi == KPIs[1]) {
      plot_ATFM(
        dataset = d(),
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
    } else if (input$kpi == KPIs[2]) {
      plot_ATFM_APT(
        dataset = d(),
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
    } else if (input$kpi %in% KPIs[3:4]) {
      plot_ADDITIONAL(
        dataset = d(),
        metric = input$metric,
        type = input$state,
        entity = input$entity,
        fontsize = input$fontsize,
        years = input$year,
        month = input$month
      )
    } else if (input$kpi == KPIs[5]) {
      plot_PREDEP(
        dataset = d(),
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
      plotly_IMAGE(draw_plot(), width=1600, height=1000, format="png", out_file=file)
    }
  )
  
}
