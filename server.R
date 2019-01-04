server <- function(input, output) {
  
  output$options1 <- renderUI({
    req(input$dat)
    if (input$dat %in% KPIs[1]) {
      pickerInput("metric", "Select Metric", choices=ATFM_metrics, width="200px")
    } else if (input$dat %in% KPIs[2]) {
      pickerInput("metric", "Select Metric", choices=ATFM_APT_metrics, width="200px")
    } else if (input$dat %in% KPIs[3:4]) {
      pickerInput("metric", "Select Metric", choices=APT_metrics, width="200px")
    } else if (input$dat %in% KPIs[5]) {
      pickerInput("metric", "Select Metric", choices=PREDEP_metrics, width="200px")
    }
  })
  
  d <- reactive({
    if (input$dat == KPIs[1]) {
      dat$ATFM
    } else if (input$dat == KPIs[2]) {
      dat$ATFM_APT
    } else if (input$dat == KPIs[3]) {
      dat$ASMA
    } else if (input$dat == KPIs[4]) {
      dat$TAXI
    } else if (input$dat == KPIs[5]) {
      dat$PREDEP
    }
  })
  
  output$options2 <- renderUI({
    req(input$dat)
    if (input$dat == KPIs[1]) {
      pickerInput("type", "Select Type", choices=sort(unique(d()$TYPE)), width="200px")
    } else if (input$dat %in% KPIs[2:5]) {
      req(input$metric)
      if (input$metric %!in% c("Delay by Centre", "July Delay by Centre", "August Delay by Centre", "Delay by Airport", "July Delay by Airport", "August Delay by Airport")) {
        pickerInput("state", "Select State", choices=sort(unique(d()$STATE)), width="200px")
      }
    }
  })
  
  output$options3 <- renderUI({
    req(input$metric)
    if (input$metric %!in% c("Delay by Centre", "July Delay by Centre", "August Delay by Centre", "Delay by Airport", "July Delay by Airport", "August Delay by Airport")) {
      if (input$dat == KPIs[1]) {
        req(input$type)
        pickerInput("entity", "Select Entity", choices=sort(unique(d()$NAME[d()$TYPE %in% input$type])), width="200px")
      } else if (input$dat %in% KPIs[2:5]) {
        req(input$state)
        pickerInput("entity", "Select Airport", choices=sort(unique(d()$NAME[d()$STATE %in% input$state])), width="200px")
      }
    } else {
      div(style="display: table; margin: 0 auto;", numericInput("top", "Display", value=10, min=3, max=50, step=1, width="100%"))
    }
  })
  
  output$options4 <- renderUI({
    if (input$metric == "Delays per Flight") {
      tagList(
        div(style="text-align: center;", checkboxInput("breakdown", "Category Breakdown", value=T)),
        div(style="text-align: center;", checkboxInput("annual", "Group by Year", value=F))
      )
    }
  })

  output$options5 <- renderUI({
    pickerInput("year", "Select Year", choices=seq(2011,2018,1), selected=seq(2011,2018,1), width="200px", multiple=T, options=list("actions-box"=T))
  })
  
  output$plot <- renderPlotly({
    draw_plot(
      dataset=d(),
      kpi=input$dat,
      metric=input$metric,
      type=input$type,
      entity=input$entity,
      breakdown=input$breakdown,
      annual=input$annual,
      top=input$top,
      fontsize=input$fontsize,
      years=input$year
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      paste(gsub(" ","_",input$dat), "_", gsub("-|:| ","",Sys.time()), ".png", sep="")
    },
    content = function(file) {
      plotly_IMAGE(
        draw_plot(
          dataset=d(),
          kpi=input$dat,
          metric=input$metric,
          type=input$type,
          entity=input$entity,
          breakdown=input$breakdown,
          annual=input$annual,
          top=input$top,
          fontsize=input$fontsize,
          years=input$year
        ),
        width=1600, height=1000, format="png", out_file=file
      )
    }
  )
}
