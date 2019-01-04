ui <- fillPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    #tags$script(src = "script.js")
  ),
  
  div(class="plot",plotlyOutput("plot", height="100%")),
  
  div(class="overlay"),
  
  absolutePanel(
    div(
      class="controlpanel",
      pickerInput("dat", "Select KPI", choices = KPIs, width="200px"),
      uiOutput("options1"),
      uiOutput("options2"),
      uiOutput("options3"),
      uiOutput("options4"),
      uiOutput("options5")
    ),
    draggable=T,
    top=15,
    left=15
  ),
  
  absolutePanel(
    div(
      class="controlpanel",
      numericInput("fontsize", "Plot Font Size", value=12, min=1, max=20, step=1, width="100%"),
      div(style="font-weight: bold; text-align: center;", "Download Options"),
      fluidRow(
        column(width=6,  style="left:5px; padding:3%;", numericInput("exportx", "Width (px)", value=1600, min=100, max=10000, step=100, width="100%")),
        column(width=6,  style="right:5px; padding:3%;", numericInput("exporty", "Height (px)", value=1000, min=100, max=10000, step=100, width="100%"))
      ),
      div(
        title="Save entire plot as png file (Daily Limit 100)",
        style="display: table; margin: 0 auto;",
        downloadButton("download", "Download")
      )
    ),
    draggable=T,
    bottom=15,
    right=15
  )
  
)
