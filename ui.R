title <- "IATA Capacity Study"

header <- dashboardHeader(title = title)

sidebar <- dashboardSidebar(
  collapsed = F,
  div(
    class = "sidebar-wrapper",
    uiOutput("option_kpi"),
    uiOutput("option_metric"),
    uiOutput("option_1"),
    uiOutput("option_2"),
    uiOutput("option_year"),
    uiOutput("option_month"),
    uiOutput("option_ranking"),
    uiOutput("option_breakdown"),
    uiOutput("option_annual")
  ),
  div(class = "think")
)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(class = "plot-wrapper", plotlyOutput("plot", height="100%")),
  div(
    class = "topbar-wrapper",
    div(style = "float:right; padding:10px 5px;", title = "Daily limit: 100", downloadButton("download", "Download")),
    div(style = "float:right; padding:10px 5px;", numericInput("exporty", NULL, value=768, min=100, max=2160, step=100, width="75px")),
    div(style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;", "Height (px)"),
    div(style = "float:right; padding:10px 5px;", numericInput("exportx", NULL, value=1280, min=100, max=4096, step=100, width="75px")),
    div(style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;", "Width (px)"),
    div(style = "float:right; padding:10px 5px;", numericInput("fontsize", NULL, value=16, min=5, max=36, step=1, width="75px")),
    div(style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;", "Font size")
  ),
  div(class = "sidebar-toggle-border")
)

ui <- dashboardPage(skin = "black", title = title, header, sidebar, body)
