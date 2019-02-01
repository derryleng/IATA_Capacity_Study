page_title <- "IATA Capacity Study"

header <- dashboardHeader(title = page_title)

sidebar <- dashboardSidebar(
  collapsed = F,
  div(
    class = "sidebar-wrapper scrollbar",
    id = "style-2",
    uiOutput("option_kpi"),
    uiOutput("option_metric"),
    uiOutput("option_1"),
    uiOutput("option_2"),
    uiOutput("option_year"),
    uiOutput("option_rankingcategories"),
    uiOutput("option_month"),
    uiOutput("option_breakdowncategories"),
    uiOutput("option_ranking"),
    uiOutput("option_breakdown"),
    uiOutput("option_annual"),
    uiOutput("option_annualtargets"),
    uiOutput("option_totalflights"),
    uiOutput("option_legend"),
    uiOutput("option_toggletitle"),
    uiOutput("option_barmode"),
    uiOutput("option_togglepredep")
  ),
  div(class = "think")
)

body <- dashboardBody(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  
  plotlyOutput("plot", width="100%"),
  
  div(
    class = "topbar-wrapper",
    div(
      style = "float:right; padding:10px 5px;",
      title = "Daily limit: 100",
      downloadButton("download", "Download")
    ),
    div(
      style = "float:right; padding:10px 5px;",
      title = "Specify height of downloaded plot (Recommended: 768)",
      numericInput("exporty", NULL, value=768, min=100, max=2160, step=1, width="75px")
    ),
    div(
      style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;",
      "Height (px)"
    ),
    div(
      style = "float:right; padding:10px 5px;",
      title = "Specify width of downloaded plot (Recommended: 1280)",
      numericInput("exportx", NULL, value=1280, min=100, max=4096, step=1, width="75px")
    ),
    div(
      style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;",
      "Width (px)"
    ),
    div(
      style = "float:right; padding:10px 5px;",
      title = "Font size 20 is recommended for 1280x768 images",
      numericInput("fontsize", NULL, value=15, min=5, max=36, step=1, width="75px")
    ),
    div(
      style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;",
      "Font size"
    )
  ),
  
  div(class = "sidebar-toggle-border")
)

ui <- dashboardPage(skin = "black", title = page_title, header, sidebar, body)
