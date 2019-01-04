header <- dashboardHeader(title = "IATA Capacity Study")

sidebar <- dashboardSidebar(
  collapsed = T,
  div(
    style = "position:absolute; left:0; top:50px; height:auto; width:100%; z-index:1000;",
    uiOutput("option_kpi"),
    uiOutput("option_metric"),
    uiOutput("option_type"),
    uiOutput("option_entity1"),
    uiOutput("option_entity2"),
    uiOutput("option_year"),
    uiOutput("option_month"),
    uiOutput("option_ranking"),
    uiOutput("option_grouping")
  ),
  div(style = "position:absolute; left:0; bottom:0; height:80px; width:100%; z-index:999;
    background-image: url('Think_FullLogoStrapline_Pink.svg');
    background-repeat: no-repeat;
    background-position: center;
    background-size: 113.4px 56.6px;")
)

body <- dashboardBody(
  tags$head(tags$style(HTML("
    html {
        -webkit-touch-callout: none;
        -webkit-user-select: none;
        -khtml-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
    }
    .shiny-output-error {
      	visibility:hidden;
    }
    .shiny-output-error:before {
      	visibility:hidden;
    }
    .main-header {
        border: 1px solid black;
    }
    .content-wrapper {
        background-color: #FFFFFF;
    }
  "))),
  div(
    style = "position:absolute; height:auto; width:auto; left:15px; right:15px; top:70px; bottom:15px;",
    plotlyOutput("plot", height="100%")
  ),
  div(
    style = "position:absolute; left:275px; right:0; top:0; height:50px; z-index:100000;",
    div(style = "float:right; padding:10px 5px;", downloadButton("download", "Download")),
    div(style = "float:right; padding:10px 5px;", numericInput("exporty", NULL, value=1000, min=100, max=10000, step=100, width="100%")),
    div(style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;", "Height (px)"),
    div(style = "float:right; padding:10px 5px;", numericInput("exportx", NULL, value=1600, min=100, max=10000, step=100, width="100%")),
    div(style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;", "Width (px)"),
    div(style = "float:right; padding:10px 5px;", numericInput("fontsize", NULL, value=12, min=1, max=20, step=1, width="100%")),
    div(style = "float:right; padding:15px 5px; font-weight:bold; text-align:center;", "Font size")
  ),
  div(
    style = "position:absolute; left:230px; top:0px; height:52px; width:45px; z-index:100000; border:1px solid black; pointer-events:none; background:none;"
  )
)

ui <- dashboardPage(skin = "black", header, sidebar, body)
