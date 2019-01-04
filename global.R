library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(here)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)

source("plot_ATFM.R", local=T)
source("plot_ATFM_APT.R", local=T)
source("plot_ADDITIONAL.R", local=T)
source("plot_PREDEP.R", local=T)

Sys.setenv("plotly_username"="rob.sawyer")
Sys.setenv("plotly_api_key"="HSQo0QjxFICKCIsnCPqW")
'%!in%' <- function(x,y){!('%in%'(x,y))}

# Import data
path <- paste0("~/graph_tool/data/")
dat <- list()
dat$ASMA <- fread(paste0(path,"ASMA.csv"), encoding="UTF-8")
dat$ATFM <- fread(paste0(path,"ATFM.csv"), encoding="UTF-8")
dat$ATFM_APT <- fread(paste0(path,"ATFM_APT.csv"), encoding="UTF-8")
dat$PREDEP <- fread(paste0(path,"PREDEP.csv"), encoding="UTF-8")
dat$TAXI <- fread(paste0(path,"TAXI.csv"), encoding="UTF-8")
dat$ASMA_ANNUAL <- fread(paste0(path,"ASMA_ANNUAL.csv"), encoding="UTF-8")
dat$ATFM_ANNUAL <- fread(paste0(path,"ATFM_ANNUAL.csv"), encoding="UTF-8")
dat$ATFM_APT_ANNUAL <- fread(paste0(path,"ATFM_APT_ANNUAL.csv"), encoding="UTF-8")
dat$PREDEP_ANNUAL <- fread(paste0(path,"PREDEP_ANNUAL.csv"), encoding="UTF-8")
dat$TAXI_ANNUAL <- fread(paste0(path,"TAXI_ANNUAL.csv"), encoding="UTF-8")
dat$STATE_FAB <- fread(paste0(path,"STATE_FAB.csv"), encoding="UTF-8")

# Date ordered factors
monthsfull <- c("January","Feburary","March","April","May","June","July","August","September","October","November","December") %>% factor(., levels=., ordered=T)
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") %>% factor(., levels=., ordered=T)
years <- seq(2011,2018,1) %>% factor(., levels=., ordered=T)
monthsyears <- as.vector(outer(months, years, FUN="paste")) %>% factor(., levels=., ordered=T)

# Delay Targets

SES_target <- data.frame(
  YEAR = c(2015, 2016, 2017, 2018),
  TARGET = 0.5
)
FABEC_target <- data.frame(
  YEAR = c(2015, 2016, 2017, 2018),
  TARGET = c(0.48,0.49,0.48,0.47)
)
Germany_target <- data.frame(
  YEAR = c(2015, 2016, 2017, 2018),
  TARGET = c(0.35,0.34,0.32,0.31)
)

# Option choices
KPIs <- c(
  "En-Route ATFM Delay",
  "Airport Arrival AFTM Delay",
  "ASMA Additional Time",
  "Taxi-Out Additional Time",
  "ATC Pre-Departure Delay",
  "En-Route vs Airport ATFM"
)
ATFM_metrics <- c(
  "Delays per Flight",
  "Delays per Flight (Yearly)",
  "Delays per Flight (Month)",
  "Delay Ranking (Yearly)",
  "Delay Ranking (Month)",
  "No. of Monthly Delays",
  "Percentage Monthly Delays",
  "No. of Monthly Delays (15min+)",
  "Percentage Monthly Delays (15min+)"
)
ATFM_APT_metrics <- c(
  "Delays per Flight",
  "Delays per Flight (Yearly)",
  "Delays per Flight (Month)",
  "Delay Ranking (Yearly)",
  "Delay Ranking (Month)"
)
APT_metrics <- c(
  "Average Monthly Delays",
  "Average Monthly Delays (Yearly)",
  "Average Monthly Delays (Month)"
)
PREDEP_metrics <- c(
  "Average Monthly Delays (APT)",
  "Average Monthly Delays (AL)",
  "Delays per IFR Dep. (APT)",
  "Delays per IFR Dep. (AL)"
)