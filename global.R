library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)

source("plot_ATFM.R", local=T)
source("plot_ATFM_APT.R", local=T)
source("plot_ASMA.R", local=T)
source("plot_TAXI.R", local=T)
source("plot_PREDEP.R", local=T)
source("plot_ASMA_TAXI_PREDEP.R", local=T)
source("plot_ATFM_BOTH.R", local=T)
source("plot_TRAFFIC_FORECAST.R", local=T)

Sys.setenv("plotly_username"="rob.sawyer")
Sys.setenv("plotly_api_key"="HSQo0QjxFICKCIsnCPqW")
# Sys.setenv("plotly_username"="derry.leng")
# Sys.setenv("plotly_api_key"="dxWgmpWjP3vkfcTQDZje")

'%!in%' <- function(x,y){!('%in%'(x,y))}

# Import data
path <- paste0(here::here(), "/data/")
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
dat$TRAFFIC_FORECAST <- fread(paste0(path,"TRAFFIC_FORECAST.csv"), encoding="UTF-8")

# Date ordered factors
monthsfull <- c("January","Feburary","March","April","May","June","July","August","September","October","November","December") %>% factor(., levels=., ordered=T)
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") %>% factor(., levels=., ordered=T)
years_range <- seq(2011,2018,1) %>% factor(., levels=., ordered=T)
years_range_extended <- seq(2011,2021,1) %>% factor(., levels=., ordered=T)
monthsyears <- as.vector(outer(months, years_range, FUN="paste")) %>% factor(., levels=., ordered=T)

# Get metrics list w/ properties
metrics_list <- fread(paste0(here::here(),"/metrics_list.csv"), encoding="UTF-8")

# For differentiation between states and FAB
ATFM_APT_FAB <- c("Baltic FAB", "BLUE MED FAB", "DANUBE FAB", "DK-SE FAB", "FAB CE (SES RP2)", "FABEC", "NEFAB", "SW FAB", "UK-Ireland FAB")

# ATFM Delay Categories
ATFM_DELAY_CATEGORIES <- c(
  "A - Accident/Incident",
  "C - ATC Capacity",
  "D - De-icing",
  "E - Equipment (Non-ATC)",
  "G - Aerodrome Capacity",
  "I - Industrial Action (ATC)",
  "M - Airspace Management",
  "N - Industrial Action (Non-ATC)",
  "O - Other",
  "P - Special Event",
  "R - ATC Routeing",
  "S - ATC Staffing",
  "T - Equipment (ATC)",
  "V - Environmental Issues",
  "W - Weather",
  "NA - Not Specified"
)

EU28_States <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Italy", "Ireland", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "The Netherlands",
  "Poland", "Portugal", "Romania", "Spain", "Slovakia", "Slovenia", "Sweden", "United Kingdom"
)

SES_States <- c(
  EU28_States, "Norway", "Switzerland"
)