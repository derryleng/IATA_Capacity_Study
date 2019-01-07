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

Sys.setenv("plotly_username"="rob.sawyer")
Sys.setenv("plotly_api_key"="HSQo0QjxFICKCIsnCPqW")
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

# Date ordered factors
monthsfull <- c("January","Feburary","March","April","May","June","July","August","September","October","November","December") %>% factor(., levels=., ordered=T)
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") %>% factor(., levels=., ordered=T)
years_range <- seq(2011,2018,1) %>% factor(., levels=., ordered=T)
monthsyears <- as.vector(outer(months, years_range, FUN="paste")) %>% factor(., levels=., ordered=T)

# Option choices
KPIs <- c(
  "En-Route ATFM Delay",
  "Airport Arrival AFTM Delay",
  "ASMA Additional Time",
  "Taxi-Out Additional Time",
  "ATC Pre-Departure Delay",
  "En-Route vs Airport ATFM"
)

metrics_ATFM <- c(
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
metrics_ATFM_APT <- c(
  "Delays per Flight",
  "Delays per Flight (Yearly)",
  "Delays per Flight (Month)",
  "Airport Delay Ranking (Yearly)",
  "Airport Delay Ranking (Month)",
  "FAB Delay Ranking (Yearly)",
  "FAB Delay Ranking (Month)"
)
metrics_APT <- c(
  "Average Monthly Delays",
  "Average Monthly Delays (Yearly)",
  "Average Monthly Delays (Month)"
)
metrics_PREDEP <- list(
  "APT Total Monthly Delays",
  "AL Total Monthly Delays",
  "APT Average Monthly Delays",
  "AL Average Monthly Delays",
  "APT Average Monthly Delays (Yearly)",
  "AL Average Monthly Delays (Yearly)",
  "APT Average Monthly Delays (Month)",
  "AL Average Monthly Delays (Month)"
)
metrics_ATFM_BOTH <- list(
  "Average Monthly Delays (Yearly)",
  "Average Monthly Delays (Month)"
)

choices_ATFM_TYPE <- sort(unique(dat$ATFM$TYPE))
choices_ATFM_ANSP <- sort(unique(dat$ATFM[TYPE %in% "ANSP (AUA)"]$NAME))
choices_ATFM_AREA1 <- sort(unique(dat$ATFM[TYPE %in% "AREA (AUA)"]$NAME %>% .[. %!in% "All AREA (AUA)"]))
choices_ATFM_AREA2 <- sort(unique(dat$ATFM[TYPE %in% "AREA (FIR)"]$NAME %>% .[. %!in% "All AREA (FIR)"]))
choices_ATFM_STATE <- sort(unique(dat$ATFM[dat$ATFM$TYPE == "COUNTRY (FIR)"]$NAME))
choices_ATFM_FAB <- sort(unique(dat$ATFM[dat$ATFM$TYPE == "FAB (FIR)"]$NAME))

choices_ATFM_APT_STATE <- sort(unique(dat$ATFM_APT$STATE))
ATFM_APT_FAB <- c("Baltic FAB", "BLUE MED FAB", "DK-SE FAB", "FAB CE (SES RP2)", "FABEC", "NEFAB", "SW FAB", "UK-Ireland FAB")

choices_ASMA_STATE <- sort(unique(dat$ASMA$STATE))

choices_TAXI_STATE <- sort(unique(dat$TAXI$STATE))

choices_PREDEP_STATE <- sort(unique(dat$PREDEP$STATE))

choices_BOTH_STATE <- unique(subset(dat$ATFM_APT_ANNUAL, grepl("^All *", NAME) | grepl("^NA$", NAME), select=c(STATE)))

metrics_ranking <- c("Delay Ranking (Yearly)", "Delay Ranking (Month)", "Airport Delay Ranking (Yearly)", "Airport Delay Ranking (Month)", "FAB Delay Ranking (Yearly)", "FAB Delay Ranking (Month)")
metrics_month <- c("Delays per Flight (Month)","Delay Ranking (Month)", "Airport Delay Ranking (Month)", "FAB Delay Ranking (Month)", "Average Monthly Delays (Month)", "APT Average Monthly Delays (Month)", "AL Average Monthly Delays (Month)")
