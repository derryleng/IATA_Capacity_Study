library(shiny)
library(shinyWidgets)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)

setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))

paths <- list.files(paste0(getwd(),"/data/"), pattern="*.csv", full.names=T)
datnames <- c("ASMA","ATFM_APT","ATFM_APT_postops","ATFM_AUA","ATFM_AUA_postops","ATFM_FIR","ATFM_FIR_postops","PREDEP","TAXI")
dat <- lapply(paths, function(x) fread(x, encoding="UTF-8")); names(dat) <- datnames; rm(paths, datnames)

months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
monthsordered <- factor(months, levels=months, ordered=T)
for (i in 1:length(unique(dat$TAXI$YEAR))) {
  if (i == 1) {
    years <- paste(unique(dat$TAXI$YEAR)[i], monthsordered)
  } else {
    years <- c(years, paste(unique(dat$TAXI$YEAR)[i], monthsordered))
  }
}
yearsordered <- factor(years, levels=years, ordered=T)

dat$TAXI$FLT_TXO_UNIMP_2 <- as.numeric(dat$TAXI$FLT_TXO_UNIMP_2)
dat$TAXI$TIME_TXO_UNIMP_2 <- as.numeric(dat$TAXI$TIME_TXO_UNIMP_2)
dat$TAXI$TIME_TXO_ADD_2 <- as.numeric(dat$TAXI$TIME_TXO_ADD_2)

dat$TAXI$TAXI_ADD_PFL <- dat$TAXI$TIME_TXO_ADD_2/dat$TAXI$FLT_TXO_UNIMP_2