library(data.table)
library(dplyr)
library(qualV)

'%!in%' <- function(x,y){!('%in%'(x,y))}

# Import data
path <- paste0("~/IATA_Capacity_Study/data/")
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

EU28_States <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Italy", "Ireland", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Spain", "Slovakia", "Slovenia", "Sweden", "United Kingdom"
)
SES_States <- c(
  EU28_States, "Norway", "Switzerland"
)

RP2 <- c(2015,2016,2017,2018)

SES_Annual_Total_Delays <- data.table(Year = RP2)
SES_Annual_Total_Delays$ATFM_DELAY <- aggregate(data=subset(dat$ATFM_ANNUAL, NAME %in% SES_States & YEAR %in% RP2), DELAY~YEAR, "sum")$DELAY
SES_Annual_Total_Delays$ATFM_APT_DELAY <- aggregate(data=subset(dat$ATFM_APT_ANNUAL, STATE %in% SES_States & YEAR %in% RP2), DELAY~YEAR, "sum")$DELAY
SES_Annual_Total_Delays$ASMA_DELAY <- aggregate(data=subset(dat$ASMA_ANNUAL, NAME %in% SES_States & YEAR %in% RP2), TIME_ADD~YEAR, "sum")$TIME_ADD
SES_Annual_Total_Delays$TAXI_DELAY <- aggregate(data=subset(dat$TAXI_ANNUAL, NAME %in% SES_States & YEAR %in% RP2), TIME_ADD~YEAR, "sum")$TIME_ADD
SES_Annual_Total_Delays$PREDEP_DELAY <- aggregate(data=subset(dat$PREDEP_ANNUAL, NAME %in% SES_States & YEAR %in% RP2), DELAY_AL~YEAR, "sum")$DELAY_AL

fwrite(SES_Annual_Total_Delays, file="SES_Annual_Total_Delays.csv")

SES_Annual_Total_Delays_Missing_States <- data.table(Year = RP2)
SES_Annual_Total_Delays_Missing_States$ATFM_DELAY <- unlist(lapply(RP2, function(x) {paste(setdiff(SES_States, subset(dat$ATFM_ANNUAL, NAME %in% SES_States & YEAR %in% x)$NAME), collapse=", ")}))
SES_Annual_Total_Delays_Missing_States$ATFM_APT_DELAY <- unlist(lapply(RP2, function(x) {paste(setdiff(SES_States, subset(dat$ATFM_APT_ANNUAL, STATE %in% SES_States & YEAR %in% x)$STATE), collapse=", ")}))
SES_Annual_Total_Delays_Missing_States$ASMA_DELAY <- unlist(lapply(RP2, function(x) {paste(setdiff(SES_States, subset(dat$ASMA_ANNUAL, STATE %in% SES_States & YEAR %in% RP2)$STATE), collapse=", ")}))
SES_Annual_Total_Delays_Missing_States$TAXI_DELAY <- unlist(lapply(RP2, function(x) {paste(setdiff(SES_States, subset(dat$TAXI_ANNUAL, STATE %in% SES_States & YEAR %in% RP2)$STATE), collapse=", ")}))
SES_Annual_Total_Delays_Missing_States$PREDEP_DELAY <- unlist(lapply(RP2, function(x) {paste(setdiff(SES_States, subset(dat$PREDEP_ANNUAL, STATE %in% SES_States & YEAR %in% RP2)$STATE), collapse=", ")}))

fwrite(SES_Annual_Total_Delays_Missing_States, file="SES_Annual_Total_Delays_Missing_States.csv")
