lib_req <- c("data.table", "magrittr", "plyr", "rstudioapi")

library(data.table)
library(magrittr)
library(plyr)

project_path <- "C:\\Dropbox (Think Research)\\Projects\\IATA Capacity Study\\0. Resources\\"

# ATFM Delay Enroute
ATFM_AUA <- fread(paste0(project_path, "ATFM Delay Enroute\\En-Route_ATFM_Delay_AUA.csv"), encoding = "UTF-8")
#ATFM_AUA_postops <- fread(paste0(project_path, "ATFM Delay Enroute\\En-Route_ATFM_Delay_AUA_post_ops.csv"), encoding = "UTF-8")
ATFM_FIR <- fread(paste0(project_path, "ATFM Delay Enroute\\En-Route_ATFM_Delay_FIR.csv"), encoding = "UTF-8")
#ATFM_FIR_postops <- fread(paste0(project_path, "ATFM Delay Enroute\\En-Route_ATFM_Delay_FIR_post_ops.csv"), encoding = "UTF-8")
ATFM <- rbind(ATFM_AUA, ATFM_FIR)

# ATFM Delay Airport
ATFM_APT <- fread(paste0(project_path, "ATFM Delay Airport\\Airport_Arrival_ATFM_Delay.csv"), encoding = "UTF-8")
#ATFM_APT_postops <- fread(paste0(project_path, "ATFM Delay Airport\\Airport_Arrival_ATFM_Delay_post_ops.csv"), encoding = "UTF-8")

# ASMA Delay
ASMA <- fread(paste0(project_path, "ASMA Delay\\ASMA_Additional_Time.csv"), encoding = "UTF-8")

# Taxi-out Additional Time
TAXI <- fread(paste0(project_path, "Taxi-Out Additional Time\\Taxi-Out_Additional_Time.csv"), encoding = "UTF-8")

# ATC Pre-departure Delay
PREDEP <- fread(paste0(project_path, "ATC Pre-Departure Delay\\ATC_Pre-Departure_Delay.csv"), encoding = "UTF-8")

# Append data tables to list
#datnames <- c("ATFM_AUA","ATFM_AUA_postops","ATFM_FIR","ATFM_FIR_postops","ATFM_APT","ATFM_APT_postops","ASMA","TAXI","PREDEP")
datnames <- c("ATFM","ATFM_APT","ASMA","TAXI","PREDEP")
dat <- lapply(datnames, function(x) eval(parse(text=x))); names(dat) <- datnames

# Remove bad values and clean numeric columns
for (file in names(dat)) {
  for (col in names(dat[[file]])) {
    set(dat[[file]], i=which(dat[[file]][[col]] %in% c("","N/A","NULL","NA","<NA>"," ")), j=col, value=NA)
    dat[[file]][[col]] <- gsub(",","",dat[[file]][[col]])
    y <- dat[[file]][[col]] %>% .[!is.na(.)]
    sample <- ifelse(length(y) > 0, y[sample(seq(1,length(y)),1)], NA)
    if (grepl("^[0-9]{1,}$", sample)) {
      suppressWarnings(dat[[file]][[col]] <- as.numeric(dat[[file]][[col]]))
    }
  }
}

# Minor data corrections
dat$ATFM_APT$STATE_NAME[dat$ATFM_APT$STATE_NAME == "The former Yugoslav Republic of Macedonia"] <- "FYR Macedonia"
dat$ATFM_APT$STATE_NAME[dat$ATFM_APT$STATE_NAME == "Bosnia and Herzegovina"] <- "Bosnia and Herzegovina"
dat$PREDEP$STATE_NAME[dat$PREDEP$STATE_NAME == "The former Yugoslav Republic of Macedonia"] <- "FYR Macedonia"
dat$PREDEP$STATE_NAME[dat$PREDEP$STATE_NAME == "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"

# ATFM --------------------------------------------------------------------
ATFM_numcols <- c(
  "FLT_ERT_1","DLY_ERT_1","DLY_ERT_A_1","DLY_ERT_C_1","DLY_ERT_D_1","DLY_ERT_E_1",
  "DLY_ERT_G_1","DLY_ERT_I_1","DLY_ERT_M_1","DLY_ERT_N_1","DLY_ERT_O_1","DLY_ERT_P_1",
  "DLY_ERT_R_1","DLY_ERT_S_1","DLY_ERT_T_1","DLY_ERT_V_1","DLY_ERT_W_1","DLY_ERT_NA_1",
  "FLT_ERT_1_DLY","FLT_ERT_1_DLY_15"
)
# Aggregate data to monthly
for (i in 1:length(ATFM_numcols)) {
  if (i == 1) {
    temp <- aggregate(eval(parse(text=ATFM_numcols[i]))~YEAR+MONTH_MON+ENTITY_NAME+ENTITY_TYPE,data=dat$ATFM,sum)
    names(temp) <- c("YEAR","MONTH_MON","ENTITY_NAME","ENTITY_TYPE",ATFM_numcols[1])
  } else {
    temp <- merge(temp, aggregate(eval(parse(text=ATFM_numcols[i]))~YEAR+MONTH_MON+ENTITY_NAME+ENTITY_TYPE,data=dat$ATFM,sum), all.x=T)
    names(temp) <- c("YEAR","MONTH_MON","ENTITY_NAME","ENTITY_TYPE",ATFM_numcols[1:i])
  }
}
# Create aggregate statistics for each type
temp_agg <- list()
for (type in unique(dat$ATFM$ENTITY_TYPE)) {
  for (i in 1:length(ATFM_numcols)) {
    if (i == 1) {
      temp_agg[[type]] <- aggregate(eval(parse(text=ATFM_numcols[i]))~YEAR+MONTH_MON+ENTITY_TYPE,data=subset(dat$ATFM,ENTITY_TYPE %in% type),sum)
      names(temp_agg[[type]]) <- c("YEAR","MONTH_MON","ENTITY_TYPE",ATFM_numcols[1])
    } else {
      temp_agg[[type]] <- merge(temp_agg[[type]], aggregate(eval(parse(text=ATFM_numcols[i]))~YEAR+MONTH_MON+ENTITY_TYPE,data=subset(dat$ATFM,ENTITY_TYPE %in% type),sum), all.x=T)
      names(temp_agg[[type]]) <- c("YEAR","MONTH_MON","ENTITY_TYPE",ATFM_numcols[1:i])
    }
  }
  temp_agg[[type]]$ENTITY_NAME <- paste("All", type)
}
for (i in temp_agg) {
  temp <- rbind(temp, i)
}
# Create average columns
for (col in ATFM_numcols[-c(1,19,20)]) {
  temp[[paste0(col,"_AVG")]] <- ifelse(temp$FLT_ERT_1 == 0, NA, temp[[col]]/temp$FLT_ERT_1)
}
names(temp) <- c(
  "YEAR","MONTH","NAME","TYPE","FLIGHTS_TOTAL",
  "DELAY","A","C","D","E","G","I","M","N","O","P","R","S","T","V","W","NA","FLIGHTS_DELAYED","FLIGHTS_DELAYED_15",
  "DELAY_AVG","A_AVG","C_AVG","D_AVG","E_AVG","G_AVG","I_AVG","M_AVG","N_AVG",
  "O_AVG","P_AVG","R_AVG","S_AVG","T_AVG","V_AVG","W_AVG","NA_AVG"
)
dat$ATFM <- temp

# ATFM_APT ----------------------------------------------------------------
ATFM_APT_numcols <- c(
  "FLT_ARR_1","DLY_APT_ARR_1","DLY_APT_ARR_A_1","DLY_APT_ARR_C_1","DLY_APT_ARR_D_1",
  "DLY_APT_ARR_E_1","DLY_APT_ARR_G_1","DLY_APT_ARR_I_1","DLY_APT_ARR_M_1","DLY_APT_ARR_N_1",
  "DLY_APT_ARR_O_1","DLY_APT_ARR_P_1","DLY_APT_ARR_R_1","DLY_APT_ARR_S_1","DLY_APT_ARR_T_1",
  "DLY_APT_ARR_V_1","DLY_APT_ARR_W_1","DLY_APT_ARR_NA_1"
)
# Aggregate data to monthly
for (i in 1:length(ATFM_APT_numcols)) {
  if (i == 1) {
    temp <- aggregate(eval(parse(text=ATFM_APT_numcols[i]))~YEAR+MONTH_MON+APT_ICAO+APT_NAME+STATE_NAME,data=dat$ATFM_APT,sum)
    names(temp) <- c("YEAR","MONTH_MON","APT_ICAO","APT_NAME","STATE_NAME",ATFM_APT_numcols[1])
  } else {
    temp <- merge(temp, aggregate(eval(parse(text=ATFM_APT_numcols[i]))~YEAR+MONTH_MON+APT_ICAO+APT_NAME+STATE_NAME,data=dat$ATFM_APT,sum), all.x=T)
    names(temp) <- c("YEAR","MONTH_MON","APT_ICAO","APT_NAME","STATE_NAME",ATFM_APT_numcols[1:i])
  }
}
# Create aggregate statistics for each type
temp_agg <- list()
for (state in unique(dat$ATFM_APT$STATE_NAME)) {
  for (i in 1:length(ATFM_APT_numcols)) {
    if (i == 1) {
      temp_agg[[state]] <- aggregate(eval(parse(text=ATFM_APT_numcols[i]))~YEAR+MONTH_MON+STATE_NAME,data=subset(dat$ATFM_APT,STATE_NAME %in% state),sum)
      names(temp_agg[[state]]) <- c("YEAR","MONTH_MON","STATE_NAME",ATFM_APT_numcols[1])
    } else {
      temp_agg[[state]] <- merge(temp_agg[[state]], aggregate(eval(parse(text=ATFM_APT_numcols[i]))~YEAR+MONTH_MON+STATE_NAME,data=subset(dat$ATFM_APT,STATE_NAME %in% state),sum), all.x=T)
      names(temp_agg[[state]]) <- c("YEAR","MONTH_MON","STATE_NAME",ATFM_APT_numcols[1:i])
    }
  }
  temp_agg[[state]]$APT_ICAO <- NA
  temp_agg[[state]]$APT_NAME <- paste("All", state)
}
for (i in temp_agg) {
  temp <- rbind(temp, i)
}
# Create average columns
for (col in ATFM_APT_numcols[-c(1)]) {
  temp[[paste0(col,"_AVG")]] <- ifelse(temp$FLT_ARR_1 == 0, NA, temp[[col]]/temp$FLT_ARR_1)
}
names(temp) <- c(
  "YEAR","MONTH","ICAO","NAME","STATE","FLIGHTS_TOTAL",
  "DELAY","A","C","D","E","G","I","M","N","O","P","R","S","T","V","W","NA",
  "DELAY_AVG","A_AVG","C_AVG","D_AVG","E_AVG","G_AVG","I_AVG","M_AVG","N_AVG",
  "O_AVG","P_AVG","R_AVG","S_AVG","T_AVG","V_AVG","W_AVG","NA_AVG"
)
temp$LABEL <- paste0(temp$NAME," (",temp$ICAO,") ")
dat$ATFM_APT <- temp

such_temp <- ddply(dat$ATFM_APT, .(YEAR,MONTH), numcolwise(sum, na.rm=T))
such_temp$ICAO <- "NA"
such_temp$NAME <- "Europe"
such_temp$STATE <- "All Countries"
such_temp$LABEL <- "NA"
such_temp <- such_temp[,c(1,2,38,39,40,seq(3,37,1),41)]
dat$ATFM_APT <- rbind(dat$ATFM_APT, such_temp)

# ASMA --------------------------------------------------------------------
dat$ASMA <- subset(dat$ASMA, select=-c(MONTH_NUM))
names(dat$ASMA) <- c("YEAR","MONTH","ICAO","NAME","STATE","ASMA_RADIUS","FLIGHTS_UNIMPEDED","TIME_REF","TIME_ADD","LABEL")

# TAXI --------------------------------------------------------------------
dat$TAXI <- subset(dat$TAXI, select=-c(MONTH_NUM))
names(dat$TAXI) <- c("YEAR","MONTH","ICAO","NAME","STATE","FLIGHTS_UNIMPEDED","TIME_REF","TIME_ADD")
dat$TAXI$LABEL <- paste0(dat$TAXI$NAME," (",dat$TAXI$ICAO,") ")

# PREDEP ------------------------------------------------------------------
PREDEP_numcols <- c(
  "FLT_DEP_1", "FLT_DEP_IFR_2", "DLY_ATC_PRE_2", "FLT_DEP_3", "DLY_ATC_PRE_3"
)
# Aggregate data to monthly
for (i in 1:length(PREDEP_numcols)) {
  if (i == 1) {
    temp <- aggregate(eval(parse(text=PREDEP_numcols[i]))~YEAR+MONTH_MON+APT_ICAO+APT_NAME+STATE_NAME,data=dat$PREDEP,sum)
    names(temp) <- c("YEAR","MONTH_MON","APT_ICAO","APT_NAME","STATE_NAME",PREDEP_numcols[1])
  } else {
    temp <- merge(temp, aggregate(eval(parse(text=PREDEP_numcols[i]))~YEAR+MONTH_MON+APT_ICAO+APT_NAME+STATE_NAME,data=dat$PREDEP,sum), all.x=T)
    names(temp) <- c("YEAR","MONTH_MON","APT_ICAO","APT_NAME","STATE_NAME",PREDEP_numcols[1:i])
  }
}
names(temp) <- c(
  "YEAR", "MONTH", "ICAO", "NAME", "STATE", "FLIGHTS_NM", "FLIGHTS_APT", "DELAY_APT", "FLIGHTS_AL", "DELAY_AL"
)
temp$LABEL <- paste0(temp$NAME," (",temp$ICAO,") ")
dat$PREDEP <- temp

# Annual ------------------------------------------------------------------
dat$ATFM_ANNUAL <- ddply(dat$ATFM[,1:24], .(YEAR,NAME,TYPE), numcolwise(sum, na.rm=T))
for (col in names(dat$ATFM_ANNUAL[,-c(1,2,3,4)])) {
  dat$ATFM_ANNUAL[[paste0(col,"_AVG")]] <- ifelse(dat$ATFM_ANNUAL$FLIGHTS_TOTAL == 0, NA, dat$ATFM_ANNUAL[[col]]/dat$ATFM_ANNUAL$FLIGHTS_TOTAL)
}

dat$ATFM_APT_ANNUAL <- ddply(dat$ATFM_APT[,1:23], .(YEAR,ICAO,NAME,STATE), numcolwise(sum, na.rm=T))
for (col in names(dat$ATFM_APT_ANNUAL[,-c(1,2,3,4,5)])) {
  dat$ATFM_APT_ANNUAL[[paste0(col,"_AVG")]] <- ifelse(dat$ATFM_APT_ANNUAL$FLIGHTS_TOTAL == 0, NA, dat$ATFM_APT_ANNUAL[[col]]/dat$ATFM_APT_ANNUAL$FLIGHTS_TOTAL)
}

dat$ASMA_ANNUAL <-  ddply(dat$ASMA, .(YEAR,ICAO,NAME,STATE,ASMA_RADIUS,LABEL), numcolwise(sum, na.rm=T))
dat$TAXI_ANNUAL <- ddply(dat$TAXI, .(YEAR,ICAO,NAME,STATE,LABEL), numcolwise(sum, na.rm=T))
dat$PREDEP_ANNUAL <- ddply(dat$PREDEP, .(YEAR,ICAO,NAME,STATE,LABEL), numcolwise(sum, na.rm=T))

# Save to CSV -------------------------------------------------------------
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))
for (x in names(dat)) {
  con <- file(paste0(getwd(),"/data/",x,".csv"), encoding="UTF-8")
  write.csv(dat[[x]], file=con, row.names=F)
}
closeAllConnections()
