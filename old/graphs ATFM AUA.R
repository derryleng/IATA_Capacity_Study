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
for (i in 1:length(unique(dat$ATFM_AUA$YEAR))) {
  if (i == 1) {
    years <- paste(unique(dat$ATFM_AUA$YEAR)[i], monthsordered)
  } else {
    years <- c(years, paste(unique(dat$ATFM_AUA$YEAR)[i], monthsordered))
  }
}
yearsordered <- factor(years, levels=years, ordered=T)

dat$ATFM_AUA$FLT_ERT_1 <- as.numeric(dat$ATFM_AUA$FLT_ERT_1)
dat$ATFM_AUA$DLY_ERT_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_1)
dat$ATFM_AUA$DLY_ERT_A_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_A_1)
dat$ATFM_AUA$DLY_ERT_C_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_C_1)
dat$ATFM_AUA$DLY_ERT_D_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_D_1)
dat$ATFM_AUA$DLY_ERT_E_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_E_1)
dat$ATFM_AUA$DLY_ERT_G_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_G_1)
dat$ATFM_AUA$DLY_ERT_I_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_I_1)
dat$ATFM_AUA$DLY_ERT_M_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_M_1)
dat$ATFM_AUA$DLY_ERT_N_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_N_1)
dat$ATFM_AUA$DLY_ERT_O_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_O_1)
dat$ATFM_AUA$DLY_ERT_P_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_P_1)
dat$ATFM_AUA$DLY_ERT_R_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_R_1)
dat$ATFM_AUA$DLY_ERT_S_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_S_1)
dat$ATFM_AUA$DLY_ERT_T_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_T_1)
dat$ATFM_AUA$DLY_ERT_V_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_V_1)
dat$ATFM_AUA$DLY_ERT_W_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_W_1)
dat$ATFM_AUA$DLY_ERT_NA_1 <- as.numeric(dat$ATFM_AUA$DLY_ERT_NA_1)

# En-route ATFM Delayed Flights -------------------------------------------

title <- "Monthly En-route ATFM Delayed Flights All ANSPs"
filename <- "ATFM_AUA_DELAYED_FLIGHTS_ANSPs.html"
g <- plot_ly(data=aggregate(FLT_ERT_1_DLY~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)) %>%
  add_trace(
    x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
    y=~FLT_ERT_1_DLY,
    type="bar",
    marker=list(color="rgb(213,16,103)")
  ) %>%
  layout(
    hovermode="compare",
     title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="No. of Delayed Flights")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)
filename <- "ATFM_AUA_DELAYED_FLIGHTS_ANSPs_V2.html"
g1 <- plot_ly()
for (j in 1:length(unique(d.agg$YEAR))) {
  g1 <- g1 %>%
    add_trace(
      data=subset(d.agg, YEAR %in% unique(d.agg$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=monthsordered)),
      x=~factor(MONTH_MON, levels=monthsordered),
      y=~TIME_ASMA_ADD_2,
      name=unique(d.agg$YEAR)[j],
      type="scatter",
      mode="lines",
      line=list(color=rev(brewer.pal(length(unique(d.agg$YEAR)), "RdYlGn"))[j])
    )
}
g1 <- g1 %>% layout(
  title=title, legend=list(x=100, y=0.5),
  xaxis=list(title="Date", tickangle=90, autotick=F),
  yaxis=list(title="Additional ASMA Time (mins)")
)
htmlwidgets::saveWidget(as_widget(g1), filename, title=title)


title <- "Monthly En-route ATFM Delayed Flights All ANSPs (>15 min.)"
filename <- "ATFM_AUA_DELAYED_FLIGHTS_15_ANSPs.html"
g <- plot_ly(data=aggregate(FLT_ERT_1_DLY_15~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)) %>%
  add_trace(
    x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
    y=~FLT_ERT_1_DLY_15,
    type="bar",
    marker=list(color="rgb(213,16,103)")
  ) %>%
  layout(
    hovermode="compare",
     title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="No. of Delayed Flights")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)

title <- "Monthly En-route ATFM Delayed Flights % All ANSPs"
filename <- "ATFM_AUA_DELAYED_FLIGHTS_PERCENT_ANSPs.html"
d1 <- aggregate(FLT_ERT_1_DLY~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)
d2 <- aggregate(FLT_ERT_1~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)
d <- merge(d1, d2)
d$agg <- d$FLT_ERT_1_DLY/d$FLT_ERT_1
g <- plot_ly(data=d) %>%
  add_trace(
    x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
    y=~agg,
    type="bar",
    marker=list(color="rgb(213,16,103)")
  ) %>%
  layout(
    hovermode="compare",
     title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="Percentage of Delayed Flights")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)

title <- "Monthly En-route ATFM Delayed Flights % All ANSPs (>15 min.)"
filename <- "ATFM_AUA_DELAYED_FLIGHTS_PERCENT_15_ANSPs.html"
d1 <- aggregate(FLT_ERT_1_DLY_15~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)
d2 <- aggregate(FLT_ERT_1~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)
d <- merge(d1, d2)
d$agg <- d$FLT_ERT_1_DLY_15/d$FLT_ERT_1
g <- plot_ly(data=d) %>%
  add_trace(
    x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
    y=~agg,
    type="bar",
    marker=list(color="rgb(213,16,103)")
  ) %>%
  layout(
    hovermode="compare",
     title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="Percentage of Delayed Flights")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)

entities <- tail(unique(dat$ATFM_AUA$ENTITY_NAME[order(dat$ATFM_AUA$FLT_ERT_1)]), 10)
for (i in entities) {
  title <- paste(i, "Monthly En-route ATFM Delayed Flights")
  filename <- paste0("ATFM_AUA_DELAYED_FLIGHTS_",i,".html")
  g <- plot_ly(data=aggregate(FLT_ERT_1_DLY~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_NAME %in% i),sum)) %>%
    add_trace(
      x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
      y=~FLT_ERT_1_DLY,
      type="bar",
      marker=list(color="rgb(213,16,103)")
    ) %>%
    layout(
      hovermode="compare",
       title=title, legend=list(x=100, y=0.5),
      xaxis=list(title="Date", tickangle=90, autotick=F),
      yaxis=list(title="No. of Delayed Flights")
    )
  htmlwidgets::saveWidget(as_widget(g), filename, title=title)
  
  title <- paste(i, "Monthly En-route ATFM Delayed Flights (>15 min.)")
  filename <- paste0("ATFM_AUA_DELAYED_FLIGHTS_15_",i,".html")
  g <- plot_ly(data=aggregate(FLT_ERT_1_DLY_15~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_NAME %in% i),sum)) %>%
    add_trace(
      x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
      y=~FLT_ERT_1_DLY_15,
      type="bar",
      marker=list(color="rgb(213,16,103)")
    ) %>%
    layout(
      hovermode="compare",
       title=title, legend=list(x=100, y=0.5),
      xaxis=list(title="Date", tickangle=90, autotick=F),
      yaxis=list(title="No. of Delayed Flights")
    )
  htmlwidgets::saveWidget(as_widget(g), filename, title=title)
  
  title <- paste(i, "Monthly En-route ATFM Delayed Flights %")
  filename <- paste0("ATFM_AUA_DELAYED_FLIGHTS_PERCENT_",i,".html")
  d1 <- aggregate(FLT_ERT_1_DLY~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_NAME %in% i),sum)
  d2 <- aggregate(FLT_ERT_1~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_NAME %in% i),sum)
  d <- merge(d1, d2)
  d$agg <- d$FLT_ERT_1_DLY/d$FLT_ERT_1
  g <- plot_ly(data=d) %>%
    add_trace(
      x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
      y=~agg,
      type="bar",
      marker=list(color="rgb(213,16,103)")
    ) %>%
    layout(
      hovermode="compare",
       title=title, legend=list(x=100, y=0.5),
      xaxis=list(title="Date", tickangle=90, autotick=F),
      yaxis=list(title="Percentage of Delayed Flights")
    )
  htmlwidgets::saveWidget(as_widget(g), filename, title=title)
  
  title <- paste(i, "Monthly En-route ATFM Delayed Flights % (>15 min.)")
  filename <- paste0("ATFM_AUA_DELAYED_FLIGHTS_PERCENT_15_",i,".html")
  d1 <- aggregate(FLT_ERT_1_DLY_15~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_NAME %in% i),sum)
  d2 <- aggregate(FLT_ERT_1~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_NAME %in% i),sum)
  d <- merge(d1, d2)
  d$agg <- d$FLT_ERT_1_DLY_15/d$FLT_ERT_1
  g <- plot_ly(data=d) %>%
    add_trace(
      x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
      y=~agg,
      type="bar",
      marker=list(color="rgb(213,16,103)")
    ) %>%
    layout(
      hovermode="compare",
       title=title, legend=list(x=100, y=0.5),
      xaxis=list(title="Date", tickangle=90, autotick=F),
      yaxis=list(title="Percentage of Delayed Flights")
    )
  htmlwidgets::saveWidget(as_widget(g), filename, title=title)
}

# En-route AFTM Delays per Flight -----------------------------------------

colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))

delay_cols <- c("FLT_ERT_1","DLY_ERT_1","DLY_ERT_A_1","DLY_ERT_C_1","DLY_ERT_D_1","DLY_ERT_E_1",
                "DLY_ERT_G_1","DLY_ERT_I_1","DLY_ERT_M_1","DLY_ERT_N_1","DLY_ERT_O_1",
                "DLY_ERT_P_1","DLY_ERT_R_1","DLY_ERT_S_1","DLY_ERT_T_1","DLY_ERT_V_1",
                "DLY_ERT_W_1","DLY_ERT_NA_1")

for (i in 1:length(delay_cols)) {
  if (i == 1) {
    d <- aggregate(eval(parse(text=delay_cols[i]))~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum)
    names(d) <- c("YEAR","MONTH_MON",delay_cols[1])
  } else {
    d <- merge(d, aggregate(eval(parse(text=delay_cols[i]))~YEAR+MONTH_MON,data=subset(dat$ATFM_AUA, ENTITY_TYPE %in% "ANSP (AUA)"),sum), all.x=T)
    names(d) <- c("YEAR","MONTH_MON",delay_cols[1:i])
  }
}
d$DLY_ERT_1_PFL <- d$DLY_ERT_1/d$FLT_ERT_1
d$DLY_ERT_A_1_PFL <- d$DLY_ERT_A_1/d$FLT_ERT_1
d$DLY_ERT_C_1_PFL <- d$DLY_ERT_C_1/d$FLT_ERT_1
d$DLY_ERT_D_1_PFL <- d$DLY_ERT_D_1/d$FLT_ERT_1
d$DLY_ERT_E_1_PFL <- d$DLY_ERT_E_1/d$FLT_ERT_1
d$DLY_ERT_G_1_PFL <- d$DLY_ERT_G_1/d$FLT_ERT_1
d$DLY_ERT_I_1_PFL <- d$DLY_ERT_I_1/d$FLT_ERT_1
d$DLY_ERT_M_1_PFL <- d$DLY_ERT_M_1/d$FLT_ERT_1
d$DLY_ERT_N_1_PFL <- d$DLY_ERT_N_1/d$FLT_ERT_1
d$DLY_ERT_O_1_PFL <- d$DLY_ERT_O_1/d$FLT_ERT_1
d$DLY_ERT_P_1_PFL <- d$DLY_ERT_P_1/d$FLT_ERT_1
d$DLY_ERT_R_1_PFL <- d$DLY_ERT_R_1/d$FLT_ERT_1
d$DLY_ERT_S_1_PFL <- d$DLY_ERT_S_1/d$FLT_ERT_1
d$DLY_ERT_T_1_PFL <- d$DLY_ERT_T_1/d$FLT_ERT_1
d$DLY_ERT_V_1_PFL <- d$DLY_ERT_V_1/d$FLT_ERT_1
d$DLY_ERT_W_1_PFL <- d$DLY_ERT_W_1/d$FLT_ERT_1
d$DLY_ERT_NA_1_PFL <- d$DLY_ERT_NA_1/d$FLT_ERT_1
title <- "All ANSPs En-route ATFM Delayed Flights"
filename <- "ATFM_AUA_DELAY_PER_FLIGHTS_ANSPs.html"
g <- plot_ly(data=d) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_A_1_PFL, name="Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_C_1_PFL, name="ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_D_1_PFL, name="De-icing", type="bar", marker=list(color=colour[3])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_E_1_PFL, name="Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_G_1_PFL, name="Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_I_1_PFL, name="Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_M_1_PFL, name="Airspace Management", type="bar", marker=list(color=colour[7])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_N_1_PFL, name="Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_O_1_PFL, name="Other", type="bar", marker=list(color=colour[9])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_P_1_PFL, name="Special Event", type="bar", marker=list(color=colour[10])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_R_1_PFL, name="ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_S_1_PFL, name="ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_T_1_PFL, name="Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_V_1_PFL, name="Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_W_1_PFL, name="Weather", type="bar", marker=list(color=colour[15])) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_NA_1_PFL, name="Not Specified", type="bar", marker=list(color=colour[16])) %>%
  layout(
    barmode="stack",
     title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="En-route ATFM Delay per Flight (minutes)")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)

for (i in 1:length(delay_cols)) {
  if (i == 1) {
    d <- aggregate(eval(parse(text=delay_cols[i]))~YEAR+MONTH_MON+ENTITY_NAME,data=dat$ATFM_AUA,sum)
    names(d) <- c("YEAR","MONTH_MON","ENTITY_NAME",delay_cols[1])
  } else {
    d <- merge(d, aggregate(eval(parse(text=delay_cols[i]))~YEAR+MONTH_MON+ENTITY_NAME,data=dat$ATFM_AUA,sum), all.x=T)
    names(d) <- c("YEAR","MONTH_MON","ENTITY_NAME",delay_cols[1:i])
  }
}
d$DLY_ERT_1_PFL <- d$DLY_ERT_1/d$FLT_ERT_1
d$DLY_ERT_A_1_PFL <- d$DLY_ERT_A_1/d$FLT_ERT_1
d$DLY_ERT_C_1_PFL <- d$DLY_ERT_C_1/d$FLT_ERT_1
d$DLY_ERT_D_1_PFL <- d$DLY_ERT_D_1/d$FLT_ERT_1
d$DLY_ERT_E_1_PFL <- d$DLY_ERT_E_1/d$FLT_ERT_1
d$DLY_ERT_G_1_PFL <- d$DLY_ERT_G_1/d$FLT_ERT_1
d$DLY_ERT_I_1_PFL <- d$DLY_ERT_I_1/d$FLT_ERT_1
d$DLY_ERT_M_1_PFL <- d$DLY_ERT_M_1/d$FLT_ERT_1
d$DLY_ERT_N_1_PFL <- d$DLY_ERT_N_1/d$FLT_ERT_1
d$DLY_ERT_O_1_PFL <- d$DLY_ERT_O_1/d$FLT_ERT_1
d$DLY_ERT_P_1_PFL <- d$DLY_ERT_P_1/d$FLT_ERT_1
d$DLY_ERT_R_1_PFL <- d$DLY_ERT_R_1/d$FLT_ERT_1
d$DLY_ERT_S_1_PFL <- d$DLY_ERT_S_1/d$FLT_ERT_1
d$DLY_ERT_T_1_PFL <- d$DLY_ERT_T_1/d$FLT_ERT_1
d$DLY_ERT_V_1_PFL <- d$DLY_ERT_V_1/d$FLT_ERT_1
d$DLY_ERT_W_1_PFL <- d$DLY_ERT_W_1/d$FLT_ERT_1
d$DLY_ERT_NA_1_PFL <- d$DLY_ERT_NA_1/d$FLT_ERT_1
entities <- tail(unique(dat$ATFM_AUA$ENTITY_NAME[order(dat$ATFM_AUA$FLT_ERT_1)]), 10)
for (i in entities) {
  title <- paste(i, "En-route ATFM Delayed Flights")
  filename <- paste0("ATFM_AUA_DELAY_PER_FLIGHTS_",i,".html")
  g <- plot_ly(data=subset(d, ENTITY_NAME %in% i)) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_A_1_PFL, name="Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_C_1_PFL, name="ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_D_1_PFL, name="De-icing", type="bar", marker=list(color=colour[3])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_E_1_PFL, name="Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_G_1_PFL, name="Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_I_1_PFL, name="Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_M_1_PFL, name="Airspace Management", type="bar", marker=list(color=colour[7])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_N_1_PFL, name="Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_O_1_PFL, name="Other", type="bar", marker=list(color=colour[9])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_P_1_PFL, name="Special Event", type="bar", marker=list(color=colour[10])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_R_1_PFL, name="ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_S_1_PFL, name="ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_T_1_PFL, name="Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_V_1_PFL, name="Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_W_1_PFL, name="Weather", type="bar", marker=list(color=colour[15])) %>%
    add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~DLY_ERT_NA_1_PFL, name="Not Specified", type="bar", marker=list(color=colour[16])) %>%
    layout(
      barmode="stack",
       title=title, legend=list(x=100, y=0.5),
      xaxis=list(title="Date", tickangle=90, autotick=F),
      yaxis=list(title="En-route ATFM Delay per Flight (minutes)")
    )
  htmlwidgets::saveWidget(as_widget(g), filename, title=title)
}
