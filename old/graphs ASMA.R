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
for (i in 1:length(unique(dat$ASMA$YEAR))) {
  if (i == 1) {
    years <- paste(unique(dat$ASMA$YEAR)[i], monthsordered)
  } else {
    years <- c(years, paste(unique(dat$ASMA$YEAR)[i], monthsordered))
  }
}
yearsordered <- factor(years, levels=years, ordered=T)

dat$ASMA$FLT_ASMA_UNIMP_2 <- as.numeric(dat$ASMA$FLT_ASMA_UNIMP_2)
dat$ASMA$TIME_ASMA_UNIMP_2 <- as.numeric(dat$ASMA$TIME_ASMA_UNIMP_2)
dat$ASMA$TIME_ASMA_ADD_2 <- as.numeric(dat$ASMA$TIME_ASMA_ADD_2)

dat$ASMA$ASMA_ADD_PFL <- dat$ASMA$TIME_ASMA_ADD_2/dat$ASMA$FLT_ASMA_UNIMP_2

d <- dat$ASMA

d1 <- aggregate(FLT_ASMA_UNIMP_2~YEAR+MONTH_MON,data=d,sum)
d2 <- aggregate(TIME_ASMA_ADD_2~YEAR+MONTH_MON,data=d,sum)
d.agg <- merge(d1,d2)
d.agg$ASMA_ADD_PFL <- d.agg$TIME_ASMA_ADD_2/d.agg$FLT_ASMA_UNIMP_2

title <- "Total ASMA Delay All Airports"
filename <- "ASMA_DELAY_TOTAL_ALL.html"
g <- plot_ly(data=d.agg) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~TIME_ASMA_ADD_2, type="bar", marker=list(color="rgb(213,16,103)")) %>%
  layout(
    barmode="stack",
    title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="Additional ASMA Time (mins)")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)
filename <- "ASMA_DELAY_TOTAL_ALL_V2.html"
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

title <- "Average ASMA Delay per IFR Flight All Airports"
filename <- "ASMA_DELAY_PER_FLIGHT_ALL.html"
g <- plot_ly(data=d.agg) %>%
  add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~ASMA_ADD_PFL, type="bar", marker=list(color="rgb(213,16,103)")) %>%
  layout(
    barmode="stack",
    title=title, legend=list(x=100, y=0.5),
    xaxis=list(title="Date", tickangle=90, autotick=F),
    yaxis=list(title="Additional ASMA Time per Flight (mins)")
  )
htmlwidgets::saveWidget(as_widget(g), filename, title=title)
filename <- "ASMA_DELAY_PER_FLIGHT_ALL_V2.html"
g1 <- plot_ly()
for (j in 1:length(unique(d.agg$YEAR))) {
  g1 <- g1 %>%
    add_trace(
      data=subset(d.agg, YEAR %in% unique(d.agg$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=monthsordered)),
      x=~factor(MONTH_MON, levels=monthsordered),
      y=~ASMA_ADD_PFL,
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

# for (i in unique(d$APT_ICAO)) {
#   title <- paste(i, "Total ASMA Delay")
#   filename <- paste0("ASMA_DELAY_TOTAL_",i,".html")
#   g <- plot_ly(data=subset(d, APT_ICAO %in% i)) %>%
#     add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~TIME_ASMA_ADD_2, type="bar", marker=list(color="rgb(213,16,103)")) %>%
#     layout(
#       barmode="stack",
#       title=title, legend=list(x=100, y=0.5),
#       xaxis=list(title="Date", tickangle=90, autotick=F),
#       yaxis=list(title="Additional ASMA Time (mins)")
#     )
#   htmlwidgets::saveWidget(as_widget(g), filename, title=title)
#   filename <- paste0("ASMA_DELAY_TOTAL_",i,"_V2.html")
#   g1 <- plot_ly()
#   for (j in 1:length(unlist(unique(subset(d, APT_ICAO %in% i,select=YEAR))))) {
#     g1 <- g1 %>%
#       add_trace(
#         data=subset(d, APT_ICAO %in% i & YEAR %in% unlist(unique(subset(d, APT_ICAO %in% i,select=YEAR)))[j]) %>% arrange(factor(MONTH_MON, levels=monthsordered)),
#         x=~factor(MONTH_MON, levels=monthsordered),
#         y=~TIME_ASMA_ADD_2,
#         name=unique(d.agg$YEAR)[j],
#         type="scatter",
#         mode="lines",
#         line=list(color=rev(brewer.pal(length(unique(d$YEAR)), "RdYlGn"))[j])
#       )
#   }
#   g1 <- g1 %>% layout(
#     title=title, legend=list(x=100, y=0.5),
#     xaxis=list(title="Date", tickangle=90, autotick=F),
#     yaxis=list(title="Additional ASMA Time (mins)")
#   )
#   htmlwidgets::saveWidget(as_widget(g1), filename, title=title)
#   
#   title <- paste(i, "Average ASMA Delay per IFR Flight")
#   filename <- paste0("ASMA_DELAY_PER_FLIGHT_",i,".html")
#   g <- plot_ly(data=subset(d, APT_ICAO %in% i)) %>%
#     add_trace(x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered), y=~ASMA_ADD_PFL, type="bar", marker=list(color="rgb(213,16,103)")) %>%
#     layout(
#       barmode="stack",
#       title=title, legend=list(x=100, y=0.5),
#       xaxis=list(title="Date", tickangle=90, autotick=F),
#       yaxis=list(title="Additional ASMA Time per Flight (mins)")
#     )
#   htmlwidgets::saveWidget(as_widget(g), filename, title=title)
#   filename <- paste0("ASMA_DELAY_PER_FLIGHT_",i,"_V2.html")
#   g1 <- plot_ly()
#   for (j in 1:length(unlist(unique(subset(d, APT_ICAO %in% i,select=YEAR))))) {
#     g1 <- g1 %>%
#       add_trace(
#         data=subset(d, APT_ICAO %in% i & YEAR %in% unlist(unique(subset(d, APT_ICAO %in% i,select=YEAR)))[j]) %>% arrange(factor(MONTH_MON, levels=monthsordered)),
#         x=~factor(MONTH_MON, levels=monthsordered),
#         y=~ASMA_ADD_PFL,
#         name=unique(d.agg$YEAR)[j],
#         type="scatter",
#         mode="lines",
#         line=list(color=rev(brewer.pal(length(unique(d$YEAR)), "RdYlGn"))[j])
#       )
#   }
#   g1 <- g1 %>% layout(
#     title=title, legend=list(x=100, y=0.5),
#     xaxis=list(title="Date", tickangle=90, autotick=F),
#     yaxis=list(title="Additional ASMA Time (mins)")
#   )
#   htmlwidgets::saveWidget(as_widget(g1), filename, title=title)
# }
