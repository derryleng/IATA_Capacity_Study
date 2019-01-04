library(shiny)
library(shinyWidgets)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)

Sys.setenv("plotly_username"="rob.sawyer")
Sys.setenv("plotly_api_key"="HSQo0QjxFICKCIsnCPqW")

# Import data
dat <- list()
dat$ASMA <- fread("~/graph_tool/data/ASMA.csv", encoding="UTF-8")
dat$ATFM <- fread("~/graph_tool/data/ATFM.csv", encoding="UTF-8")
dat$ATFM_APT <- fread("~/graph_tool/data/ATFM_APT.csv", encoding="UTF-8")
dat$PREDEP <- fread("~/graph_tool/data/PREDEP.csv", encoding="UTF-8")
dat$TAXI <- fread("~/graph_tool/data/TAXI.csv", encoding="UTF-8")
dat$ASMA_ANNUAL <- fread("~/graph_tool/data/ASMA_ANNUAL.csv", encoding="UTF-8")
dat$ATFM_ANNUAL <- fread("~/graph_tool/data/ATFM_ANNUAL.csv", encoding="UTF-8")
dat$ATFM_APT_ANNUAL <- fread("~/graph_tool/data/ATFM_APT_ANNUAL.csv", encoding="UTF-8")
dat$PREDEP_ANNUAL <- fread("~/graph_tool/data/PREDEP_ANNUAL.csv", encoding="UTF-8")
dat$TAXI_ANNUAL <- fread("~/graph_tool/data/TAXI_ANNUAL.csv", encoding="UTF-8")

# Date ordered factors
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") %>% factor(., levels=., ordered=T)
years <- seq(2011,2018,1) %>% factor(., levels=., ordered=T)
monthsyears <- as.vector(outer(months, years, FUN="paste")) %>% factor(., levels=., ordered=T)

# Goodies
'%!in%' <- function(x,y){!('%in%'(x,y))}
KPIs <- c(
  "En-Route ATFM Delay",
  "Airport Arrival AFTM Delay",
  "ASMA Additional Time",
  "Taxi-Out Additional Time",
  "ATC Pre-Departure Delay"
)
ATFM_metrics <- c(
  "Delays per Flight",
  "Delays per Flight (Yearly)",
  "Delays per Flight (July)",
  "Delays per Flight (August)",
  "Delay by Centre",
  "July Delay by Centre",
  "August Delay by Centre",
  "No. of Monthly Delays",
  "Percentage Monthly Delays",
  "No. of Monthly Delays (15min+)",
  "Percentage Monthly Delays (15min+)"
)
ATFM_APT_metrics <- c(
  ATFM_metrics[1:4],
  "Delay by Airport",
  "July Delay by Airport",
  "August Delay by Airport"
)
APT_metrics <- c(
  "Total Monthly Delays",
  "Total Monthly Delays (Yearly)",
  "Total Monthly Delays (July)",
  "Total Monthly Delays (August)"
)
PREDEP_metrics <- c(
  "Total Monthly Delays (APT)",
  "Total Monthly Delays (AL)",
  "Delays per IFR Dep. (APT)",
  "Delays per IFR Dep. (AL)"
)
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

# Plotting function
draw_plot <- function(dataset, kpi, metric, type, entity, breakdown=T, annual=F, top=10, fontsize=1, years) {
  g <- plot_ly(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)))
  
  if (kpi == "En-Route ATFM Delay") {
    
    if (metric == "Delays per Flight") {
      
      title <- paste("En-Route ATFM Delay per Flight for", entity)
      ytitle <- "En-Route ATFM Delay per Flight (min.)"
      if (breakdown == T & annual == T) {
        xtitle <- "Year"
        colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
        g <- g %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
          add_lines(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      } else if (breakdown == F & annual == T) {
        xtitle <- "Year"
        g <- g %>%
          add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
          add_lines(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      } else if (breakdown == T & annual == F) {
        xtitle <- "Date"
        colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
        g <- g %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
          add_lines(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      } else if (breakdown == F & annual == F) {
        xtitle <- "Date"
        g <- g %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
          add_lines(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      }
      
    } else if (metric == "Delays per Flight (Yearly)") {
      
      title <- paste("En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
      ytitle <- "En-Route ATFM Delay per Flight (min.)"
      xtitle <- "Month"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years)$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
            x=~factor(MONTH, levels=months),
            y=~DELAY_AVG,
            name=uniqueyears[i],
            type="scatter",
            mode="lines",
            line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3)
          )
      }
      g <- g %>% layout(hovermode="compare")
      
    } else if (metric == "Delays per Flight (July)") {
      
      title <- paste("July En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
      ytitle <- "En-Route ATFM Delay per Flight (min.)"
      xtitle <- "Year"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% "JUL")$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% "JUL"),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
      
    } else if (metric == "Delays per Flight (August)") {
      
      title <- paste("August En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
      ytitle <- "En-Route ATFM Delay per Flight (min.)"
      xtitle <- "Year"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% "AUG")$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% "AUG"),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
      
    } else if (metric == "No. of Monthly Delays") {
      
      title <- paste("Monthly En-Route ATFM Delayed Flights for", entity)
      ytitle <- "No. of Delayed Flights"
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~FLIGHTS_DELAYED,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        ) %>% layout(hovermode="compare")
      
    } else if (metric == "Percentage Monthly Delays") {
      
      title <- paste("Monthly En-Route ATFM % Delayed Flights for", entity)
      ytitle <- "Delayed Flights (% of Total Flights within Airspace)"
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~FLIGHTS_DELAYED/FLIGHTS_TOTAL,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        ) %>% layout(hovermode="compare")
      
    } else if (metric == "No. of Monthly Delays (15min+)") {
      
      title <- paste("Monthly En-Route ATFM Delayed Flights (15min+) for", entity)
      ytitle <- "No. of 15min+ Delayed Flights"
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~FLIGHTS_DELAYED_15,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        ) %>% layout(hovermode="compare")
      
    } else if (metric == "Percentage Monthly Delays (15min+)") {
      
      title <- paste("Monthly En-Route ATFM % Delayed Flights (15min+) for", entity)
      ytitle <- "15min+ Delayed Flights (% of Total Flights within Airspace)"
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~FLIGHTS_DELAYED_15/FLIGHTS_TOTAL,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        ) %>% layout(hovermode="compare")
      
    } else if (metric == "Delay by Centre") {
      
      title <- "Yearly Delay by Centre"
      ytitle <- "Total Delay (min.)"
      xtitle <- "Centre"
      temp <- subset(dat$ATFM_ANNUAL, TYPE %in% type & NAME %!in% paste("All",type) & !is.na(DELAY) & YEAR %in% years) %>% .[rev(order(DELAY))]
      g <- g %>%
        add_trace(
          data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~DELAY,
          color=~factor(YEAR, levels=years),
          colors="Spectral",
          type="bar"
        ) %>% layout(barmode="group")
      
    } else if (metric == "July Delay by Centre") {
      
      title <- "July Delay by Centre"
      ytitle <- "Total Delay (min.)"
      xtitle <- "Centre"
      temp <- subset(dat$ATFM, MONTH %in% "JUL" & TYPE %in% type & NAME %!in% paste("All",type) & !is.na(DELAY) & YEAR %in% years) %>% .[rev(order(DELAY))]
      g <- g %>%
        add_trace(
          data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~DELAY,
          color=~factor(YEAR, levels=years),
          colors="Spectral",
          type="bar"
        ) %>% layout(barmode="group")
      
    } else if (metric == "August Delay by Centre") {
      
      title <- "August Delay by Centre"
      ytitle <- "Total Delay (min.)"
      xtitle <- "Centre"
      temp <- subset(dat$ATFM, MONTH %in% "AUG" & TYPE %in% type & NAME %!in% paste("All",type) & !is.na(DELAY) & YEAR %in% years) %>% .[rev(order(DELAY))]
      g <- g %>%
        add_trace(
          data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~DELAY,
          color=~factor(YEAR, levels=years),
          colors="Spectral",
          type="bar"
        ) %>% layout(barmode="group")
      
    }
    
    # RP2 ER ATFM Delay Targets
    if (annual == T) {
      if (entity %in% c("SES Area (RP1)", "SES Area (RP2)")) {
        g <- g %>% add_lines(data=subset(SES_target, YEAR %in% years), x=~YEAR, y=~TARGET, name="RP2 Delay Target", line=list(color="rgb(0,0,250)", width=3))
      } else if (entity %in% c("FABEC")) {
        g <- g %>% add_lines(data=subset(FABEC_target, YEAR %in% years), x=~YEAR, y=~TARGET, name="RP2 Delay Target", line=list(color="rgb(0,0,250)", width=3))
      } else if (entity %in% c("Germany")) {
        g <- g %>% add_lines(data=subset(Germany_target, YEAR %in% years), x=~YEAR, y=~TARGET, name="RP2 Delay Target", line=list(color="rgb(0,0,250)", width=3))
      }
    }
    
  } else if (kpi == "Airport Arrival AFTM Delay") {
    
    if (metric == "Delays per Flight") {
      title <- paste("Airport Arrivals ATFM Delay per Flight for", entity)
      ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
      
      if (breakdown == T & annual == T) {
        xtitle <- "Year"
        colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
        g <- g %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
          add_lines(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      } else if (breakdown == F & annual == T) {
        xtitle <- "Year"
        g <- g %>%
          add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~DELAY_AVG, name="Delays per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
          add_lines(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      } else if (breakdown == T & annual == F) {
        xtitle <- "Date"
        colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
        g <- g %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
          add_lines(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      } else if (breakdown == F & annual == F) {
        xtitle <- "Date"
        g <- g %>%
          add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~DELAY_AVG, name="Delays per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
          add_lines(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
      }
      
    } else if (metric == "Delays per Flight (Yearly)") {
      title <- paste("Airport Arrivals ATFM Delay per Flight for", entity, "Yearly Trends")
      ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
      xtitle <- "Month"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years)$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
            x=~factor(MONTH, levels=months),
            y=~DELAY_AVG,
            name=uniqueyears[i],
            type="scatter",
            mode="lines",
            line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3)
          )
      }
      g <- g %>% layout(hovermode="compare")
    } else if (metric == "Delays per Flight (July)") {
      title <- paste("July Airport Arrivals ATFM Delay per Flight for", entity, "Yearly Trends")
      ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
      xtitle <- "Year"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% "JUL")$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% "JUL"),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    } else if (metric == "Delays per Flight (August)") {
      title <- paste("August Airport Arrivals ATFM Delay per Flight for", entity, "Yearly Trends")
      ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
      xtitle <- "Year"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% "AUG")$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% "AUG"),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    } else if (metric == "Delay by Airport") {
      
      title <- "Yearly Delay by Airport"
      ytitle <- "Total Delay (min.)"
      xtitle <- "Airport"
      temp <- subset(dat$ATFM_APT_ANNUAL, NAME %!in% paste("All", c("Countries",unique(dat$ATFM_APT$STATE))) & !is.na(DELAY) & YEAR %in% years) %>% .[rev(order(DELAY))]
      g <- g %>%
        add_trace(
          data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~DELAY,
          color=~factor(YEAR, levels=years),
          colors="Spectral",
          type="bar"
        ) %>% layout(barmode="group")
      
    } else if (metric == "July Delay by Airport") {
      
      title <- "July Delay by Airport"
      ytitle <- "Total Delay (min.)"
      xtitle <- "Airport"
      temp <- subset(dat$ATFM_APT, NAME %!in% paste("All", c("Countries",unique(dat$ATFM_APT$STATE))) & MONTH %in% "JUL" & !is.na(DELAY) & YEAR %in% years) %>% .[rev(order(DELAY))]
      g <- g %>%
        add_trace(
          data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~DELAY,
          color=~factor(YEAR, levels=years),
          colors="Spectral",
          type="bar"
        ) %>% layout(barmode="group")
      
    } else if (metric == "August Delay by Airport") {
      
      title <- "August Delay by Airport"
      ytitle <- "Total Delay (min.)"
      xtitle <- "Airport"
      temp <- subset(dat$ATFM_APT, NAME %!in% paste("All", c("Countries",unique(dat$ATFM_APT$STATE))) & MONTH %in% "AUG" & !is.na(DELAY) & YEAR %in% years) %>% .[rev(order(DELAY))]
      g <- g %>%
        add_trace(
          data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~DELAY,
          color=~factor(YEAR, levels=years),
          colors="Spectral",
          type="bar"
        ) %>% layout(barmode="group")
      
    }
    
  } else if (kpi %in% c("ASMA Additional Time","Taxi-Out Additional Time")) {
    
    if (metric == "Total Monthly Delays") {
      title <- paste(kpi, "for", entity)
      ytitle <- paste(kpi, "(min.)")
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~TIME_ADD,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        )
    } else if (metric == "Total Monthly Delays (Yearly)") {
      title <- paste(kpi, "for", entity, "Yearly Trends")
      ytitle <- paste(kpi, "(min.)")
      xtitle <- "Month"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years)$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
            x=~factor(MONTH, levels=months),
            y=~TIME_ADD,
            name=uniqueyears[i],
            type="scatter",
            mode="lines",
            line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3)
          )
      }
      g <- g %>% layout(hovermode="compare")
    } else if (metric == "Total Monthly Delays (July)") {
      title <- paste("July", kpi, "for", entity, "Yearly Trends")
      ytitle <- paste(kpi, "(min.)")
      xtitle <- "Year"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% "JUL")$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% "JUL"),
            x=~YEAR,
            y=~TIME_ADD,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    } else if (metric == "Total Monthly Delays (August)") {
      title <- paste("August", kpi, "for", entity, "Yearly Trends")
      ytitle <- paste(kpi, "(min.)")
      xtitle <- "Year"
      uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years& MONTH %in% "AUG")$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% "AUG"),
            x=~YEAR,
            y=~TIME_ADD,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    }
    
  } else if (kpi == "ATC Pre-Departure Delay") {
    
    if (metric == "Total Monthly Delays (APT)") {
      title <- paste(kpi, "(Reported by Airport Operators) for", entity)
      ytitle <- paste(kpi, "(min.)")
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_APT,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        )
    } else if (metric == "Total Monthly Delays (AL)") {
      title <- paste(kpi, "(Reported by CODA/Airlines) for", entity)
      ytitle <- paste(kpi, "(min.)")
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_APT/FLIGHTS_APT,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        )
    } else if (metric == "Delays per IFR Dep. (APT)") {
      title <- paste(kpi, "per IFR Departure (Reported by Airport Operators) for", entity)
      ytitle <- paste(kpi, "per IFR Departure (min.)")
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_AL,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        )
    } else if (metric == "Delays per IFR Dep. (AL)") {
      title <- paste(kpi, "per IFR Departure (Reported by CODA/Airlines) for", entity)
      ytitle <- paste(kpi, "per IFR Departure (min.)")
      xtitle <- "Date"
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_AL/FLIGHTS_AL,
          type="scatter",
          mode="lines",
          line=list(color="rgb(213,16,103)", width=3)
        )
    }
    
  }
  
  g <- g %>%
    layout(
      title=title,
      legend=list(x=1.04,y=0.5),
      font=list(size=fontsize),
      xaxis=list(title=xtitle, linewidth=1, showgrid=F, tickangle=90, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}
