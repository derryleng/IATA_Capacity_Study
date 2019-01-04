plot_ATFM_APT <- function(dataset, metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly()
  
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
        add_lines(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years[years != 2018]) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
    } else if (breakdown == F & annual == T) {
      xtitle <- "Year"
      g <- g %>%
        add_trace(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~DELAY_AVG, name="Delays per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_lines(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years[years != 2018]) %>% arrange(factor(YEAR, levels=years)), x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
    } else if (breakdown == T & annual == F) {
      xtitle <- "Date"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- g %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
        add_lines(data=subset(dataset, NAME %in% entity & YEAR %in% years[years != 2018]) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack", yaxis2=list(overlaying="y", side="right", title="Total Flights", linewidth=1, showgrid=F))
    } else if (breakdown == F & annual == F) {
      xtitle <- "Date"
      g <- g %>%
        add_trace(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~DELAY_AVG, name="Delays per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_lines(data=subset(dataset, NAME %in% entity & YEAR %in% years[years != 2018]) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
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
  } else if (metric == "Delays per Flight (Month)") {
    title <- paste(month, "Airport Arrivals ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
    xtitle <- "Year"
    uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
          x=~YEAR,
          y=~DELAY_AVG,
          name=uniqueyears[i],
          marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
          type="bar",
          showlegend=F
        )
    }
  } else if (metric == "Delay Ranking (Yearly)") {
    
    title <- "Yearly Average Delay Ranking"
    ytitle <- "Average Delay (min.)"
    xtitle <- "Airport"
    temp <- subset(dat$ATFM_APT_ANNUAL, NAME %!in% c("Europe",paste("All", c("Countries",unique(dat$ATFM_APT$STATE)))) & !is.na(DELAY_AVG) & YEAR %in% years) %>%
      .[rev(order(YEAR, DELAY_AVG))]
    g <- g %>%
      add_trace(
        data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=~DELAY_AVG,
        color=~factor(YEAR, levels=years),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group")
    
  } else if (metric == "Delay Ranking (Month)") {
    
    title <- paste(month, "Average Delay Ranking")
    ytitle <- "Average Delay (min.)"
    xtitle <- "Airport"
    temp <- subset(dat$ATFM_APT, NAME %!in% c("Europe",paste("All", c("Countries",unique(dat$ATFM_APT$STATE)))) & MONTH %in% "JUL" & !is.na(DELAY_AVG) & YEAR %in% years) %>%
      .[rev(order(YEAR, DELAY_AVG))]
    g <- g %>%
      add_trace(
        data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=~DELAY_AVG,
        color=~factor(YEAR, levels=years),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group")
    
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