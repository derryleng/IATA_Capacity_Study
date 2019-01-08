plot_ATFM <- function(metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  
  if (metric == "Delays per Flight") {
    
    title <- paste("En-Route ATFM Delay per Flight for", entity)
    ytitle <- "En-Route ATFM Delay per Flight (min.)"
    
    if (breakdown == T & annual == T) {
      xtitle <- "Year"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      g <- g %>%
        add_trace(y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
        add_trace(y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
        add_trace(y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
        add_trace(y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
        add_trace(y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
        add_trace(y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
        add_trace(y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
        add_trace(y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
        add_trace(y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
        add_trace(y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
        add_trace(y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
        add_trace(y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
        add_trace(y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
        add_trace(y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
        add_trace(y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
        add_trace(y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
        add_markers(y=~TARGET, name="Delay Target", marker=list(symbol="x", color="red", size=10)) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack",
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F,range=c(0,max(subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
      
    } else if (breakdown == F & annual == T) {
      xtitle <- "Year"
      g <- plot_ly(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      g <- g %>%
        add_trace(y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_markers(y=~TARGET, name="Delay Target", marker=list(symbol="x", color="red", size=10)) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack",
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
      
    } else if (breakdown == T & annual == F) {
      xtitle <- "Date"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears)
      )
      g <- g %>%
        add_trace(y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
        add_trace(y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
        add_trace(y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
        add_trace(y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
        add_trace(y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
        add_trace(y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
        add_trace(y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
        add_trace(y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
        add_trace(y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
        add_trace(y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
        add_trace(y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
        add_trace(y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
        add_trace(y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
        add_trace(y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
        add_trace(y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
        add_trace(y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack",  xaxis=list(tickangle=90),
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
      
    } else if (breakdown == F & annual == F) {
      xtitle <- "Date"
      g <- plot_ly(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears)
      )
      g <- g %>%
        add_trace(y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack", xaxis=list(tickangle=90),
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
    }
    
  } else if (metric == "Delays per Flight (Yearly)") {
    
    title <- paste("En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "En-Route ATFM Delay per Flight (min.)"
    xtitle <- "Month"
    uniqueyears <- unique(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$YEAR)
    g <- plot_ly()
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$ATFM, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
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
    
    title <- paste(month ,"En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "En-Route ATFM Delay per Flight (min.)"
    xtitle <- "Year"
    if (breakdown == T) {
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      temp <- subset(dat$ATFM, NAME %in% entity & MONTH %in% months[which(monthsfull == month)])
      g <- plot_ly(data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      g <- g %>%
        add_trace(y=~A_AVG, name="A - Accident/Incident", type="bar", marker=list(color=colour[1])) %>%
        add_trace(y=~C_AVG, name="C - ATC Capacity", type="bar", marker=list(color=colour[2])) %>%
        add_trace(y=~D_AVG, name="D - De-icing", type="bar", marker=list(color=colour[3])) %>%
        add_trace(y=~E_AVG, name="E - Equipment (Non-ATC)", type="bar", marker=list(color=colour[4])) %>%
        add_trace(y=~G_AVG, name="G - Aerodrome Capacity", type="bar", marker=list(color=colour[5])) %>%
        add_trace(y=~I_AVG, name="I - Industrial Action (ATC)", type="bar", marker=list(color=colour[6])) %>%
        add_trace(y=~M_AVG, name="M - Airspace Management", type="bar", marker=list(color=colour[7])) %>%
        add_trace(y=~N_AVG, name="N - Industrial Action (Non-ATC)", type="bar", marker=list(color=colour[8])) %>%
        add_trace(y=~O_AVG, name="O - Other", type="bar", marker=list(color=colour[9])) %>%
        add_trace(y=~P_AVG, name="P - Special Event", type="bar", marker=list(color=colour[10])) %>%
        add_trace(y=~R_AVG, name="R - ATC Routeing", type="bar", marker=list(color=colour[11])) %>%
        add_trace(y=~S_AVG, name="S - ATC Staffing", type="bar", marker=list(color=colour[12])) %>%
        add_trace(y=~T_AVG, name="T - Equipment (ATC)", type="bar", marker=list(color=colour[13])) %>%
        add_trace(y=~V_AVG, name="V - Environmental Issues", type="bar", marker=list(color=colour[14])) %>%
        add_trace(y=~W_AVG, name="W - Weather", type="bar", marker=list(color=colour[15])) %>%
        add_trace(y=~NA_AVG, name="NA - Not Specified", type="bar", marker=list(color=colour[16])) %>%
        layout(barmode="stack")

    } else {
      g <- plot_ly()
      uniqueyears <- unique(subset(dat$ATFM, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dat$ATFM, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    }
    
  } else if (metric == "No. of Monthly Delays") {
    
    title <- paste("Monthly En-Route ATFM Delayed Flights for", entity)
    ytitle <- "No. of Delayed Flights"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Percentage Monthly Delays") {
    
    title <- paste("Monthly En-Route ATFM % Delayed Flights for", entity)
    ytitle <- "Delayed Flights (% of Total Flights within Airspace)"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED/FLIGHTS_TOTAL,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "No. of Monthly Delays (15min+)") {
    
    title <- paste("Monthly En-Route ATFM Delayed Flights (15min+) for", entity)
    ytitle <- "No. of 15min+ Delayed Flights"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED_15,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Percentage Monthly Delays (15min+)") {
    
    title <- paste("Monthly En-Route ATFM % Delayed Flights (15min+) for", entity)
    ytitle <- "15min+ Delayed Flights (% of Total Flights within Airspace)"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED_15/FLIGHTS_TOTAL,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Delay Ranking (Yearly)") {
    
    title <- paste("Yearly Average En-Route AFTM Delay Ranking by", gsub("COUNTRY","State",strsplit(type," ")[[1]][1]))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    temp <- subset(dat$ATFM_ANNUAL, TYPE %in% type & NAME %!in% paste("All",type) & !is.na(DELAY_AVG) & YEAR %in% years & NAME %!in% "FAB CE") %>%
      .[rev(order(YEAR, DELAY_AVG))]
    g <- plot_ly(data=subset(temp, NAME %in% head(unique(temp$NAME), top)))
    g <- g %>%
      add_trace(
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=~DELAY_AVG,
        color=~factor(YEAR, levels=years),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    #g <- g %>% add_markers(x=~factor(NAME, levels=unique(temp$NAME)), y=~TARGET, name="Delay Target", marker=list(symbol="x", color="red", size=10))
    
  } else if (metric == "Delay Ranking (Month)") {
    
    title <- paste(month, "Average En-Route AFTM Delay Ranking by", gsub("COUNTRY","State",strsplit(type," ")[[1]][1]))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    g <- plot_ly()
    temp <- subset(dat$ATFM, MONTH %in% months[which(monthsfull == month)] & TYPE %in% type & NAME %!in% paste("All",type) & !is.na(DELAY_AVG) & YEAR %in% years) %>%
      .[rev(order(YEAR, DELAY_AVG))]
    g <- g %>%
      add_trace(
        data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=~DELAY_AVG,
        color=~factor(YEAR, levels=years),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  }
  
  g <- g %>%
    layout(
      title=title,
      legend=list(x=1.05,y=0.5),
      font=list(size=fontsize),
      xaxis=list(title=xtitle, linewidth=1, showgrid=F, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}