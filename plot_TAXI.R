plot_TAXI <- function(metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly()
  
  if (metric == "Total Monthly Delays") {
    
    title <- paste("Total Monthly Taxi-Out Additional Time for", entity)
    ytitle <- paste("Total Delay (min.)")
    xtitle <- "Date"
    if (annual == F) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~TIME_ADD,
          type="bar",
          marker=list(color="rgb(213,16,103)")
        ) %>% layout(xaxis=list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~TIME_ADD,
          type="bar",
          marker=list(color="rgb(213,16,103)")
        )
    }
    
    
  } else if (metric == "Average Monthly Delays") {
    
    title <- paste("Average Monthly Taxi-Out Additional Time for", entity)
    ytitle <- paste("Average Delay (min.)")
    xtitle <- "Date"
    if (annual == F) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~TIME_ADD/FLIGHTS_UNIMPEDED,
          type="bar",
          marker=list(color="rgb(213,16,103)")
        ) %>% layout(xaxis=list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~TIME_ADD/FLIGHTS_UNIMPEDED,
          type="bar",
          marker=list(color="rgb(213,16,103)")
        )
    }

    
  } else if (metric == "Average Monthly Delays (Yearly)") {
    title <- paste("Average Monthly Taxi-Out Additional Time for", entity, "Yearly Trends")
    ytitle <- paste("Average Delay (min.)")
    xtitle <- "Month"
    uniqueyears <- unique(subset(dat$TAXI, NAME %in% entity & YEAR %in% years)$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
          x=~factor(MONTH, levels=months),
          y=~TIME_ADD,
          name=uniqueyears[i],
          type="scatter",
          mode="lines+markers",
          line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3),
          marker=list(color="rgb(34,45,50)")
        )
    }
    g <- g %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Average Monthly Delays (Month)") {
    title <- paste(month, "Average Monthly Taxi-Out Additional Time for", entity, "Yearly Trends")
    ytitle <- paste("Average Delay (min.)")
    xtitle <- "Year"
    uniqueyears <- unique(subset(dat$TAXI, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
          x=~YEAR,
          y=~TIME_ADD,
          name=uniqueyears[i],
          marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
          type="bar",
          showlegend=F
        )
    }
  } else if (metric == "Airport Delay Ranking (Yearly)") {
    title <- paste("Average Monthly Taxi-Out Additional Time Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$TAXI_ANNUAL
    } else {
      temp <- subset(dat$TAXI_ANNUAL, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(TIME_ADD) & FLIGHTS_UNIMPEDED != 0 & !is.na(FLIGHTS_UNIMPEDED) & YEAR %in% years) %>% .[rev(order(YEAR, TIME_ADD/FLIGHTS_UNIMPEDED))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~TIME_ADD/FLIGHTS_UNIMPEDED,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "Airport Delay Ranking (Month)") {
    
    title <- paste(month, "Average Monthly Taxi-Out Additional Time Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$TAXI
    } else {
      temp <- subset(dat$TAXI, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(TIME_ADD) & FLIGHTS_UNIMPEDED != 0 & !is.na(FLIGHTS_UNIMPEDED) & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% .[rev(order(YEAR, TIME_ADD/FLIGHTS_UNIMPEDED))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~TIME_ADD/FLIGHTS_UNIMPEDED,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  }
  
  g <- g %>%
    layout(
      title=title,
      font=list(size=fontsize),
      xaxis=list(title=xtitle, linewidth=1, showgrid=F, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}