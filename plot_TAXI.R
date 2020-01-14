plot_TAXI <- function(metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly()
  
  if (metric == "Total Monthly Delays") {
    
    title <- paste("Total Taxi-Out Additional Time for", entity)
    ytitle <- paste("Total Delay (min.)")
    xtitle <- ""
    if (annual == F) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~TIME_ADD,
          name="Taxi-Out Additional Time",
          type="bar",
          marker=list(color="EF8700")
        ) %>% layout(xaxis=list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~TIME_ADD,
          name="Taxi-Out Additional Time",
          type="bar",
          marker=list(color="EF8700")
        )
    }
    
    
  } else if (metric == "Average Monthly Delays" | grepl("^Delays per Flight \\(Pre-Dep [A-z]+\\)$",metric)) {
    
    title <- paste("Average Taxi-Out Additional Time for", entity)
    ytitle <- paste("Average Delay (min.)")
    xtitle <- ""
    if (annual == F) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~TIME_ADD/FLIGHTS_UNIMPEDED,
          name="Taxi-Out Additional Time",
          type="bar",
          marker=list(color="EF8700")
        ) %>% layout(xaxis=list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$TAXI_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~TIME_ADD/FLIGHTS_UNIMPEDED,
          name="Taxi-Out Additional Time",
          type="bar",
          marker=list(color="EF8700")
        )
    }

    
  } else if (metric == "Average Monthly Delays (Yearly)") {
    title <- paste("Average Taxi-Out Additional Time for", entity, "Yearly Trends")
    ytitle <- paste("Average Delay (min.)")
    xtitle <- ""
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
    title <- paste(month, "Average Taxi-Out Additional Time for", entity, "Yearly Trends")
    ytitle <- paste("Average Delay (min.)")
    xtitle <- ""
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
    title <- paste("Average Taxi-Out Additional Time Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$TAXI_ANNUAL
    } else {
      temp <- subset(dat$TAXI_ANNUAL, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(TIME_ADD) & FLIGHTS_UNIMPEDED != 0 & !is.na(FLIGHTS_UNIMPEDED) & NAME %!in% c("SES", SES_States) & YEAR %in% years) %>% .[rev(order(YEAR, TIME_ADD/FLIGHTS_UNIMPEDED))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~TIME_ADD/FLIGHTS_UNIMPEDED,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "Airport Delay Ranking (Month)") {
    title <- paste(month, "Average Taxi-Out Additional Time Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$TAXI
    } else {
      temp <- subset(dat$TAXI, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(TIME_ADD) & FLIGHTS_UNIMPEDED != 0 & !is.na(FLIGHTS_UNIMPEDED) & NAME %!in% c("SES", SES_States) & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% .[rev(order(YEAR, TIME_ADD/FLIGHTS_UNIMPEDED))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~TIME_ADD/FLIGHTS_UNIMPEDED,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "Top 20 Airport Delay Ranking (Yearly)") {
    title <- paste("Average Taxi-Out Additional Time Ranking")
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    
    temp <- subset(dat$TAXI_ANNUAL, NAME %in% Top20_Airports_ASMA_TAXI & YEAR %in% years) %>% .[rev(order(YEAR, TIME_ADD/FLIGHTS_UNIMPEDED))]
    
    g <- plot_ly(
      data=temp,
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~TIME_ADD/FLIGHTS_UNIMPEDED,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "Top 20 Airport Delay Ranking (Month)") {
    title <- paste(month, "Average Taxi-Out Additional Time Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- "Average Delay (min.)"
    xtitle <- ""

    temp <- subset(dat$TAXI, NAME %in% Top20_Airports_ASMA_TAXI & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% .[rev(order(YEAR, TIME_ADD/FLIGHTS_UNIMPEDED))]
    
    g <- plot_ly(
      data=temp,
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
    )
  
  return(g)
}