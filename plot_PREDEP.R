plot_PREDEP <- function(dataset, metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)))
  
  if (metric == "APT Total Monthly Delays") {
    title <- paste("ATC Pre-Departure Total Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Total Delay (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_APT,
        type="scatter",
        mode="lines+markers",
        line=list(color="rgb(213,16,103)", width=3),
        marker=list(color="rgb(34,45,50)")
      )
    
  } else if (metric == "AL Total Monthly Delays") {
    title <- paste("ATC Pre-Departure Total Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Total Delay (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_AL,
        type="scatter",
        mode="lines+markers",
        line=list(color="rgb(213,16,103)", width=3),
        marker=list(color="rgb(34,45,50)")
      )
    
  } else if (metric == "APT Average Monthly Delays") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_APT/FLIGHTS_APT,
        type="scatter",
        mode="lines+markers",
        line=list(color="rgb(213,16,103)", width=3),
        marker=list(color="rgb(34,45,50)")
      )
    
  } else if (metric == "AL Average Monthly Delays") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_AL/FLIGHTS_AL,
        type="scatter",
        mode="lines+markers",
        line=list(color="rgb(213,16,103)", width=3),
        marker=list(color="rgb(34,45,50)")
      )
    
  } else if (metric == "APT Average Monthly Delays (Yearly)") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years)$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
          x=~factor(MONTH,levels=months),
          y=~DELAY_APT/FLIGHTS_APT,
          name=uniqueyears[i],
          type="scatter",
          mode="lines+markers",
          line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3),
          marker=list(color="rgb(34,45,50)")
        )
    }

  } else if (metric == "AL Average Monthly Delays (Yearly)") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years)$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
          x=~factor(MONTH,levels=months),
          y=~DELAY_AL/FLIGHTS_AL,
          name=uniqueyears[i],
          type="scatter",
          mode="lines+markers",
          line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3),
          marker=list(color="rgb(34,45,50)")
        )
    }

  } else if (metric == "APT Average Monthly Delays (Month)") {
    title <- paste(month,"ATC Pre-Departure Average Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
          x=~YEAR,
          y=~DELAY_APT/FLIGHTS_APT,
          name=uniqueyears[i],
          marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
          type="bar",
          showlegend=F
        )
    }
    
  } else if (metric == "AL Average Monthly Delays (Month)") {
    title <- paste(month,"ATC Pre-Departure Average Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    uniqueyears <- unique(subset(dataset, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dataset, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
          x=~YEAR,
          y=~DELAY_AL/FLIGHTS_AL,
          name=uniqueyears[i],
          marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
          type="bar",
          showlegend=F
        )
    }

  }
  
  g <- g %>%
    layout(
      title=title,
      legend=list(x=1.04,y=0.5),
      font=list(size=fontsize),
      xaxis=list(title="Date", linewidth=1, showgrid=F, tickangle=90, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}