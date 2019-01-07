plot_ASMA <- function(metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly()
  
  if (metric == "Average Monthly Delays") {
    title <- paste("Average Monthly ASMA Additional Time for", entity)
    ytitle <- paste("Average Delay (min.)")
    xtitle <- "Date"
    g <- g %>%
      add_trace(
        data=subset(dat$ASMA, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~TIME_ADD,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      )
    
  } else if (metric == "Average Monthly Delays (Yearly)") {
    title <- paste("Average Monthly ASMA Additional Time for", entity, "Yearly Trends")
    ytitle <- paste("Average Delay (min.)")
    xtitle <- "Month"
    uniqueyears <- unique(subset(dat$ASMA, NAME %in% entity & YEAR %in% years)$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$ASMA, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
          x=~factor(MONTH, levels=months),
          y=~TIME_ADD,
          name=uniqueyears[i],
          type="scatter",
          mode="lines",
          line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3)
        )
    }
    g <- g %>% layout(hovermode="compare")
    
  } else if (metric == "Average Monthly Delays (Month)") {
    title <- paste(month, "Average Monthly ASMA Additional Time for", entity, "Yearly Trends")
    ytitle <- paste("Average Delay (min.)")
    xtitle <- "Year"
    uniqueyears <- unique(subset(dat$ASMA, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$ASMA, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
          x=~YEAR,
          y=~TIME_ADD,
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
      xaxis=list(title=xtitle, linewidth=1, showgrid=F, tickangle=90, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}