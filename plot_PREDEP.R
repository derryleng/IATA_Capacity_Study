plot_PREDEP <- function(dataset, metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly(data=subset(dataset, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)))
  
  if (metric == "Total Monthly Delays (APT)") {
    title <- paste("ATC Pre-Departure Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_APT,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      )
  } else if (metric == "Total Monthly Delays (AL)") {
    title <- paste("ATC Pre-Departure Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Delay (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_APT/FLIGHTS_APT,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      )
  } else if (metric == "Delays per IFR Dep. (APT)") {
    title <- paste("ATC Pre-Departure Delay per IFR Departure (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_AL,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      )
  } else if (metric == "Delays per IFR Dep. (AL)") {
    title <- paste("ATC Pre-Departure Delay per IFR Departure (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    g <- g %>%
      add_trace(
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~DELAY_AL/FLIGHTS_AL,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      )
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