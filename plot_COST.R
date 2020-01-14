plot_COST <- function(fontsize=12, years) {
  
  title <- "SES Cost of Delays"
  xtitle <- ""
  ytitle <- "Euro"
  
  g <- plot_ly(data=dat$SES_DELAY_COSTS %>% arrange(factor(Year, levels=years_range)), type="bar")
  
  g <- g %>%
    add_trace(x=~Year, y = ~ATFM_DELAY_COST, name="En-Route ATFM Delay") %>%
    add_trace(x=~Year, y = ~ATFM_APT_DELAY_COST, name="Airport Arrival ATFM Delay") %>%
    add_trace(x=~Year, y = ~ASMA_DELAY_COST, name="ASMA Additional Time") %>%
    add_trace(x=~Year, y = ~TAXI_DELAY_COST, name="Taxi-Out Additional Time") %>%
    add_trace(x=~Year, y = ~PREDEP_DELAY_COST, name="Pre-Departure Delay (AL)") %>%
    layout(barmode="stack")
  
  g <- g %>%
    layout(
      title=title,
      font=list(size=fontsize),
      xaxis=list(title=xtitle, linewidth=1, showgrid=F, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    )
  
  return(g)
}