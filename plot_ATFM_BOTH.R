plot_ATFM_BOTH <- function(metric, entity, top=10, fontsize=12, years, month, barmode) {
  
  if (metric == "Average Monthly Delays (Yearly)") {
    title <- paste("Average En-Route vs Airport Arrival ATFM Delay for", entity)
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    g <- plot_ly()
    g <- g %>%
      add_trace(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
                x=~YEAR, y=~DELAY_AVG, name="ER Delay per Flight", type="bar", marker=list(color="rgb(213,16,103)")) %>%
      add_trace(data=subset(dat$ATFM_APT_ANNUAL, STATE %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
                x=~YEAR, y=~DELAY_AVG, name="APT Delay per Flight", type="bar", marker=list(color="rgb(128,34,69)")) %>%
      add_lines(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
                x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(85,87,89)"), yaxis="y2") %>%
      layout(barmode="group",
             yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL, na.rm=T))),
             annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
    
  } else if (metric == "Average Monthly Delays (Month)") {
    title <- paste(month, "Average En-Route vs Airport ATFM Delay for", entity)
    ytitle <- "Average Delay (min.)"
    xtitle <- ""
    g <- plot_ly()
    g <- g %>%
      add_trace(data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% arrange(factor(YEAR, levels=years_range)),
                x=~YEAR, y=~DELAY_AVG, name="ER Delay per Flight", type="bar", marker=list(color="rgb(213,16,103)")) %>%
      add_trace(data=subset(dat$ATFM_APT, STATE %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% arrange(factor(YEAR, levels=years_range)),
                x=~YEAR, y=~DELAY_AVG, name="APT Delay per Flight", type="bar", marker=list(color="rgb(128,34,69)")) %>%
      add_lines(data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% arrange(factor(YEAR, levels=years_range)),
                x=~YEAR, y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(85,87,89)"), yaxis="y2") %>%
      layout(barmode="group",
             yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL, na.rm=T))),
             annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
    
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
