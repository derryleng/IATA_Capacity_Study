plot_ASMA_TAXI_PREDEP <- function(
  metric,
  type,
  entity,
  annual,
  fontsize,
  years,
  barmode,
  predep_source
) {
  g <- plot_ly()
  
  title <- paste("ASMA, Taxi-Out, Pre-Departure Delay Comparison for", entity)
  ytitle <- "Average Delay (min.)"
  xtitle <- ""
  
  if (annual == F) {
    g <- g %>%
      add_trace(
        data=subset(dat$ASMA, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~TIME_ADD/FLIGHTS_UNIMPEDED,
        name="ASMA Additional Time",
        type="bar",
        marker=list(color="D62411")
      ) %>% add_trace(
        data=subset(dat$TAXI, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~TIME_ADD/FLIGHTS_UNIMPEDED,
        name="Taxi-Out Additional Time",
        type="bar",
        marker=list(color="EF8700")
      )
    
    if (predep_source == "Airlines") {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_AL/FLIGHTS_AL,
          name="Pre-Departure Delay (AL)",
          type="bar",
          marker=list(color="FCE625") 
        )
    } else if (predep_source == "Airports") {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_APT/FLIGHTS_APT,
          name="Pre-Departure Delay (APT)",
          type="bar",
          marker=list(color="FCEA4C")
        )
    }
    
    g <- g %>% layout(xaxis=list(tickangle=90))
    
  } else if (annual == T) {
    g <- g %>%
      add_trace(
        data=subset(dat$ASMA_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~TIME_ADD/FLIGHTS_UNIMPEDED,
        name="ASMA Additional Time",
        type="bar",
        marker=list(color="D62411")
      ) %>%
      add_trace(
        data=subset(dat$TAXI_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~TIME_ADD/FLIGHTS_UNIMPEDED,
        name="Taxi-Out Additional Time",
        type="bar",
        marker=list(color="EF8700")
      )
    
    if (predep_source == "Airlines") {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~DELAY_AL/FLIGHTS_AL,
          name="Pre-Departure Delay (AL)",
          type="bar",
          marker=list(color="FCE625")
        )
    } else if (predep_source == "Airports") {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~DELAY_APT/FLIGHTS_APT,
          name="Pre-Departure Delay (APT)",
          type="bar",
          marker=list(color="FCEA4C")
        )
    }
  }
  
  if (barmode == "Grouped") {
    g <- g %>% layout(barmode="group")
  } else if (barmode == "Stacked") {
    g <- g %>% layout(barmode="stack")
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