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
  
  title <- "ASMA Additional Time, Taxi-Out Additional Time & Pre-Departure Delay"
  ytitle <- "Average Delay (min.)"
  
  
  if (barmode == "Separate") {
    
    xtitle <- "ASMA Additional Time"
    g1 <- plot_ly(
        data=subset(dat$ASMA_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~ifelse(is.na(TIME_ADD/FLIGHTS_UNIMPEDED),0,TIME_ADD/FLIGHTS_UNIMPEDED),
        color=~as.factor(YEAR),
        colors=brewer.pal(length(years)+1,"Reds")[-c(1)],
        name="ASMA Additional Time",
        type="bar",
        showlegend=F
      )
    
    if (predep_source == "Airlines") {
      g2 <- plot_ly(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~ifelse(is.na(DELAY_AL/FLIGHTS_AL),0,DELAY_AL/FLIGHTS_AL),
          color=~as.factor(YEAR),
          colors=brewer.pal(length(years)+1,"Blues")[-c(1)],
          name="Pre-Departure Delay (AL)",
          type="bar",
          showlegend=F
        ) %>% layout(xaxis=list(title="Pre-Departure Delay (AL)"))
    } else if (predep_source == "Airports") {
      g2 <- plot_ly(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~ifelse(is.na(DELAY_APT/FLIGHTS_APT),0,DELAY_APT/FLIGHTS_APT),
          color=~as.factor(YEAR),
          colors=brewer.pal(length(years)+1,"Blues")[-c(1)],
          name="Pre-Departure Delay (APT)",
          type="bar",
          showlegend=F
        ) %>% layout(xaxis=list(title="Pre-Departure Delay (APT)"))
    }
    
    g3 <- plot_ly(
      data=subset(dat$TAXI_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x=~YEAR,
      y=~ifelse(is.na(TIME_ADD/FLIGHTS_UNIMPEDED),0,TIME_ADD/FLIGHTS_UNIMPEDED),
      color=~as.factor(YEAR),
      colors=brewer.pal(length(years)+1,"Greens")[-c(1)],
      name="Taxi-Out Additional Time",
      type="bar",
      showlegend=F
    ) %>% layout(xaxis=list(title="Taxi-Out Additional Time"))
    
    g <- subplot(list(g1,g2,g3),nrows=1, shareY=T, titleX=T)
    #g <- g2
    
  } else {
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
