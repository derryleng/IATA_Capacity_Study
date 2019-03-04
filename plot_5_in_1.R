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
  
  title <- "Sources of Delay Comparison"
  ytitle <- ifelse(grepl("^Average", metric), "Average Delay (min.)", "Total Delay (min.)")
  
  
  if (metric == "Average Monthly Delays") {
    if (barmode == "Separate") {
      
      g1 <- plot_ly(
        data=subset(dat$ATFM_ANNUAL, NAME %in% "SES Area (FIR)" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~ifelse(is.na(DELAY_AVG),0,DELAY_AVG),
        color=~as.factor(YEAR),
        colors=brewer.pal(length(years)+1,"Reds")[-c(1)],
        name="En-Route ATFM Delay",
        type="bar",
        showlegend=F
      ) %>% layout(xaxis=list(title="En-Route ATFM Delay"))
      
      g2 <- plot_ly(
        data=subset(dat$ATFM_APT_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~ifelse(is.na(DELAY_AVG),0,DELAY_AVG),
        color=~as.factor(YEAR),
        colors=brewer.pal(length(years)+1,"Oranges")[-c(1)],
        name="Airport Arrival ATFM Delay",
        type="bar",
        showlegend=F
      ) %>% layout(xaxis=list(title="Airport Arrival ATFM Delay"))
      
      g3 <- plot_ly(
        data=subset(dat$ASMA_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~ifelse(is.na(TIME_ADD/FLIGHTS_UNIMPEDED),0,TIME_ADD/FLIGHTS_UNIMPEDED),
        color=~as.factor(YEAR),
        colors=brewer.pal(length(years)+1,"Greens")[-c(1)],
        name="ASMA Additional Time",
        type="bar",
        showlegend=F
      ) %>% layout(xaxis=list(title="ASMA Additional Time"))
      
      g4 <- plot_ly(
        data=subset(dat$TAXI_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
        x=~YEAR,
        y=~ifelse(is.na(TIME_ADD/FLIGHTS_UNIMPEDED),0,TIME_ADD/FLIGHTS_UNIMPEDED),
        color=~as.factor(YEAR),
        colors=brewer.pal(length(years)+1,"Blues")[-c(1)],
        name="Taxi-Out Additional Time",
        type="bar",
        showlegend=F
      ) %>% layout(xaxis=list(title="Taxi-Out Additional Time"))
      
      if (predep_source == "Airlines") {
        g5 <- plot_ly(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~ifelse(is.na(DELAY_AL/FLIGHTS_AL),0,DELAY_AL/FLIGHTS_AL),
          color=~as.factor(YEAR),
          colors=brewer.pal(length(years)+1,"Purples")[-c(1)],
          name="Pre-Departure Delay (AL)",
          type="bar",
          showlegend=F
        ) %>% layout(xaxis=list(title="Pre-Departure Delay (AL)"))
      } else if (predep_source == "Airports") {
        g5 <- plot_ly(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~ifelse(is.na(DELAY_APT/FLIGHTS_APT),0,DELAY_APT/FLIGHTS_APT),
          color=~as.factor(YEAR),
          colors=brewer.pal(length(years)+1,"Purples")[-c(1)],
          name="Pre-Departure Delay (APT)",
          type="bar",
          showlegend=F
        ) %>% layout(xaxis=list(title="Pre-Departure Delay (APT)"))
      }
      
      g <- subplot(list(g1,g2,g3,g4,g5),nrows=1, shareY=T, titleX=T)
      
    } else {
      
      if (annual == F) {
        g <- g %>%
          add_trace(
            data=subset(dat$ATFM, NAME %in% "SES Area (FIR)" & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
            x=~factor(paste(MONTH,YEAR),levels=monthsyears),
            y=~DELAY_AVG,
            name="En-Route ATFM Delay",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Reds")[-c(1)] %>% .[length(.)-1])
          ) %>% add_trace(
            data=subset(dat$ATFM_APT, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
            x=~factor(paste(MONTH,YEAR),levels=monthsyears),
            y=~DELAY_AVG,
            name="Airport Arrival ATFM Delay",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Oranges")[-c(1)] %>% .[length(.)-1])
          ) %>% add_trace(
            data=subset(dat$ASMA, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
            x=~factor(paste(MONTH,YEAR),levels=monthsyears),
            y=~TIME_ADD/FLIGHTS_UNIMPEDED,
            name="ASMA Additional Time",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Greens")[-c(1)] %>% .[length(.)-1])
          ) %>% add_trace(
            data=subset(dat$TAXI, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
            x=~factor(paste(MONTH,YEAR),levels=monthsyears),
            y=~TIME_ADD/FLIGHTS_UNIMPEDED,
            name="Taxi-Out Additional Time",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Blues")[-c(1)] %>% .[length(.)-1])
          ) %>% layout(xaxis=list(title=""))
        
        if (predep_source == "Airlines") {
          g <- g %>%
            add_trace(
              data=subset(dat$PREDEP, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
              x=~factor(paste(MONTH,YEAR),levels=monthsyears),
              y=~DELAY_AL/FLIGHTS_AL,
              name="Pre-Departure Delay (AL)",
              type="bar",
              marker=list(color=brewer.pal(length(years)+1,"Purples")[-c(1)] %>% .[length(.)-1]) 
            ) %>% layout(xaxis=list(title=""))
        } else if (predep_source == "Airports") {
          g <- g %>%
            add_trace(
              data=subset(dat$PREDEP, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
              x=~factor(paste(MONTH,YEAR),levels=monthsyears),
              y=~DELAY_APT/FLIGHTS_APT,
              name="Pre-Departure Delay (APT)",
              type="bar",
              marker=list(color=brewer.pal(length(years)+1,"Purples")[-c(1)] %>% .[length(.)-1])
            ) %>% layout(xaxis=list(title=""))
        }
        
        g <- g %>% layout(xaxis=list(tickangle=90))
        
      } else if (annual == T) {
        g <- g %>%
          add_trace(
            data=subset(dat$ATFM_ANNUAL, NAME %in% "SES Area (FIR)" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
            x=~YEAR,
            y=~DELAY_AVG,
            name="En-Route ATFM Delay",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Reds")[-c(1)] %>% .[length(.)-1])
          ) %>%
          add_trace(
            data=subset(dat$ATFM_APT_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
            x=~YEAR,
            y=~DELAY_AVG,
            name="Airport Arrival ATFM Delay",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Oranges")[-c(1)] %>% .[length(.)-1])
          ) %>%
          add_trace(
            data=subset(dat$ASMA_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
            x=~YEAR,
            y=~TIME_ADD/FLIGHTS_UNIMPEDED,
            name="ASMA Additional Time",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Greens")[-c(1)] %>% .[length(.)-1])
          ) %>%
          add_trace(
            data=subset(dat$TAXI_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
            x=~YEAR,
            y=~TIME_ADD/FLIGHTS_UNIMPEDED,
            name="Taxi-Out Additional Time",
            type="bar",
            marker=list(color=brewer.pal(length(years)+1,"Blues")[-c(1)] %>% .[length(.)-1])
          ) %>% layout(xaxis=list(title=""))
        
        if (predep_source == "Airlines") {
          g <- g %>%
            add_trace(
              data=subset(dat$PREDEP_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
              x=~YEAR,
              y=~DELAY_AL/FLIGHTS_AL,
              name="Pre-Departure Delay (AL)",
              type="bar",
              marker=list(color=brewer.pal(length(years)+1,"Purples")[-c(1)] %>% .[length(.)-1])
            ) %>% layout(xaxis=list(title=""))
        } else if (predep_source == "Airports") {
          g <- g %>%
            add_trace(
              data=subset(dat$PREDEP_ANNUAL, NAME %in% "SES" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
              x=~YEAR,
              y=~DELAY_APT/FLIGHTS_APT,
              name="Pre-Departure Delay (APT)",
              type="bar",
              marker=list(color=brewer.pal(length(years)+1,"Purples")[-c(1)] %>% .[length(.)-1])
            ) %>% layout(xaxis=list(title=""))
        }
      }
      
      if (barmode == "Grouped") {
        g <- g %>% layout(barmode="group")
      } else if (barmode == "Stacked") {
        g <- g %>% layout(barmode="stack")
      }
    }
    
  } else if (metric == "Total Monthly Delays") {
    
    
  }
  

  
  g <- g %>%
    layout(
      title=title,
      font=list(size=fontsize),
      xaxis=list(linewidth=1, showgrid=F, autotick=F),
      yaxis=list(title=ytitle, linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}
