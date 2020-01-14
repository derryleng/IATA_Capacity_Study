plot_TRAFFIC_FORECAST <- function(entity, fontsize=12, years) {

  g <- plot_ly()
  
  types <- unique(dat$TRAFFIC_FORECAST[ENTITY %in% entity]$TYPE)
  
  if ("2014-B" %in% types) {
    g <- g %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2014-L" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2014 Forecast", showlegend=F,
        type="scatter", mode="lines", line = list(color = "#3690c0", dash = "dash")
      ) %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2014-H" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2014 Forecast", showlegend=F,
        type="scatter", mode="lines", line = list(color = "#0570b0", dash = "dash"), fill = 'tonexty', fillcolor = "rgba(130, 177, 255, 0.7)"
      ) %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2014-B" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2014 Forecast", name = "2014 Forecast", 
        type="scatter", mode="lines", line = list(color = "#034e7b")
      )
  }
  
  if ("2015-B" %in% types) {
    g <- g %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2015-L" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2015 Forecast", showlegend=F,
        type="scatter", mode="lines", line = list(color = "#00b3b3", dash = "dash")
      ) %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2015-H" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2015 Forecast", showlegend=F,
        type="scatter", mode="lines", line = list(color = "#008080", dash = "dash"), fill = 'tonexty', fillcolor = "rgba(51, 255, 255, 0.7)"
      ) %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2015-B" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2015 Forecast", name = "2015 Forecast",
        type="scatter", mode="lines", line = list(color = "#009999")
      )
  }
  
  if ("2018-B" %in% types) {
    g <- g %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2018-L" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2018 Forecast", showlegend=F,
        type="scatter", mode="lines", line = list(color = "#41ae76", dash = "dash")
      ) %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2018-H" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2018 Forecast", showlegend=F,
        type="scatter", mode="lines", line = list(color = "#238b45", dash = "dash"), fill = 'tonexty', fillcolor = "rgba(138, 255, 130, 0.7)"
      ) %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2018-B" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, legendgroup = "2018 Forecast", name = "2018 Forecast",
        type="scatter", mode="lines", line = list(color = "#005824")
      )
  }
  
  if ("Actual" %in% types) {
    g <- g %>%
      add_trace(
        data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "Actual" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range_extended)),
        x = ~factor(YEAR, levels=years_range_extended), y = ~TRAFFIC, name = "Actual",
        type="scatter", mode="lines", line = list(color = "red")
      )
  }
  
  g <- g %>%
    layout(
      title=paste("IFR Flight Movements Forecast for", entity),
      font=list(size=fontsize),
      xaxis=list(title="Year", linewidth=1, showgrid=F, autotick=F),
      yaxis=list(title="IFR Movements (1000s)", linewidth=1, showgrid=F)
    )
  
  return(g)
}