plot_TRAFFIC_FORECAST <- function(entity, fontsize=12, years) {

  g <- plot_ly()
  
  g <- g %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2014-L" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2014-L", legendgroup = "2014 Forecast",
      type="scatter", mode="lines", line = list(color = "#3690c0", dash = "dash")
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2014-H" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2014-H", legendgroup = "2014 Forecast",
      type="scatter", mode="lines", line = list(color = "#0570b0", dash = "dash"), fill = 'tonexty', fillcolor = "rgba(130, 177, 255, 0.7)"
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2014-B" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2014-B", legendgroup = "2014 Forecast",
      type="scatter", mode="lines", line = list(color = "#034e7b")
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2015-L" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2015-L", legendgroup = "2015 Forecast",
      type="scatter", mode="lines", line = list(color = "#3690c0", dash = "dash")
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2015-H" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2015-H", legendgroup = "2015 Forecast",
      type="scatter", mode="lines", line = list(color = "#0570b0", dash = "dash"), fill = 'tonexty', fillcolor = "rgba(130, 177, 255, 0.7)"
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2015-B" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2015-B", legendgroup = "2015 Forecast",
      type="scatter", mode="lines", line = list(color = "#034e7b")
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2018-L" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2018-L", legendgroup = "2018 Forecast",
      type="scatter", mode="lines", line = list(color = "#41ae76", dash = "dash")
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2018-H" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2018-H", legendgroup = "2018 Forecast",
      type="scatter", mode="lines", line = list(color = "#238b45", dash = "dash"), fill = 'tonexty', fillcolor = "rgba(138, 255, 130, 0.7)"
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "2018-B" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "2018-B", legendgroup = "2018 Forecast",
      type="scatter", mode="lines", line = list(color = "#005824")
    ) %>%
    add_trace(
      data=subset(dat$TRAFFIC_FORECAST, ENTITY %in% entity & TYPE %in% "Actual" & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
      x = ~factor(YEAR, levels=years_range), y = ~TRAFFIC, name = "Actual",
      type="scatter", mode="lines", line = list(color = "red")
    )
  
  g <- g %>%
    layout(
      title=paste("IFR Flight Movements Forecast for", entity),
      legend=list(x=1.04,y=0.5),
      font=list(size=fontsize),
      xaxis=list(title="Year", linewidth=1, showgrid=F, autotick=F),
      yaxis=list(title="IFR Movements (1000s)", linewidth=1, showgrid=F)
    ) %>% config(collaborate=F, showLink=F)
  
  return(g)
}