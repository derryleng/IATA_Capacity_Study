plot_PREDEP <- function(metric, type, entity, breakdown=T, annual=F, top=10, fontsize=12, years, month) {
  g <- plot_ly(data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)))
  
  if (metric == "APT Total Monthly Delays") {
    title <- paste("ATC Pre-Departure Total Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Total Delay (min.)")
    xtitle <- ""
    if (annual == F) {
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_APT,
          type="bar",
          marker=list(color="FCEA4C")
        ) %>% layout(xaxis = list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~DELAY_APT,
          type="bar",
          marker=list(color="FCEA4C")
        )
    }

    
  } else if (metric == "AL Total Monthly Delays") {
    title <- paste("ATC Pre-Departure Total Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Total Delay (min.)")
    xtitle <- ""
    if (annual == F) {
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_AL,
          type="bar",
          marker=list(color="FCE625")
        ) %>% layout(xaxis = list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~DELAY_AL,
          type="bar",
          marker=list(color="FCE625")
        )
    }

    
  } else if (metric == "APT Average Monthly Delays" | metric == "Delays per Flight (Pre-Dep APT)") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    if (annual == F) {
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_APT/FLIGHTS_APT,
          name="Pre-Departure Delay (APT)",
          type="bar",
          marker=list(color="FCEA4C")
        ) %>% layout(xaxis = list(tickangle=90))
    } else if (annual == T) {
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

    
  } else if (metric == "AL Average Monthly Delays" | metric == "Delays per Flight (Pre-Dep AL)") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by CODA/Airlines) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    if (annual == F) {
      g <- g %>%
        add_trace(
          x=~factor(paste(MONTH,YEAR),levels=monthsyears),
          y=~DELAY_AL/FLIGHTS_AL,
          name="Pre-Departure Delay (AL)",
          type="bar",
          marker=list(color="FCE625")
        ) %>% layout(xaxis = list(tickangle=90))
    } else if (annual == T) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)),
          x=~YEAR,
          y=~DELAY_AL/FLIGHTS_AL,
          name="Pre-Departure Delay (AL)",
          type="bar",
          marker=list(color="FCE625")
        )
    }

    
  } else if (metric == "APT Average Monthly Delays (Yearly)") {
    title <- paste("ATC Pre-Departure Average Delay (Reported by Airport Operators) for", entity)
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    uniqueyears <- unique(subset(dat$PREDEP, NAME %in% entity & YEAR %in% years)$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
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
    xtitle <- ""
    uniqueyears <- unique(subset(dat$PREDEP, NAME %in% entity & YEAR %in% years)$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
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
    xtitle <- ""
    uniqueyears <- unique(subset(dat$PREDEP, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
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
    xtitle <- ""
    uniqueyears <- unique(subset(dat$PREDEP, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$PREDEP, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
          x=~YEAR,
          y=~DELAY_AL/FLIGHTS_AL,
          name=uniqueyears[i],
          marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
          type="bar",
          showlegend=F
        )
    }

  } else if (metric == "AL Airport Delay Ranking (Yearly)") {
    
    title <- paste("ATC Pre-Departure Average Delay (Reported by CODA/Airlines) Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$PREDEP_ANNUAL
    } else {
      temp <- subset(dat$PREDEP_ANNUAL, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(DELAY_AL) & FLIGHTS_AL != 0 & !is.na(FLIGHTS_AL) & NAME %!in% c("SES", SES_States) & YEAR %in% years) %>% .[rev(order(YEAR, DELAY_AL/FLIGHTS_AL))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~DELAY_AL/FLIGHTS_AL,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "AL Airport Delay Ranking (Month)") {
    
    title <- paste(month,"ATC Pre-Departure Average Delay (Reported by CODA/Airlines) Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$PREDEP
    } else {
      temp <- subset(dat$PREDEP, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(DELAY_AL) & FLIGHTS_AL != 0 & !is.na(FLIGHTS_AL) & NAME %!in% c("SES", SES_States) & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% .[rev(order(YEAR, DELAY_AL/FLIGHTS_AL))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~DELAY_AL/FLIGHTS_AL,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "APT Airport Delay Ranking (Yearly)") {
    
    title <- paste("ATC Pre-Departure Average Delay (Reported by Airport Operators) Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$PREDEP_ANNUAL
    } else {
      temp <- subset(dat$PREDEP_ANNUAL, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(DELAY_APT) & FLIGHTS_APT != 0 & !is.na(FLIGHTS_APT) & NAME %!in% c("SES", SES_States) & YEAR %in% years) %>% .[rev(order(YEAR, DELAY_APT/FLIGHTS_APT))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~DELAY_APT/FLIGHTS_APT,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "APT Airport Delay Ranking (Month)") {
    
    title <- paste(month,"ATC Pre-Departure Average Delay (Reported by Airport Operators) Ranking", ifelse(type=="All Countries", "", paste("for", type)))
    ytitle <- paste("Delay per IFR Departure (min.)")
    xtitle <- ""
    if (type == "All Countries") {
      temp <- dat$PREDEP
    } else {
      temp <- subset(dat$PREDEP, STATE %in% type)
    }
    temp <- temp %>% subset(., !is.na(DELAY_APT) & FLIGHTS_APT != 0 & !is.na(FLIGHTS_APT) & NAME %!in% c("SES", SES_States) & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% .[rev(order(YEAR, DELAY_APT/FLIGHTS_APT))]
    
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=~DELAY_APT/FLIGHTS_APT,
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
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