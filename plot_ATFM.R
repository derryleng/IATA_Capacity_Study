plot_ATFM <- function(metric, type, entity, breakdown=T, category, annual=F, annualtargets=T, totalflights=T, top=10, fontsize=12, years, month, rank, rank_title) {
  
  if (metric == "Delays per Flight") {
    
    title <- paste("En-Route ATFM Delay per Flight for", entity)
    ytitle <- "En-Route ATFM Delay per Flight (min.)"
    
    if (breakdown == T & annual == T) {
      xtitle <- "Year"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      
      for (i in 1:length(ATFM_DELAY_CATEGORIES)) {
        if (ATFM_DELAY_CATEGORIES[i] %in% category) {
          g <- g %>% add_trace(y=eval(parse(text=paste0("~", strsplit(ATFM_DELAY_CATEGORIES[i], split=" ")[[1]][1], "_AVG"))),
                               name=ATFM_DELAY_CATEGORIES[i], type="bar", marker=list(color=colour[i]))
        }
      }
      
      if (annualtargets & all(category == ATFM_DELAY_CATEGORIES)) {
        g <- g %>% add_markers(y=~TARGET, name="Delay Target", marker=list(symbol="x", color="red", size=10))
      }
      
      if (totalflights) {
        g <- g %>%
          add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(
            yaxis2=list(overlaying="y",side="right", title="", linewidth=1, showgrid=F,range=c(0,max(subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
            annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90))
          )
      }
      
      g <- g %>% layout(barmode="stack")
      
    } else if (breakdown == F & annual == T) {
      xtitle <- "Year"
      g <- plot_ly(data=subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      g <- g %>%
        add_trace(y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_markers(y=~TARGET, name="Delay Target", marker=list(symbol="x", color="red", size=10)) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack",
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
      
    } else if (breakdown == T & annual == F) {
      xtitle <- "Date"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears)
      )
      
      for (i in 1:length(ATFM_DELAY_CATEGORIES)) {
        if (ATFM_DELAY_CATEGORIES[i] %in% category) {
          g <- g %>% add_trace(y=eval(parse(text=paste0("~", strsplit(ATFM_DELAY_CATEGORIES[i], split=" ")[[1]][1], "_AVG"))),
                               name=ATFM_DELAY_CATEGORIES[i], type="bar", marker=list(color=colour[i]))
        }
      }
      
      if (totalflights) {
        g <- g %>%
          add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
          layout(
            yaxis2=list(overlaying="y",side="right", title="", linewidth=1, showgrid=F,range=c(0,max(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
            annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90))
          )
      }
      
      g <- g %>% layout(barmode="stack", xaxis=list(tickangle=90))
      
    } else if (breakdown == F & annual == F) {
      xtitle <- "Date"
      g <- plot_ly(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears)
      )
      g <- g %>%
        add_trace(y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack", xaxis=list(tickangle=90),
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
    }
    
  } else if (metric == "Delays per Flight (Yearly)") {
    
    title <- paste("En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "En-Route ATFM Delay per Flight (min.)"
    xtitle <- "Month"
    uniqueyears <- unique(subset(dat$ATFM, NAME %in% entity & YEAR %in% years)$YEAR)
    g <- plot_ly()
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$ATFM, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
          x=~factor(MONTH, levels=months),
          y=~DELAY_AVG,
          name=uniqueyears[i],
          type="scatter",
          mode="lines",
          line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3)
        )
    }
    g <- g %>% layout(hovermode="compare")
    
  } else if (metric == "Delays per Flight (Month)") {
    
    title <- paste(month ,"En-Route ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "En-Route ATFM Delay per Flight (min.)"
    xtitle <- "Year"
    if (breakdown == T) {
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      temp <- subset(dat$ATFM, NAME %in% entity & MONTH %in% months[which(monthsfull == month)])
      g <- plot_ly(data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      
      for (i in 1:length(ATFM_DELAY_CATEGORIES)) {
        if (ATFM_DELAY_CATEGORIES[i] %in% category) {
          g <- g %>% add_trace(y=eval(parse(text=paste0("~", strsplit(ATFM_DELAY_CATEGORIES[i], split=" ")[[1]][1], "_AVG"))),
                               name=ATFM_DELAY_CATEGORIES[i], type="bar", marker=list(color=colour[i]))
        }
      }
      
      g <- g %>% layout(barmode="stack")

    } else {
      g <- plot_ly()
      uniqueyears <- unique(subset(dat$ATFM, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dat$ATFM, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    }
    
  } else if (metric == "No. of Monthly Delays") {
    
    title <- paste("Monthly En-Route ATFM Delayed Flights for", entity)
    ytitle <- "No. of Delayed Flights"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Percentage Monthly Delays") {
    
    title <- paste("Monthly En-Route ATFM % Delayed Flights for", entity)
    ytitle <- "Delayed Flights (% of Total Flights within Airspace)"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED/FLIGHTS_TOTAL,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "No. of Monthly Delays (15min+)") {
    
    title <- paste("Monthly En-Route ATFM Delayed Flights (15min+) for", entity)
    ytitle <- "No. of 15min+ Delayed Flights"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED_15,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Percentage Monthly Delays (15min+)") {
    
    title <- paste("Monthly En-Route ATFM % Delayed Flights (15min+) for", entity)
    ytitle <- "15min+ Delayed Flights (% of Total Flights within Airspace)"
    xtitle <- "Date"
    g <- plot_ly()
    g <- g %>%
      add_trace(
        data=subset(dat$ATFM, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)),
        x=~factor(paste(MONTH,YEAR),levels=monthsyears),
        y=~FLIGHTS_DELAYED_15/FLIGHTS_TOTAL,
        type="scatter",
        mode="lines",
        line=list(color="rgb(213,16,103)", width=3)
      ) %>% layout(hovermode="compare", xaxis=list(tickangle=90))
    
  } else if (metric == "Delay Ranking (Yearly)") {
    
    title <- paste("Yearly En-Route ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by", gsub("COUNTRY","State",strsplit(type," ")[[1]][1]))
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_ANNUAL, TYPE %in% type & NAME %!in% paste("All",type) & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    if (type == "COUNTRY (FIR)") temp <- subset(temp, NAME %!in% c("United Kingdom", "UK Oceanic", "Spain", "Spain Canarias", "Portugal", "Portugal Santa Maria"))
    if (type == "FAB (FIR)") temp <- subset(temp, NAME %!in% c("FAB CE (SES RP1)", "FAB CE"))
    temp <- temp %>% subset(., NAME %in% head(unique(.$NAME), top))
    g <- plot_ly(data=temp)
    g <- g %>%
      add_trace(
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=eval(parse(text=paste0("~",rank))),
        color=~factor(YEAR, levels=years),
        colors=ifelse(length(years) == 1, "#d51067", "Spectral"),
        type="bar",
        legendgroup=~YEAR
      )
    
    if (rank == "DELAY_AVG") {
      g <- g %>%
        add_trace(
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~ifelse(is.na(TARGET),0,TARGET),
          name=~paste(YEAR,"Target"),
          marker=list(color="rgba(0,0,0,0)", line=list(color="red", width=10/top)),
          type="bar",
          xaxis="x2",
          showlegend=F,
          legendgroup=~YEAR
        ) %>%
        layout(xaxis2=list(overlaying="x", showticklabels=F))
    }
      
    g <- g %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "Delay Ranking (Month)") {
    
    title <- paste(month, "En-Route ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by", gsub("COUNTRY","State",strsplit(type," ")[[1]][1]))
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    g <- plot_ly()
    temp <- subset(dat$ATFM, MONTH %in% months[which(monthsfull == month)] & TYPE %in% type & NAME %!in% paste("All",type) & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- g %>%
      add_trace(
        data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=eval(parse(text=paste0("~",rank))),
        color=~factor(YEAR, levels=years),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "FAB State Delay Ranking (Yearly)") {
    
    title <- paste("Yearly En-Route ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking in", type)
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_ANNUAL, NAME %in% sort(unique(dat$STATE_FAB[dat$STATE_FAB$FAB %in% type]$STATE)) & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    temp <- temp %>% subset(., NAME %in% head(unique(.$NAME), top))
    g <- plot_ly(data=temp)
    g <- g %>%
      add_trace(
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=eval(parse(text=paste0("~",rank))),
        color=~factor(YEAR, levels=years),
        colors=ifelse(length(years) == 1, "#d51067", "Spectral"),
        type="bar",
        legendgroup=~YEAR
      )
    
    if (rank == "DELAY_AVG") {
      g <- g %>%
        add_trace(
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~ifelse(is.na(TARGET),0,TARGET),
          name=~paste(YEAR,"Target"),
          marker=list(color="rgba(0,0,0,0)", line=list(color="red", width=10/top)),
          type="bar",
          xaxis="x2",
          showlegend=F,
          legendgroup=~YEAR
        ) %>%
        layout(xaxis2=list(overlaying="x", showticklabels=F))
    }

    g <- g %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "FAB State Delay Ranking (Month)") {
    
    title <- paste(month, "En-Route ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking in", type)
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM, NAME %in% sort(unique(dat$STATE_FAB[dat$STATE_FAB$FAB %in% type]$STATE)) & MONTH %in% months[which(monthsfull == month)] & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(gsub("All ","",NAME), levels=unique(gsub("All ","",temp$NAME))),
      y=eval(parse(text=paste0("~",rank))),
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "SES State Delay Ranking (Yearly)") {
    
    title <- paste("Yearly En-Route ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking in SES Area")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_ANNUAL, NAME %in% sort(unique(dat$STATE_FAB[dat$STATE_FAB$STATE %!in% "MUAC"]$STATE)) & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    temp <- temp %>% subset(., NAME %in% head(unique(.$NAME), top))
    g <- plot_ly(data=temp)
    g <- g %>%
      add_trace(
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=eval(parse(text=paste0("~",rank))),
        color=~factor(YEAR, levels=years),
        colors=ifelse(length(years) == 1, "#d51067", "Spectral"),
        type="bar",
        legendgroup=~YEAR
      )
    
    if (rank == "DELAY_AVG") {
      g <- g %>%
        add_trace(
          x=~factor(NAME, levels=unique(temp$NAME)),
          y=~ifelse(is.na(TARGET),0,TARGET),
          name=~paste(YEAR,"Target"),
          marker=list(color="rgba(0,0,0,0)", line=list(color="red", width=10/top)),
          type="bar",
          xaxis="x2",
          showlegend=F,
          legendgroup=~YEAR
        ) %>%
        layout(xaxis2=list(overlaying="x", showticklabels=F))
    }
    
    g <- g %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "SES State Delay Ranking (Month)") {
    
    title <- paste(month, "En-Route ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking in SES Area")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM, NAME %in% sort(unique(dat$STATE_FAB[dat$STATE_FAB$STATE %!in% "MUAC"]$STATE)) & MONTH %in% months[which(monthsfull == month)] & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(gsub("All ","",NAME), levels=unique(gsub("All ","",temp$NAME))),
      y=eval(parse(text=paste0("~",rank))),
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