plot_ATFM_APT <- function(metric, type, entity, breakdown=T, category, annual=F, annualtargets=T, totalflights=T, top=10, fontsize=12, years, month, rank, rank_title) {
  
  if (metric == "Delays per Flight") {
    title <- paste("Airport Arrivals ATFM Delay per Flight for", entity)
    ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
    
    if (breakdown == T & annual == T) {
      xtitle <- "Year"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      
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
            yaxis2=list(overlaying="y",side="right", title="", linewidth=1, showgrid=F,range=c(0,max(subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
            annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90))
          )
      }
      
      g <- g %>% layout(barmode="stack")
      
    } else if (breakdown == F & annual == T) {
      xtitle <- "Year"
      g <- plot_ly(data=subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      g <- g %>%
        add_trace(y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_lines(y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack",
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM_APT_ANNUAL, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
      
    } else if (breakdown == T & annual == F) {
      xtitle <- "Date"
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(data=subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)), x=~factor(paste(MONTH,YEAR),levels=monthsyears))
      
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
            yaxis2=list(overlaying="y",side="right", title="", linewidth=1, showgrid=F,range=c(0,max(subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
            annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90))
          )
      }
      
      g <- g %>% layout(barmode="stack", xaxis=list(tickangle=90))
      
    } else if (breakdown == F & annual == F) {
      xtitle <- "Date"
      g <- plot_ly(data=subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years) %>% arrange(factor(paste(MONTH, YEAR), levels=monthsyears)))
      g <- g %>%
        add_trace(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~DELAY_AVG, name="Delay per Flight", type="bar", marker=list(color="rgb(85,87,89)")) %>%
        add_lines(x=~factor(paste(MONTH,YEAR),levels=monthsyears), y=~FLIGHTS_TOTAL, name="Total Flights", line=list(color="rgb(213,16,103)"), yaxis="y2") %>%
        layout(barmode="stack", xaxis=list(tickangle=90),
               yaxis2=list(overlaying="y", side="right", title="", linewidth=1, showgrid=F, range=c(0,max(subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years)$FLIGHTS_TOTAL*1.02, na.rm=T))),
               annotations=list(list(x=1, y=1, text="Total Flights", xref="paper", yref="paper", showarrow=F, textangle=90)))
    }
    
  } else if (metric == "Delays per Flight (Yearly)") {
    title <- paste("Airport Arrivals ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
    xtitle <- "Month"
    uniqueyears <- unique(subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years)$YEAR)
    g <- plot_ly()
    for (i in 1:length(uniqueyears)) {
      g <- g %>%
        add_trace(
          data=subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% uniqueyears[i]) %>% arrange(factor(MONTH, levels=months)),
          x=~factor(MONTH, levels=months),
          y=~DELAY_AVG,
          name=uniqueyears[i],
          type="scatter",
          mode="lines",
          line=list(color=rev(brewer.pal(length(uniqueyears),"Dark2"))[i], width=3)
        )
    }
    
  } else if (metric == "Delays per Flight (Month)") {
    
    title <- paste(month, "Airport Arrivals ATFM Delay per Flight for", entity, "Yearly Trends")
    ytitle <- "Airport Arrivals ATFM Delay per Flight (min.)"
    xtitle <- "Year"
    if (breakdown == T) {
      colour <- c(brewer.pal(9, "Set1"), brewer.pal(7, "Set3"))
      g <- plot_ly(data=subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)]) %>% arrange(factor(YEAR, levels=years_range)), x=~YEAR)
      
      for (i in 1:length(ATFM_DELAY_CATEGORIES)) {
        if (ATFM_DELAY_CATEGORIES[i] %in% category) {
          g <- g %>% add_trace(y=eval(parse(text=paste0("~", strsplit(ATFM_DELAY_CATEGORIES[i], split=" ")[[1]][1], "_AVG"))),
                               name=ATFM_DELAY_CATEGORIES[i], type="bar", marker=list(color=colour[i]))
        }
      }
      
      g <- g %>% layout(barmode="stack")
      
    } else {
      uniqueyears <- unique(subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% years & MONTH %in% months[which(monthsfull == month)])$YEAR)
      g <- plot_ly()
      for (i in 1:length(uniqueyears)) {
        g <- g %>%
          add_trace(
            data=subset(dat$ATFM_APT, NAME %in% entity & YEAR %in% uniqueyears[i] & MONTH %in% months[which(monthsfull == month)]),
            x=~YEAR,
            y=~DELAY_AVG,
            name=uniqueyears[i],
            marker=list(color=rev(brewer.pal(length(uniqueyears),"Spectral"))[i]),
            type="bar",
            showlegend=F
          )
      }
    }
    
  } else if (metric == "Airport Delay Ranking (Yearly)") {
    title <- paste("Yearly Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by Airport")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT_ANNUAL, NAME %!in% c("Europe",paste("All", c("Countries",unique(dat$ATFM_APT$STATE)))) & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
        data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=eval(parse(text=paste0("~",rank))),
        color=~factor(YEAR, levels=years_range),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "Airport Delay Ranking (Month)") {
    title <- paste("Average Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by Airport")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT, NAME %!in% c("Europe",paste("All", c("Countries",unique(dat$ATFM_APT$STATE)))) & MONTH %in% months[which(monthsfull == month)] & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
        data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
        x=~factor(NAME, levels=unique(temp$NAME)),
        y=eval(parse(text=paste0("~",rank))),
        color=~factor(YEAR, levels=years_range),
        colors="Spectral",
        type="bar"
      ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "State Airport Delay Ranking (Yearly)") {
    title <- paste("Yearly Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking in", type)
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT_ANNUAL, STATE %in% type & NAME %!in% c("Europe",paste("All", c("Countries",unique(dat$ATFM_APT$STATE)))) & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=eval(parse(text=paste0("~",rank))),
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "State Airport Delay Ranking (Month)") {
    title <- paste(month, "Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking in", type)
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT, STATE %in% type & NAME %!in% c("Europe",paste("All", c("Countries",unique(dat$ATFM_APT$STATE)))) & MONTH %in% months[which(monthsfull == month)] & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(NAME, levels=unique(temp$NAME)),
      y=eval(parse(text=paste0("~",rank))),
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "State Delay Ranking (Yearly)") {
    title <- paste("Yearly Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by State")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT_ANNUAL, NAME %in% paste("All", c("Countries",unique(dat$ATFM_APT$STATE))) & STATE %!in% ATFM_APT_FAB & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(gsub("All ","",NAME), levels=unique(gsub("All ","",temp$NAME))),
      y=eval(parse(text=paste0("~",rank))),
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "State Delay Ranking (Month)") {
    title <- paste(month, "Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by State")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT, NAME %in% paste("All", c("Countries",unique(dat$ATFM_APT$STATE))) & STATE %!in% ATFM_APT_FAB & MONTH %in% months[which(monthsfull == month)] & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, NAME %in% head(unique(temp$NAME), top)),
      x=~factor(gsub("All ","",NAME), levels=unique(gsub("All ","",temp$NAME))),
      y=eval(parse(text=paste0("~",rank))),
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "FAB Delay Ranking (Yearly)") {
    title <- paste("Yearly Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by FAB")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT_ANNUAL, STATE %in% ATFM_APT_FAB & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, STATE %in% head(unique(temp$STATE), top)),
      x=~factor(STATE, levels=unique(temp$STATE)),
      y=eval(parse(text=paste0("~",rank))),
      color=~factor(YEAR, levels=years_range),
      colors="Spectral",
      type="bar"
    ) %>% layout(barmode="group", xaxis=list(tickangle=45))
    
  } else if (metric == "FAB Delay Ranking (Month)") {
    title <- paste(month, "Airport Arrival ATFM", ifelse(rank_title=="Total Flights","Flight",ifelse(rank_title=="Average Delay",rank_title,paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay"))), "Ranking by FAB")
    ytitle <- ifelse(rank_title=="Total Flights",rank_title,ifelse(rank_title=="Average Delay","Average Delay (min.)",paste(strsplit(rank_title,split=" - ")[[1]][2],"Delay (min.)")))
    xtitle <- ""
    temp <- subset(dat$ATFM_APT, STATE %in% ATFM_APT_FAB & MONTH %in% months[which(monthsfull == month)] & !is.na(eval(parse(text=rank))) & YEAR %in% years) %>%
      .[rev(order(YEAR, eval(parse(text=rank))))]
    g <- plot_ly(
      data=subset(temp, STATE %in% head(unique(temp$STATE), top)),
      x=~factor(STATE, levels=unique(temp$STATE)),
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