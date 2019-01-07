plot_ATFM_BOTH <- function(metric, type, entity, top=10, fontsize=12, years, month, barmode) {
  
  if (metric == "Average Monthly Delays (Yearly)") {
    g <- plot()
    g <- g %>%
      add_trace(
        data=unique(subset(dat$ATFM_APT_ANNUAL, grepl("^All *", NAME) | grepl("^NA$", NAME), select=-c(ICAO, NAME)))
      ) %>%
      add_trace(
        data=unique(subset(dat$ATFM_ANNUAL, TYPE %in% c("COUNTRY (FIR)","FAB (FIR)")))
      )
    
  } else if (metric == "Average Monthly Delays (Month)") {
    g <- plot(data=unique(subset(dat$ATFM_APT, (grepl("^All *", NAME) | grepl("^NA$", NAME)) & MONTH %in% months[which(monthsfull == month)], select=-c(ICAO, NAME))))
    
  }
  

}
