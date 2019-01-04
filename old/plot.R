library(shiny)
library(shinyWidgets)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)

# Set working directory
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))

# Import data
paths <- list.files(paste0(getwd(),"/data/"), pattern="*.csv", full.names=T)
datnames <- c("ASMA","ATFM_APT","ATFM_APT_postops","ATFM_AUA","ATFM_AUA_postops","ATFM_FIR","ATFM_FIR_postops","PREDEP","TAXI")
dat <- lapply(paths, function(x) fread(x, encoding="UTF-8")); names(dat) <- datnames

# Date ordered factors
months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC") %>% factor(., levels=., ordered=T)
years <- as.vector(outer(months, seq(2011,2018,1), FUN="paste")) %>% factor(., levels=., ordered=T)

# Fix format for numeric columns
for (file in names(dat)) {
  for (col in names(dat[[file]])) {
    if (grepl("^[0-9]{1,}$",sample(dat[[file]][[col]] %>% .[!is.na(.)],1))) {
      dat[[file]][[col]] <- as.numeric(dat[[file]][[col]])
    }
  }
}

# Get airports with most IFR arrivals
topifr_table <- dat$ATFM_APT %>%
  aggregate(FLT_ARR_1~APT_ICAO+APT_NAME+`Pivot Label`, data=., sum) %>% 
  as.data.table(.) %>%
  .[rev(order(FLT_ARR_1))]
  #head(., 10)
# cat(paste(topifr_table$`Pivot Label`,collapse="\n"))

# Plotting function
plot_save <- function(data="ATFM_AUA", yearly=T) {
  # Check if data name given is valid
  if (!(data %in% datnames)) {
    # Error if data name given is invalid
    message("Error in data selection: expecting ",paste(unlist(datnames[!grepl("postops",datnames)]),collapse=", "))
  } else {
    # Get data table
    d <- dat[[data]]
    if (data == "ASMA") { # --------------------------------- ASMA -----------------------
        for (airport in topasma_table$APT_ICAO) {
          
          # Gather data
          d.part <- subset(d, APT_ICAO %in% airport)
          d.part$TIME_ASMA_ADD_2_AVG <- d.part$TIME_ASMA_ADD_2/d.part$FLT_ASMA_UNIMP_2
          
          # Get airports with most ASMA delays
          topasma_table <- dat$ASMA[!is.na(dat$ASMA$TIME_ASMA_ADD_2)] %>%
            aggregate(TIME_ASMA_ADD_2~APT_ICAO+APT_NAME+PIVOT_LABEL, data=., sum) %>% 
            as.data.table(.) %>%
            .[rev(order(TIME_ASMA_ADD_2))]
            #head(., 10)
          #cat(paste(topasma_table$PIVOT_LABEL,collapse="\n"))
          
          if (yearly) {
            # Yearly Comparison Delays
            title <- paste("Monthly ASMA Delays for", airport, "(Yearly Comparison)")
            g <- plot_ly()
            for (j in 1:length(unique(d.part$YEAR))) {
              g <- g %>%
                add_trace(
                  data=subset(d.part, YEAR %in% unique(d.part$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=months)),
                  x=~factor(MONTH_MON, levels=months),
                  y=~TIME_ASMA_ADD_2,
                  name=unique(d.part$YEAR)[j],
                  type="scatter",
                  mode="lines",
                  line=list(color=rev(brewer.pal(length(unique(d.part$YEAR)), "RdYlGn"))[j])
                )
            }
            g <- g %>% layout(
              hovermode="compare",
              title=title,
              legend=list(x=100, y=0.5),
              xaxis=list(title="Date", tickangle=90, autotick=F),
              yaxis=list(title="Additional ASMA Time (min.)")
            )
            htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
            
            # Yearly Compaison Delays per Flight
            title <- paste("Monthly ASMA Delays per Flight for", airport, "(Yearly Comparison)")
            g <- plot_ly()
            for (j in 1:length(unique(d.part$YEAR))) {
              g <- g %>%
                add_trace(
                  data=subset(d.part, YEAR %in% unique(d.part$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=months)),
                  x=~factor(MONTH_MON, levels=months),
                  y=~TIME_ASMA_ADD_2_AVG,
                  name=unique(d.part$YEAR)[j],
                  type="scatter",
                  mode="lines",
                  line=list(color=rev(brewer.pal(length(unique(d.part$YEAR)), "RdYlGn"))[j])
                )
            }
            g <- g %>% layout(
              hovermode="compare",
              title=title,
              legend=list(x=100, y=0.5),
              xaxis=list(title="Date", tickangle=90, autotick=F),
              yaxis=list(title="Additional ASMA Time (min.)")
            )
            htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
          } else {
            # Delays
            title <- paste("Monthly ASMA Delays for", airport)
            g <- plot_ly(data=d.part) %>%
              add_trace(
                x=~factor(paste(MONTH_MON,YEAR), levels=years),
                y=~TIME_ASMA_ADD_2,
                type="bar",
                marker=list(color="rgb(213,16,103)")
              ) %>%
              layout(
                barmode="stack",
                title=title, legend=list(x=100, y=0.5),
                xaxis=list(title="Date", tickangle=90, autotick=F),
                yaxis=list(title="Additional ASMA Time (mins)")
              )
            htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
            
            # Delays per Flight
            title <- paste("Monthly ASMA Delays per Flight for", airport)
            g <- plot_ly(data=d.part) %>%
              add_trace(
                x=~factor(paste(MONTH_MON,YEAR), levels=years),
                y=~TIME_ASMA_ADD_2_AVG,
                type="bar",
                marker=list(color="rgb(213,16,103)")
              ) %>%
              layout(
                barmode="stack",
                title=title, legend=list(x=100, y=0.5),
                xaxis=list(title="Date", tickangle=90, autotick=F),
                yaxis=list(title="Additional ASMA Time (mins)")
              )
            htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
          }
          
        }
      
    } else if (data == "ATFM_APT") { # -------------------- ATFM_APT ---------------------
      
      if (aggregates) { # Aggregates data over all airports
        
        # Gather data
        ATFM_APT_num <- c(
          "FLT_ARR_1","DLY_APT_ARR_1","DLY_APT_ARR_A_1","DLY_APT_ARR_C_1","DLY_APT_ARR_D_1",
          "DLY_APT_ARR_E_1","DLY_APT_ARR_G_1","DLY_APT_ARR_I_1","DLY_APT_ARR_M_1","DLY_APT_ARR_N_1",
          "DLY_APT_ARR_O_1","DLY_APT_ARR_P_1","DLY_APT_ARR_R_1","DLY_APT_ARR_S_1","DLY_APT_ARR_T_1",
          "DLY_APT_ARR_V_1","DLY_APT_ARR_W_1","DLY_APT_ARR_NA_1"
        )
        for (i in 1:length(ATFM_APT_num)) {
          if (i == 1) {
            d.agg <- aggregate(eval(parse(text=ATFM_APT_num[i]))~YEAR+MONTH_MON,data=d,sum)
            names(d.agg) <- c("YEAR","MONTH_MON",ATFM_APT_num[1])
          } else {
            d.agg <- merge(d.agg, aggregate(eval(parse(text=ATFM_APT_num[i]))~YEAR+MONTH_MON,data=d,sum), all.x=T)
            names(d.agg) <- c("YEAR","MONTH_MON",ATFM_APT_num[1:i])
          }
        }
        for (col in ATFM_APT_num[-c(1)]) {
          d.agg[[paste0(col,"_AVG")]] <- ifelse(d.agg[[col]] != 0, d.agg[[col]]/d.agg$FLT_ARR_1, NA)
        }
        
        # Delays
        title <- "Monthly Arrival ATFM Delayed Flights for All Airports"
        g <- plot_ly(data=aggregate(FLT_ARR_1_DLY~YEAR+MONTH_MON,data=dat$ATFM_APT,sum)) %>%
          add_trace(
            x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
            y=~FLT_ARR_1_DLY,
            type="bar",
            marker=list(color="rgb(213,16,103)")
          ) %>%
          layout(
            hovermode="compare",
            title=title, legend=list(x=100, y=0.5),
            xaxis=list(title="Date", tickangle=90, autotick=F),
            yaxis=list(title="No. of Delayed Flights")
          )
        htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
        
        title <- "Monthly Arrival ATFM Delayed Flights for All Airports (15 min+)"
        filename <- "ATFM_APT_DELAYED_FLIGHTS_15_APTs.html"
        g <- plot_ly(data=aggregate(FLT_ARR_1_DLY_15~YEAR+MONTH_MON,data=dat$ATFM_APT,sum)) %>%
          add_trace(
            x=~factor(paste(YEAR,MONTH_MON), levels=yearsordered),
            y=~FLT_ARR_1_DLY_15,
            type="bar",
            marker=list(color="rgb(213,16,103)")
          ) %>%
          layout(
            hovermode="compare",
            title=title, legend=list(x=100, y=0.5),
            xaxis=list(title="Date", tickangle=90, autotick=F),
            yaxis=list(title="No. of Delayed Flights")
          )
        htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
        
        # Delays by Category
        
        # Delays per Flight
        
        # Delays per Flight by Category
        
        # Yearly Comparison Delays 
        
        # Yearly Comparison Delays by Category
        
        # Yearly Comparison Delays per Flight
        
        # Yearly Comparison Delays per Flight by Category
        
        
        
        
        
        
        # Yearly Comparison Delays
        title <- "Monthly ASMA Delays for All Airports (Yearly Comparison)"
        g <- plot_ly()
        for (j in 1:length(unique(d.agg$YEAR))) {
          g <- g %>%
            add_trace(
              data=subset(d.agg, YEAR %in% unique(d.agg$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=months)),
              x=~factor(MONTH_MON, levels=months),
              y=~TIME_ASMA_ADD_2,
              name=unique(d.agg$YEAR)[j],
              type="scatter",
              mode="lines",
              line=list(color=rev(brewer.pal(length(unique(d.agg$YEAR)), "RdYlGn"))[j])
            )
        }
        g <- g %>% layout(
          hovermode="compare",
          title=title,
          legend=list(x=100, y=0.5),
          xaxis=list(title="Date", tickangle=90, autotick=F),
          yaxis=list(title="Additional ASMA Time (min.)")
        )
        htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
        
        # Yearly Comparison Delays per Flight
        title <- "Monthly ASMA Delays per Flight for All Airports (Yearly Comparison)"
        g <- plot_ly()
        for (j in 1:length(unique(d.agg$YEAR))) {
          g <- g %>%
            add_trace(
              data=subset(d.agg, YEAR %in% unique(d.agg$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=months)),
              x=~factor(MONTH_MON, levels=months),
              y=~TIME_ASMA_ADD_2_AVG,
              name=unique(d.agg$YEAR)[j],
              type="scatter",
              mode="lines",
              line=list(color=rev(brewer.pal(length(unique(d.agg$YEAR)), "RdYlGn"))[j])
            )
        }
        g <- g %>% layout(
          hovermode="compare",
          title=title,
          legend=list(x=100, y=0.5),
          xaxis=list(title="Date", tickangle=90, autotick=F),
          yaxis=list(title="Additional ASMA Time (min.)")
        )
        htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
        
        # Delays
        title <- "Monthly ASMA Delays for All Airports"
        g <- plot_ly(data=d.agg) %>%
          add_trace(
            x=~factor(paste(MONTH_MON,YEAR), levels=years),
            y=~TIME_ASMA_ADD_2,
            type="bar",
            marker=list(color="rgb(213,16,103)")
          ) %>%
          layout(
            barmode="stack",
            title=title, legend=list(x=100, y=0.5),
            xaxis=list(title="Date", tickangle=90, autotick=F),
            yaxis=list(title="Additional ASMA Time (mins)")
          )
        htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
        
        # Delays per Flight
        title <- "Monthly ASMA Delays per Flight for All Airports"
        g <- plot_ly(data=d.agg) %>%
          add_trace(
            x=~factor(paste(MONTH_MON,YEAR), levels=years),
            y=~TIME_ASMA_ADD_2_AVG,
            type="bar",
            marker=list(color="rgb(213,16,103)")
          ) %>%
          layout(
            barmode="stack",
            title=title, legend=list(x=100, y=0.5),
            xaxis=list(title="Date", tickangle=90, autotick=F),
            yaxis=list(title="Additional ASMA Time (mins)")
          )
        htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
        
      } else { # Data for each of the busiest airports
        for (airport in topasma_icao) {
          
          # Gather data
          d.part <- subset(d, APT_ICAO %in% airport)
          d.part$TIME_ASMA_ADD_2_AVG <- d.part$TIME_ASMA_ADD_2/d.part$FLT_ASMA_UNIMP_2
          
          # Yearly Comparison Delays
          title <- paste("Monthly ASMA Delays for", airport, "(Yearly Comparison)")
          g <- plot_ly()
          for (j in 1:length(unique(d.part$YEAR))) {
            g <- g %>%
              add_trace(
                data=subset(d.part, YEAR %in% unique(d.part$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=months)),
                x=~factor(MONTH_MON, levels=months),
                y=~TIME_ASMA_ADD_2,
                name=unique(d.part$YEAR)[j],
                type="scatter",
                mode="lines",
                line=list(color=rev(brewer.pal(length(unique(d.part$YEAR)), "RdYlGn"))[j])
              )
          }
          g <- g %>% layout(
            hovermode="compare",
            title=title,
            legend=list(x=100, y=0.5),
            xaxis=list(title="Date", tickangle=90, autotick=F),
            yaxis=list(title="Additional ASMA Time (min.)")
          )
          htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
          
          # Yearly Compaison Delays per Flight
          title <- paste("Monthly ASMA Delays per Flight for", airport, "(Yearly Comparison)")
          g <- plot_ly()
          for (j in 1:length(unique(d.part$YEAR))) {
            g <- g %>%
              add_trace(
                data=subset(d.part, YEAR %in% unique(d.part$YEAR)[j]) %>% arrange(factor(MONTH_MON, levels=months)),
                x=~factor(MONTH_MON, levels=months),
                y=~TIME_ASMA_ADD_2_AVG,
                name=unique(d.part$YEAR)[j],
                type="scatter",
                mode="lines",
                line=list(color=rev(brewer.pal(length(unique(d.part$YEAR)), "RdYlGn"))[j])
              )
          }
          g <- g %>% layout(
            hovermode="compare",
            title=title,
            legend=list(x=100, y=0.5),
            xaxis=list(title="Date", tickangle=90, autotick=F),
            yaxis=list(title="Additional ASMA Time (min.)")
          )
          htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
          
          # Delays
          title <- paste("Monthly ASMA Delays for", airport)
          g <- plot_ly(data=d.part) %>%
            add_trace(
              x=~factor(paste(MONTH_MON,YEAR), levels=years),
              y=~TIME_ASMA_ADD_2,
              type="bar",
              marker=list(color="rgb(213,16,103)")
            ) %>%
            layout(
              barmode="stack",
              title=title, legend=list(x=100, y=0.5),
              xaxis=list(title="Date", tickangle=90, autotick=F),
              yaxis=list(title="Additional ASMA Time (mins)")
            )
          htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
          
          # Delays per Flight
          title <- paste("Monthly ASMA Delays per Flight for", airport)
          g <- plot_ly(data=d.part) %>%
            add_trace(
              x=~factor(paste(MONTH_MON,YEAR), levels=years),
              y=~TIME_ASMA_ADD_2_AVG,
              type="bar",
              marker=list(color="rgb(213,16,103)")
            ) %>%
            layout(
              barmode="stack",
              title=title, legend=list(x=100, y=0.5),
              xaxis=list(title="Date", tickangle=90, autotick=F),
              yaxis=list(title="Additional ASMA Time (mins)")
            )
          htmlwidgets::saveWidget(as_widget(g), paste0(title,".html"), title=title)
          
        }
      }
      
    } else if (data == "ATFM_AUA") { # -------------------- ATFM_AUA ---------------------
      
      
      
    } else if (data == "ATFM_FIR") { # -------------------- ATFM_FIR ---------------------
      
      
      
    } else if (data == "PREDEP") { # ----------------------- PREDEP ----------------------
      
      
      
    } else if (data == "TAXI") { # ------------------------- TAXI ------------------------
      
      
      
    }
  }
}
