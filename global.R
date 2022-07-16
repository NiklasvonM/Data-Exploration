
library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard) # dashboard UI
library(lubridate) # date manipulation
library(rhandsontable) # interactive table (Excel-like)
#library(leaflet) # world map
#library(leaflet.minicharts) # world map
#library(leaftime) # animated map
library(magrittr) # pipe
library(jsonlite) # json reading
library(scico) # scale_color_scico
library(ggdark)
library(DataExplorer)
library(dndselectr) # dropZoneInput(), dragZone()

source("config.R")


DATA <- fread(paste0("data/", fileName), encoding = "UTF-8")


COLUMNS <- names(DATA)
#COLUMNS <- sort(COLUMNS)
N <- nrow(DATA)


# set in config.R
if(generateReport) {
  create_report(
    DATA,
    output_file = "Report.html",
    report_title = "Report",
    y = ifelse(DV %in% COLUMNS, DV, NULL),
    config = configure_report(
      add_plot_prcomp = TRUE,
      add_plot_density = TRUE,
      #plot_qq_args = list("by" = "cut", sampled_rows = 1000L),
      #plot_bar_args = list("with" = "carat"),
      plot_correlation_args = list("cor_args" = list("use" = "pairwise.complete.obs")),
      #plot_boxplot_args = list("by" = "cut"),
      global_ggtheme = quote(theme_minimal()),
      plot_str_args = list("type" = "radial", "height" = 800, "width" = 800, "print_network" = FALSE) #"diagonal", "radial"
    )
  )
}



# correlation of activities with each other
MAT_COR <- matrix(data = NA_real_, nrow = length(COLUMNS), ncol = length(COLUMNS))
colnames(MAT_COR) <- COLUMNS
rownames(MAT_COR) <- COLUMNS
for (i in COLUMNS) {
  for (j in COLUMNS) {
    if(is.numeric(DATA[[i]]) & is.numeric(DATA[[j]]))
      MAT_COR[i, j] <- round(cor(DATA[[i]], DATA[[j]]), 2)
  }
}









