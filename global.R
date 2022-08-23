
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
library(DT) # renderDataTable()
library(ranger)
library(performance) # check_model(mdlLM)
library(iml)

source("config.R")
source("train.R")




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












