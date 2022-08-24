
shinyServer <- function(input, output, session) {
  
  
  DATA <- reactive({
    req(input$sel_file)
    #as.data.table(input$sel_file)
    #fread(paste0("data/", input$sel_file), encoding = "UTF-8")
    data <- fread(input$sel_file$datapath, encoding = "UTF-8")
    names(data) <- make.names(names(data))
    data
  })
  COLUMNS <- reactive({
    names(DATA())
  })
  N <- reactive({
    nrow(DATA())
  })
  
  # correlation of activities with each other
  MAT_COR <- reactive({
    columns <- COLUMNS()
    data <- DATA()
    MAT_COR <- matrix(data = NA_real_, nrow = length(columns), ncol = length(columns))
    colnames(MAT_COR) <- columns
    rownames(MAT_COR) <- columns
    for (i in columns) {
      for (j in columns) {
        if(is.numeric(data[[i]]) & is.numeric(data[[j]]))
          MAT_COR[i, j] <- round(cor(data[[i]], data[[j]]), 2)
      }
    }
    MAT_COR
  })

    # observe({
    #   create_report(
    #     DATA,
    #     output_file = "Report.html",
    #     report_title = "Report",
    #     y = input$sel_dv,
    #     config = configure_report(
    #       add_plot_prcomp = TRUE,
    #       #plot_qq_args = list("by" = "cut", sampled_rows = 1000L),
    #       #plot_bar_args = list("with" = "carat"),
    #       plot_correlation_args = list("cor_args" = list("use" = "pairwise.complete.obs")),
    #       #plot_boxplot_args = list("by" = "cut"),
    #       global_ggtheme = quote(theme_minimal())
    #     )
    #   )
    # })
  
  
  shiny::observeEvent(input$sel_file, {
    #print("Observing file...")
    columns <- COLUMNS()
    
    updateSelectInput(inputId = "sel_column1", choices = columns)
    updateSelectInput(inputId = "sel_dv", choices = columns)
    updateSelectInput(inputId = "sel_attributes_mdl_influence", choices = columns)
    
    
    updateDropZoneInput(session, inputId = "dropzone_key", presets = character(0), choices = columns)
    updateDragZone(session, id = "dragzone_key", choices = columns)
    
  })
  
  shiny::observeEvent(input$sel_dv, {
    columns <- COLUMNS()
    
    updateSelectInput(inputId = "sel_attributes_mdl_influence", choices = setdiff(columns, input$sel_dv))
  })
  
  ## Not needed any more
  # rdtKeys <- reactive({
  #   req(input$sel_file_key)
  #   print("Getting reactive file...")
  #   fread(paste0("data/", input$sel_file_key))
  # })

  
  
  output$data <- DT::renderDataTable({
      DATA()
  }, options = list(pageLength = 20L, scrollX = TRUE))
  
    
    
  output$correlation_matrix <- renderRHandsontable({#renderDataTable({
      # tbl <- rhandsontable(MAT_COR())
      # tbl <- hot_cols(tbl, fixedColumnsLeft = 0)
      # tbl <- hot_rows(tbl, fixedRowsTop = 0)
      tbl <- rhandsontable(MAT_COR())
      tbl <- hot_cols(tbl,
          renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
           if (col != row && value < -0.1) {
            td.style.background = 'pink';
           } else if (col != row && value > 0.1) {
            td.style.background = 'lightgreen';
           }
         }")
      tbl
  })
  
  output$correlation_matrix_plot <- renderPlotly({
    matCor <- MAT_COR()
    columns <- COLUMNS()
    dtPlot <- data.table(
      Column1 = rownames(matCor)[row(matCor)],
      Column2 = colnames(matCor)[col(matCor)],
      KorrelationPlot = c(matCor)
    )
    for(col in columns) {
      maxCor <- max(abs(dtPlot[Column1 == col & Column1 != Column2]$KorrelationPlot))
      dtPlot[Column1 == col, Shape := as.factor(as.integer(abs(KorrelationPlot) == maxCor))]
    }
    dtPlot[, Korrelation := KorrelationPlot]
    dtPlot[!(input$correlationThreshold[1] <= Korrelation & Korrelation <= input$correlationThreshold[2]), KorrelationPlot := NA]
    dtPlot[Korrelation < 0, Farbe := -1]
    dtPlot[Korrelation > 0, Farbe := 1]
    dtPlot[is.na(Farbe), Farbe := 0]
    p <- ggplot(
      dtPlot,
      aes(
        Column1,
        Column2,
        size = abs(KorrelationPlot),
        color = Farbe,
        shape = Shape,
        text = paste0(
          "Column 1: ", Column1, "<br>",
          "Column 2: ", Column2, "<br>",
          "Correlation: ", Korrelation
        )
      )
    ) +
      geom_point() +
      scale_color_continuous(low = "#3794bf", high = "#df8640") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab("Column 1") +
      ylab("Column 2") +
      scale_size_continuous(limits = c(0, 1)) +
      # Keep order
      scale_x_discrete(limits = columns) +
      scale_y_discrete(limits = columns)
    ggplotly(p, tooltip = "text")
  })
  
  
  output$dv_distribution_by_column_value <- renderPlotly({
    req(input$sel_column1 != input$sel_dv)
    req(is.numeric(DATA()[[input$sel_dv]]))
    #req(length(unique(DATA()[[input$sel_dv]])) < 30)
    #req(length(unique(DATA()[[input$sel_column1]])) < 30)
    column <- input$sel_column1
    dv <- input$sel_dv
    dtPlot <- DATA()[, c(dv, column), with = FALSE]
    
    dtPlot <- dtPlot[, .(Mean = mean(get(dv), na.rm = TRUE), N = .N), by = c(column)]
    setnames(dtPlot, "Mean", dv)
    p <- ggplot(dtPlot, aes_string(paste0("`", column, "`"), dv)) +
      geom_bar(stat = "identity") +
      ylab(paste("Mean", dv))
    
    # if (is.numeric(dtPlot[[column]])) {
    #     dtPlot[, (column) := ordered(as.character(get(column)))]
    # } else {
    #     dtPlot[, (column) := factor(get(column))]
    # }
    # dtPlot[, n_column := .N, by = c(column)]
    # dtPlot[, Datenpunkte := .N, by = c(dv, column)]
    # dtPlot <- unique(dtPlot)
    # dtFill <- as.data.table(expand.grid(
    #     dv = unique(dtPlot[[dv]]),
    #     V1 = unique(dtPlot[[column]])
    # ))
    # setnames(dtFill, "V1", column)
    # setnames(dtFill, "dv", dv)
    # dtPlot <- merge(dtPlot, dtFill, by = c(dv, column), all = TRUE)
    # dtPlot[, `dv distribution` := Datenpunkte / n_column]
    # dtPlot[is.na(`dv distribution`), `dv distribution` := 0]
    # dtPlot[is.na(Datenpunkte), Datenpunkte := 0]
    # setnames(dtPlot, column, make.names(column))
    # p <- ggplot(dtPlot, aes_string(x = dv, y = "`dv distribution`", color = column)) +
    #     geom_line() +
    #     geom_point(aes(size = Datenpunkte)) +
    #     scale_y_continuous(limits = c(0, 1)) +
    #     #scale_x_continuous(limits = c(1, 5)) +
    #     scale_size_continuous(limits = c(0, max(dtPlot$n)), guide = "none") +
    #     theme_minimal()
    p <- ggplotly(p)
    
    p
  })
  
  # Check for dependencies i.e. if column A has the value x, then column B always has the value y.
  output$dependencies <- renderRHandsontable({
    data <- DATA()
    n <- N()
    dtDependencies <- data.table(Column = COLUMNS(), DependentColumns = "")
    for(col in dtDependencies$Column) {
      dependentColumns <- NULL
      for(val in unique(data[[col]])) {
        dtCur <- data[get(col) == val]
        # Skip in case of too few or too many observations of this value
        if(nrow(dtCur) < pmax(10, n / 20) | n - nrow(dtCur) < pmax(10, n / 20))
          next
        for(dependentColumn in dtDependencies$Column) {
          if(col == dependentColumn)
            next
          if(length(unique(dtCur[[dependentColumn]])) == 1) {
            dependentVal <- unique(dtCur[[dependentColumn]])
            dependentColumns <- c(dependentColumns, paste0(val, " -> ", dependentColumn, ": ", dependentVal))
          }
        }
      }
      dtDependencies[Column == col, DependentColumns := paste0(dependentColumns, collapse = ", ")]
    }
    
    
    rhandsontable(dtDependencies, stretchH = "all") %>%
      hot_cols(colWidths = c(100, 50, 50),
               manualColumnMove = FALSE,
               manualColumnResize = TRUE
               ##, wordWrap = "yes please"
      ) %>%
      hot_rows(rowHeights = NULL ) %>% #default
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    
    
    
    # rhandsontable(
    #   dtDependencies,
    #   stretchH =
    # ) %>%
    #   hot_cols(
    #     manualColumnResize = TRUE
    #   )
  })
  
  # Check if the presumed keys actually form a key.
  # I.e. if we have a dataset that contains items and item categories,
  # choosing item id should not give any cases (as the id should form a key),
  # but choosing item category should give cases, as multiple items are in one
  # item category.
  output$keys_table <- renderRHandsontable({
    req(input$dropzone_key)
    potentialKey <- input$dropzone_key
    dt <- DATA()#rdtKeys()
    dt <- dt[, .(N = .N), by = c(potentialKey)][N > 1]
    rhandsontable(
      dt
    ) %>%
      hot_cols(columnSorting = TRUE)
  })
  
  # taken from library(DataExplorer)
  data_introduction <- reactive({
    data <- DATA()
    split_data <- DataExplorer::split_columns(data)
    output <- data.table(rows = nrow(data), columns = ncol(data), 
                         discrete_columns = split_data[["num_discrete"]], continuous_columns = split_data[["num_continuous"]], 
                         all_missing_columns = split_data[["num_all_missing"]], 
                         total_missing_values = sum(is.na(data)), complete_rows = sum(complete.cases(data)), 
                         total_observations = nrow(data) * ncol(data), memory_usage = as.numeric(object.size(data)))
    output
  })
  
  output$data_basic_statistics <- renderTable({
    intro <- data_introduction()
    memory_usage <- intro[["memory_usage"]]
    class(memory_usage) <- "object_size"
    intro_df <- data.frame(
      "Name" = c("Rows", "Columns",
                 "Discrete columns", "Continuous columns", "All missing columns",
                 "Missing observations", "Complete Rows",
                 "Total observations", "Memory allocation"),
      "Value" = c(
        format(intro[["rows"]], big.mark = ","),
        format(intro[["columns"]], big.mark = ","),
        format(intro[["discrete_columns"]], big.mark = ","),
        format(intro[["continuous_columns"]], big.mark = ","),
        format(intro[["all_missing_columns"]], big.mark = ","),
        format(intro[["total_missing_values"]], big.mark = ","),
        format(intro[["complete_rows"]], big.mark = ","),
        format(intro[["total_observations"]], big.mark = ","),
        format(memory_usage, unit = "auto")
      )
    )
  }, hover = FALSE, bordered = TRUE, striped = TRUE)
  
  
  output$data_missings <- renderPlot({
    DataExplorer::plot_missing(DATA())
  })
  
  output$data_histogram_plots <- renderPlot({
    dtTest <<- DATA()
    DataExplorer::plot_histogram(DATA(), nrow = ceiling(data_introduction()$continuous_columns / 3), ncol = 3)
  })
  
  
  
  
  MODEL <- reactive({
    req(input$sel_model)
    req(input$sel_dv)
    
    mdl <- train(input$sel_model, DATA(), input$sel_dv)
    mdl
  })
  
  IML_MODEL_EFFECT <- reactive({
    req(input$sel_attributes_mdl_influence)
    
    allNumeric <- TRUE
    for(attribute in input$sel_attributes_mdl_influence) {
      if(!is.numeric(DATA()[[attribute]])) {
        allNumeric <- FALSE
      }
    }
    req(allNumeric)
    
    X <- DATA()[, -input$sel_dv, with = FALSE]
    
    model <- Predictor$new(MODEL(), data = X, y = DATA()[[input$sel_dv]])
    effect <- FeatureEffects$new(model)
    effect
  })
  
  output$plot_mdl_influence <- renderPlot({
    
    IML_MODEL_EFFECT()$plot(features = input$sel_attributes_mdl_influence)
  })
  
  output$plot_mdl_check <- renderPlot({
    req(isTRUE(input$sel_model == "lm"))
    
    performance::check_model(MODEL())
  })
    
    
}
