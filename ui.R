
shinyUI(
    dashboardPage(
        dashboardHeader(
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            #menuItem("Report", tabName = "report"),
            menuItem("Data", tabName = "data"),
            menuItem("Correlation matrix", tabName = "correlation_matrix"),
            menuItem("Dependent variable by column value", tabName = "dv_distribution_by_column_value"),
            menuItem("Dependencies", tabName = "dependencies"),
            menuItem("Check for Keys", tabName = "check_for_keys"),
            menuItem("Model", tabName = "model"),
            width = "400px"
          ),
          fileInput(
            inputId = "sel_file",
            label = "File",
            multiple = FALSE
            #choices = list.files(path = "data/", recursive = TRUE)
          ),
          selectInput(
              inputId = "sel_column1",
              label = "Column 1",
              choices = NULL,
              multiple = FALSE,
              selectize = FALSE,
              size = 5
          ),
          # selectInput(
          #   inputId = "sel_column2",
          #   label = "Column 2",
          #   choices = COLUMNS,
          #   multiple = FALSE,
          #   selectize = FALSE,
          #   size = 5
          # ),
          selectInput(
            inputId = "sel_dv",
            label = "Dependent variable",
            selected = NULL,
            choices = NULL,
            multiple = FALSE,
            selectize = TRUE
          )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                    fluidPage(
                    )
                ),
                tabItem(tabName = "data",
                    fluidPage(
                      DT::dataTableOutput("data")
                    )   
                ),
                # Apparently not working together with observe()
                # tabItem(tabName = "report",
                #     fluidPage(
                #       includeHTML("Report.html")
                #     )
                # ),
                tabItem(tabName = "correlation_matrix",
                    shiny::h2("Correlation of the individual numeric columns"),
                    plotlyOutput("correlation_matrix_plot", height = "1200px"),
                    sliderInput("correlationThreshold",
                                "Minimum correlation",
                                min = -1, max = 1, value = c(-1, 1), step = 0.01),
                    #dataTableOutput("correlation_matrix")
                    rHandsontableOutput("correlation_matrix")
                ),
                tabItem(tabName = "dv_distribution_by_column_value",
                    shiny::h2("Dependent variable by column value"),
                    plotlyOutput("dv_distribution_by_column_value")
                ),
                tabItem(tabName = "dependencies",
                    rHandsontableOutput("dependencies")
                ),
                tabItem(tabName = "check_for_keys",
                    fluidPage(
                      column(
                        width = 3,
                        dragZone(
                          id = "dragzone_key",
                          choices = NULL
                        )
                      ),
                      column(
                        width = 9,
                        fluidRow(
                          column(
                            width = 3,
                            dropZoneInput(
                              inputId = "dropzone_key",
                              choices = NULL
                            )
                          )
                        ),
                        fluidRow(
                          rHandsontableOutput(
                            outputId = "keys_table"
                          )
                        )
                      )
                    )
                ),
                tabItem(tabName = "model",
                    fluidPage(
                      fluidRow(
                        column(6,
                          selectInput(
                           inputId = "sel_model",
                           label = "Model",
                           choices = c("rf", "lm")
                          )
                        ),
                        column(6,
                          selectInput(
                           inputId = "sel_attributes_mdl_influence",
                           label = "Attributes",
                           choices = NULL,
                           multiple = TRUE
                          )    
                        )
                      ),
                      fluidRow(
                        plotOutput(
                          outputId = "plot_mdl_influence"
                        )
                      ),
                      fluidRow(
                        plotOutput(
                          outputId = "plot_mdl_check"
                        )
                      )
                    )
                )
            )
        )
    )
)