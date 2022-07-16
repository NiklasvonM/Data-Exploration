
shinyUI(
    dashboardPage(
        dashboardHeader(
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            #menuItem("Report", tabName = "report"),
            menuItem("Weekday", tabName = "wochentag", icon = icon("calendar-alt")),
            menuItem("Correlation matrix", tabName = "correlation_matrix"),
            menuItem("Dependent variable by column value", tabName = "dv_distribution_by_column_value"),
            menuItem("Dependencies", tabName = "dependencies"),
            menuItem("Check for Keys", tabName = "check_for_keys"),
            width = "400px"
          ),
          selectInput(
              inputId = "sel_column1",
              label = "Column 1",
              choices = COLUMNS,
              multiple = FALSE,
              selectize = FALSE,
              size = min(length(COLUMNS), 5)
          ),
          selectInput(
            inputId = "sel_column2",
            label = "Column 2",
            choices = COLUMNS,
            multiple = FALSE,
            selectize = FALSE,
            size = min(length(COLUMNS), 5)
          ),
          selectInput(
            inputId = "sel_dv",
            label = "Dependent variable",
            selected = DV,
            choices = COLUMNS,
            multiple = FALSE,
            selectize = FALSE,
            size = min(length(COLUMNS), 5)
          )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                    fluidPage(
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
                          choices = COLUMNS
                        )
                      ),
                      column(
                        width = 9,
                        fluidRow(
                          column(
                            width = 3,
                            dropZoneInput(
                              inputId = "dropzone_key",
                              choices = COLUMNS
                            )
                          ),
                          column(
                            width = 3,
                            selectInput(
                              inputId = "sel_file_key",
                              label = NULL,
                              choices = list.files(path = "data/", recursive = TRUE)
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
                )
            )
        )
    )
)