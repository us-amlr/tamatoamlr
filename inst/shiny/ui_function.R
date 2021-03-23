ui.tabs <- function() {
  tabItems(
    #--------------------------------------------------------------------------
    tabItem(
      tabName = "tab_info",
      fluidRow(
        box(
          title = "Database connection information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          tableOutput("pool_db_conn")
        ),
        box(
          title = "Season information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          tableOutput("info_season_info")
        )
      )
    ),

    #--------------------------------------------------------------------------
    tabItem(
      tabName = "tab_afs_natal",
      fluidRow(
        box(
          status = "primary", width = 6,
          plotOutput("afs_natal_plot")
        ),
        column(
          width = 6,
          fluidRow(
            box(
              title = "Plot info", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(
                  width = 3,
                  radioButtons("afs_natal_type", label = tags$h5("Plot to display"),
                               choices = list("Overview" = "overview", "Natality rate" = "natal_rate",
                                              "Proportional loss type" = "prop_loss"),
                               selected = NULL)
                )
              )
            ),
            box(
              title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(3, selectInput("afs_natal_season_min", label = tags$h5("Minimum season"),
                                      choices = season.list, selected = season.list.id.max)),
                column(3, selectInput("afs_natal_season_max", label = tags$h5("Maximum season"),
                                      choices = season.list, selected = season.list.id.max)),
                column(3, checkboxInput("afs_natal_prime", label = "Plot prime age", value = FALSE))
              )
            )
          )
        )
      )
    ),

    #--------------------------------------------------------------------------
    tabItem(
      tabName = "tab_census",
      tags$h5("todo: census")
    ),

    #--------------------------------------------------------------------------
    tabItem(
      tabName = "tab_tr",
      # numericInput("nrows", "How many rows to show?", 10),
      # tableOutput("tr_tbl")
      fluidRow(
        column(
          width = 6,
          fluidRow(
            box(
              status = "primary", width = 12,
              plotOutput("tr_plot")
            ),
            box(
              status = "primary", width = 12,
              DTOutput("tr_tbl")
            )
          )
        ),
        column(
          width = 6,
          fluidRow(
            box(
              title = "Plot info", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(
                  width = 4,
                  checkboxGroupInput("tr_species", label = tags$h5("Species"),
                                     choices = list("Fur seals" = "fur seal",
                                                    "Elephant seals" = "elephant seal",
                                                    "Leopard seals" = "leopard seal",
                                                    "Weddell seals" = "weddell seal"),
                                     selected = "fur seal")
                ),
                column(
                  width = 4,
                  radioButtons("tr_type", label = tags$h5("Data to plot"),
                               choices = list("Individuals by year" = "ind_by_year",
                                              "Total resights by year" = "tot_by_year"),
                               selected = NULL)
                )
              )
            ),
            box(
              title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(3, selectInput("tr_season_min", label = tags$h5("Minimum season"),
                                      choices = season.list, selected = season.list.id.max)),
                column(3, selectInput("tr_season_max", label = tags$h5("Maximum season"),
                                      choices = season.list, selected = season.list.id.max))
              )
            )
          )
        )
      )
    )
  )
}
