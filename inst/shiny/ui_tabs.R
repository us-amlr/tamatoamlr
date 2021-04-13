#--------------------------------------------------------------------------
ui_tab_info <- function() {
  tabItem(
    tabName = "tab_info",
    fluidRow(
      box(
        title = "Database connection information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tableOutput("pool_db_conn"),
        tags$br(),
        selectInput("info_db_name", tags$h5("Database..."), width = "200px",
                    choices = list("***REMOVED***" = "remote_prod",
                                   "***REMOVED***_Test" = "remote_test",
                                   "Local database" = "local"),
                    selected = "remote_prod")
      ),
      box(
        title = "Season information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tableOutput("info_season_info")
      )
    )
  )
}

#--------------------------------------------------------------------------
ui_tab_afs_diet <- function() {
  tabItem(
    tabName = "tab_afs_diet",
    fluidRow(
    )
  )
}

#--------------------------------------------------------------------------
ui_tab_afs_natal <- function() {
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
              column(4, radioButtons("afs_natal_type", label = tags$h5("Plot to display"),
                                     choices = list("Overview" = "overview",
                                                    "Natality rate" = "natal_rate",
                                                    "Proportional loss type" = "prop_loss"),
                                     selected = NULL)),
              column(4, checkboxInput("afs_natal_prime", label = "Plot prime age", value = FALSE))
            )
          ),
          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              column(4, selectInput("afs_natal_season_min", label = tags$h5("Minimum season"),
                                    choices = season.list, selected = season.list.id.max)),
              column(4, selectInput("afs_natal_season_max", label = tags$h5("Maximum season"),
                                    choices = season.list, selected = season.list.id.max))
            )
          )
        )
      )
    )
  )
}

#--------------------------------------------------------------------------
ui_tab_census <- function() {
  tabItem(
    tabName = "tab_census",
    fluidRow(
      column(
        width = 6,
        fluidRow(
          box(
            status = "primary", width = 12,
            plotOutput("census_plot"),
            tags$br(), tags$br(),
            uiOutput("census_warning_na_records")
          ),
          box(
            status = "primary", width = 12,
            tags$h5("This table shows the data displayed in the plot above.",
                    "Note that all rows with only counts of zero for the selected columns have been filtered out."),
            DTOutput("census_tbl")
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          box(
            title = "Summary ...", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            tags$h5("This tab allows you to summarize and visualize census data. Select your census type and ",
                    "how you wish to summarize this data, and then any filters you would like to apply"),
            fluidRow(
              column(
                width = 4,
                radioButtons("census_type", label = NULL, #tags$h5("Census type"),
                             choices = list("AFS pup census" = "afs_pup",
                                            "AFS study beach census" = "afs_study_beach",
                                            "Phocid census" = "phocid")
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                radioButtons("census_summary_level_1", label = tags$h5("Summary level 1"),
                             choices = list("Multiple seasons - total" = "fs_multiple_total",
                                            "Multiple seasons - weekly" = "fs_multiple_week",
                                            "Single season" = "fs_single"),
                             selected = "fs_multiple_total")
              ),
              column(
                width = 4,
                radioButtons("census_summary_level_2", label = tags$h5("Summary level 2"),
                             choices = list("By beach" = "by_beach",
                                            "Cape - wide" = "by_capewide"),
                             selected = "by_capewide")
              ),
              column(
                width = 4,
                radioButtons("census_summary_level_3", label = tags$h5("Summary level 3"),
                             choices = list("By species and age+sex class" = "by_sp_age_sex",
                                            "By species" = "by_sp"),
                             selected = "by_sp_age_sex")
              )
            ),
            conditionalPanel(
              condition = "input.census_summary_level_1 == 'fs_single'",
              checkboxInput("census_cumsum", "Plot cumulative sum (doesn't do stuff yet)", value = FALSE)
            ),
            tags$br(), tags$br(),
            tags$h5("Todo?: descriptive text about what the above choices 'mean' in terms of what is plotted"),
          ),

          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              conditionalPanel(
                condition = "input.census_summary_level_1 != 'fs_single'",
                column(
                  width = 12,
                  fluidRow(seasoninfo_mod_ui("census", 4)),
                  # column(
                  #   width = 4,
                  #   selectInput("census_season_min", label = tags$h5("Minimum season"),
                  #               choices = season.list, selected = season.list.id.min),
                  #   conditionalPanel(
                  #     condition = "input.census_summary_level_1 == 'fs_multiple_week'",
                  #     selectInput("census_week_num", tags$h5("Week number"), choices = list(), selected = NULL)
                  #   )
                  # ),
                  # column(4, selectInput("census_season_max", label = tags$h5("Maximum season"),
                  #                       choices = season.list, selected = season.list.id.max))
                  fluidRow(
                    column(
                      width = 4,
                      conditionalPanel(
                        condition = "input.census_summary_level_1 == 'fs_multiple_week'",
                        selectInput("census_week_num", tags$h5("Week number"), choices = list(), selected = NULL)
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.census_summary_level_1 == 'fs_single'",
                column(4, selectInput("census_season_select", label = tags$h5("Select season"), choices = NULL)),
                column(4, dateRangeInput("census_date_range", label = tags$h5("Date range"))) #Updated in observe() based on selected season
              ),

              column(
                width = 3, offset = 1,
                conditionalPanel(
                  condition = "input.census_type == 'phocid'",
                  checkboxGroupInput("census_species", label = tags$h5("Species"),
                                     choices = pinniped.sp.list.phocid,
                                     selected = unname(unlist(pinniped.sp.list.phocid)))
                )
              )
            ),
            uiOutput("census_age_sex_uiOut_selectize"),
            uiOutput("census_beach_uiOut_selectize")
          )
        )
      )
    )
  )
}

#--------------------------------------------------------------------------
ui_tab_tr <- function() {
  tabItem(
    tabName = "tab_tr",
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
              column(4, radioButtons("tr_type", label = tags$h5("Data to plot"),
                                     choices = list("Individuals by year" = "ind_by_year",
                                                    "Total resights by year" = "tot_by_year"),
                                     selected = NULL))
            )
          ),
          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              seasoninfo_mod_ui("tag_resights", 4),
              # column(4, selectInput("tr_season_min", label = tags$h5("Minimum season"),
              #                       choices = season.list, selected = season.list.id.min)),
              # column(4, selectInput("tr_season_max", label = tags$h5("Maximum season"),
              #                       choices = season.list, selected = season.list.id.max)),
              column(4, checkboxGroupInput("tr_species", label = tags$h5("Species"),
                                           choices = pinniped.sp.list.tr,
                                           selected = "fur seal")),

            )
          )
        )
      )
    )
  )
}
