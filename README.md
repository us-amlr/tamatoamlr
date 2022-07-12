# amlrPinnipeds

<!-- badges: start -->

<!-- badges: end -->

amlrPinnipeds and Tamatoa, its accompanying Shiny app, are for analyzing and visualizing data collected by the [AERD](https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center) [Pinniped Research Program](https://www.fisheries.noaa.gov/international/science-data/pinniped-research-antarctic). USA

## Installation

You can install the development version of amlrPinnipeds from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("us-amlr/amlrPinnipeds")
```

## Tamatoa: Shiny app

To run Tamatoa, the amlrPinnipeds Shiny app, locally, you currently must have [RStudio](https://www.rstudio.com/products/rstudio/download/#download) installed. From within RStudio, install amlrPinnipeds as described above, and then run the following code in your RStudio console:

``` r
amlrPinnipeds::amlr_pinnipeds_gui()
```

By default, Tamatoa attempts to connect to the database on the SWFSC server. For it to connect, you must be logged into VPN on whatever computer you are using to run the app. You can also choose to connect to a local copy of the database, if appropriate.

### Shiny app overview and guiding principles, for developers:

* mod_database_server(), from [amlrDatabases](https://github.com/us-amlr/amlrDatabases), returns the connection to the user-specified database via a [pool](https://github.com/rstudio/pool) object that is used by the rest of the amlrPinnipeds Shiny App modules. Pool connections to the four possible databases (remote/local and AMLR_PINNIPEDS/AMLR_PINNIPEDS_Test) are generated at the top of the amlrPinnipeds app.R code, and then passed to mod_database_server. 

* mod_season_info_server returns the season information data for the rest of the modules

* The 'Database and season info' tab relies on mod_database and mod_season_info. The rest of the tabs all have a dedicated module that takes at least the pool object from mod_database, and generally season.df and season.id.list from mod_season_info_server, as inputs. These modules pass both a table and a plot to mod_output_server from  [amlrDatabases](https://github.com/us-amlr/amlrDatabases), which displays the table and plot along with associated visualization and download options. These modules generally depend on views created in the database.
TODO: this needs a system that is more robust to changes in the SQL server views. For instance, each view has a function that checks for required column names, and returns a df with standardized capitalization things?

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
