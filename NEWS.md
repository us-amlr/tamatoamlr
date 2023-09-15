# amlrPinnipeds 0.5.0.9000

* Requires [amlrDatabases](https://github.com/us-amlr/amlrDatabases) >= v0.7.0. Specifically, `amlrDatabases::mod_output_server` now does not take `parent` argument

* Updated afs_capewide_pup_census module to handle new SQL table structures, and added new plots and displays for single and multiple seasons

* Added `afs_capewide_pup` functions


# amlrPinnipeds 0.4.0

* amlrPinnipeds now depends on the following additional packages: [hms](https://github.com/tidyverse/hms), [forcats](https://github.com/tidyverse/forcats), and [stringi](https://github.com/gagolews/stringi)

* Updated `mutate_location` to keep in step with name column in ***REMOVED***.beaches

* `mutate_location` now uses `stringi::stri_escape_unicode` to escape all non-ASCII characters

* Added `csphoc.core.location.groups` variable to define core census locations for CS-PHOC data paper

* Added 'census_counts' functions `sum_count` and `total_count` to handle common situations when working with US AMLR Pinniped census data

* Added `csphoc_complete_aggregated` to complete aggregated US AMLR CS-PHOC data, by header ID and species


# amlrPinnipeds 0.3.0

* Requires [amlrDatabases](https://github.com/us-amlr/amlrDatabases) >= v0.6.0

* Added Captures tab and associated functionality from [@karensnyder](https://github.com/karensnyder)

* Added `mutate_location` for standardizing Cape Shirreff locations 

* Added `arrange_tag` for nicely sorting pinniped tag numbers

* Added extract (`tbl_...`) functions for consistent extraction of data from the ***REMOVED*** database

* Added lots of additional functionality to Tamatoa during the 2022-23 field season, including: 

  - Phocid Census tab; specifically to work with new database table structure
  - AFS Study Beach Census tab
  - AFS Capewide Pup Census +/- five percent check
  - DCC tab: process DCC files from current season


# amlrPinnipeds 0.2.0

* Update database connection management to keep up with amlrDatabases v0.4.0

* Views now must have 'location' and 'location_group' variables

* Currently only the phocid census tab is displayed; other module files have been temporarily removed to facilitate testing and package checking


# amlrPinnipeds 0.1.0

* Initial release, 2021/22 field season
