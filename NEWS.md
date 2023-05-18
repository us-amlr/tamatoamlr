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
