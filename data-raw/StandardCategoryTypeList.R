library( magrittr )
## reading raw tsv
StandardCategoryTypeList     <- data.table::fread( file = "data-raw/StandardCategoryTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardCategoryTypeList ),
        FUN = function( col ) {
          if( is.character( StandardCategoryTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardCategoryTypeList,
                             j     = col,
                             value = trimws( x     = StandardCategoryTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardCategoryTypeList,
                 j     = vapply( X   = StandardCategoryTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardCategoryTypeList, overwrite = TRUE )
