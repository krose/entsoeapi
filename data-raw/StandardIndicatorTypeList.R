library( magrittr )
## reading raw tsv
StandardIndicatorTypeList     <- data.table::fread( file = "data-raw/StandardIndicatorTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardIndicatorTypeList ),
        FUN = function( col ) {
          if( is.character( StandardIndicatorTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardIndicatorTypeList,
                             j     = col,
                             value = trimws( x     = StandardIndicatorTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardIndicatorTypeList,
                 j     = vapply( X   = StandardIndicatorTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardIndicatorTypeList, overwrite = TRUE )
