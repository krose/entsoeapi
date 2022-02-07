library( magrittr )
## reading raw tsv
StandardObjectAggregationTypeList     <- data.table::fread( file = "data-raw/StandardObjectAggregationTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardObjectAggregationTypeList ),
        FUN = function( col ) {
          if( is.character( StandardObjectAggregationTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardObjectAggregationTypeList,
                             j     = col,
                             value = trimws( x     = StandardObjectAggregationTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardObjectAggregationTypeList,
                 j     = vapply( X   = StandardObjectAggregationTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardObjectAggregationTypeList, overwrite = TRUE )
