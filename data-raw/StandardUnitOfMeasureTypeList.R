library( magrittr )
## reading raw tsv
StandardUnitOfMeasureTypeList     <- data.table::fread( file = "data-raw/StandardUnitOfMeasureTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardUnitOfMeasureTypeList ),
        FUN = function( col ) {
          if( is.character( StandardUnitOfMeasureTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardUnitOfMeasureTypeList,
                             j     = col,
                             value = trimws( x     = StandardUnitOfMeasureTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardUnitOfMeasureTypeList,
                 j     = vapply( X   = StandardUnitOfMeasureTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardUnitOfMeasureTypeList, overwrite = TRUE )
