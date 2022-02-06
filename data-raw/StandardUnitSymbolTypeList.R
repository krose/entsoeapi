library( magrittr )
## reading raw tsv
StandardUnitSymbolTypeList     <- data.table::fread( file = "data-raw/StandardUnitSymbolTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardUnitSymbolTypeList ),
        FUN = function( col ) {
          if( is.character( StandardUnitSymbolTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardUnitSymbolTypeList,
                             j     = col,
                             value = trimws( x     = StandardUnitSymbolTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardUnitSymbolTypeList,
                 j     = vapply( X   = StandardUnitSymbolTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardUnitSymbolTypeList, overwrite = TRUE )
