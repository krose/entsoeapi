library( magrittr )
## reading raw tsv
StandardTarifTypeTypeList     <- data.table::fread( file = "data-raw/StandardTarifTypeTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardTarifTypeTypeList ),
        FUN = function( col ) {
          if( is.character( StandardTarifTypeTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardTarifTypeTypeList,
                             j     = col,
                             value = trimws( x     = StandardTarifTypeTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardTarifTypeTypeList,
                 j     = vapply( X   = StandardTarifTypeTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardTarifTypeTypeList, overwrite = TRUE )
