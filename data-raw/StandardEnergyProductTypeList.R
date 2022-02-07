library( magrittr )
## reading raw tsv
StandardEnergyProductTypeList     <- data.table::fread( file = "data-raw/StandardEnergyProductTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardEnergyProductTypeList ),
        FUN = function( col ) {
          if( is.character( StandardEnergyProductTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardEnergyProductTypeList,
                             j     = col,
                             value = trimws( x     = StandardEnergyProductTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardEnergyProductTypeList,
                 j     = vapply( X   = StandardEnergyProductTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardEnergyProductTypeList, overwrite = TRUE )
