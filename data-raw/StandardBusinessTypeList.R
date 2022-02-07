library( magrittr )
## reading raw tsv
StandardBusinessTypeList     <- data.table::fread( file = "data-raw/StandardBusinessTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardBusinessTypeList ),
        FUN = function( col ) {
          if( is.character( StandardBusinessTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardBusinessTypeList,
                             j     = col,
                             value = trimws( x     = StandardBusinessTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardBusinessTypeList,
                 j     = vapply( X   = StandardBusinessTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardBusinessTypeList, overwrite = TRUE )
