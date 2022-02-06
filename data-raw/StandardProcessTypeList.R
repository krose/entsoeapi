library( magrittr )
## reading raw tsv
StandardProcessTypeList     <- data.table::fread( file = "data-raw/StandardProcessTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardProcessTypeList ),
        FUN = function( col ) {
          if( is.character( StandardProcessTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardProcessTypeList,
                             j     = col,
                             value = trimws( x     = StandardProcessTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardProcessTypeList,
                 j     = vapply( X   = StandardProcessTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardProcessTypeList, overwrite = TRUE )
