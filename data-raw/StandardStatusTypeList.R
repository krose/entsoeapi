library( magrittr )
## reading raw tsv
StandardStatusTypeList     <- data.table::fread( file = "data-raw/StandardStatusTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardStatusTypeList ),
        FUN = function( col ) {
          if( is.character( StandardStatusTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardStatusTypeList,
                             j     = col,
                             value = trimws( x     = StandardStatusTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardStatusTypeList,
                 j     = vapply( X   = StandardStatusTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardStatusTypeList, overwrite = TRUE )
