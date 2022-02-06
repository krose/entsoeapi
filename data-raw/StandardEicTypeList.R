library( magrittr )
## reading raw tsv
StandardEicTypeList     <- data.table::fread( file = "data-raw/StandardEicTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardEicTypeList ),
        FUN = function( col ) {
          if( is.character( StandardEicTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardEicTypeList,
                             j     = col,
                             value = trimws( x     = StandardEicTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardEicTypeList,
                 j     = vapply( X   = StandardEicTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardEicTypeList, overwrite = TRUE )
