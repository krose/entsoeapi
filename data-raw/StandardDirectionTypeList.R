library( magrittr )
## reading raw tsv
StandardDirectionTypeList     <- data.table::fread( file = "data-raw/StandardDirectionTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardDirectionTypeList ),
        FUN = function( col ) {
          if( is.character( StandardDirectionTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardDirectionTypeList,
                             j     = col,
                             value = trimws( x     = StandardDirectionTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardDirectionTypeList,
                 j     = vapply( X   = StandardDirectionTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardDirectionTypeList, overwrite = TRUE )
