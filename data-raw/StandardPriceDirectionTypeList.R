library( magrittr )
## reading raw tsv
StandardPriceDirectionTypeList     <- data.table::fread( file = "data-raw/StandardPriceDirectionTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardPriceDirectionTypeList ),
        FUN = function( col ) {
          if( is.character( StandardPriceDirectionTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardPriceDirectionTypeList,
                             j     = col,
                             value = trimws( x     = StandardPriceDirectionTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardPriceDirectionTypeList,
                 j     = vapply( X   = StandardPriceDirectionTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardPriceDirectionTypeList, overwrite = TRUE )
