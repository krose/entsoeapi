library( magrittr )
## reading raw tsv
StandardCurrencyTypeList     <- data.table::fread( file = "data-raw/StandardCurrencyTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardCurrencyTypeList ),
        FUN = function( col ) {
          if( is.character( StandardCurrencyTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardCurrencyTypeList,
                             j     = col,
                             value = trimws( x     = StandardCurrencyTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardCurrencyTypeList,
                 j     = vapply( X   = StandardCurrencyTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardCurrencyTypeList, overwrite = TRUE )
