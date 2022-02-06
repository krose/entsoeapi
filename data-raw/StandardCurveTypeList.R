library( magrittr )
## reading raw tsv
StandardCurveTypeList     <- data.table::fread( file = "data-raw/StandardCurveTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardCurveTypeList ),
        FUN = function( col ) {
          if( is.character( StandardCurveTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardCurveTypeList,
                             j     = col,
                             value = trimws( x     = StandardCurveTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardCurveTypeList,
                 j     = vapply( X   = StandardCurveTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardCurveTypeList, overwrite = TRUE )
