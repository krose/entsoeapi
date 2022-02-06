library( magrittr )
## reading raw tsv
StandardQualityTypeList     <- data.table::fread( file = "data-raw/StandardQualityTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardQualityTypeList ),
        FUN = function( col ) {
          if( is.character( StandardQualityTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardQualityTypeList,
                             j     = col,
                             value = trimws( x     = StandardQualityTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardQualityTypeList,
                 j     = vapply( X   = StandardQualityTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardQualityTypeList, overwrite = TRUE )
