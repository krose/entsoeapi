library( magrittr )
## reading raw tsv
StandardClassificationTypeList     <- data.table::fread( file = "data-raw/StandardClassificationTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardClassificationTypeList ),
        FUN = function( col ) {
          if( is.character( StandardClassificationTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardClassificationTypeList,
                             j     = col,
                             value = trimws( x     = StandardClassificationTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardClassificationTypeList,
                 j     = vapply( X   = StandardClassificationTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardClassificationTypeList, overwrite = TRUE )
