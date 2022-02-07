library( magrittr )
## reading raw tsv
StandardReasonCodeTypeList     <- data.table::fread( file = "data-raw/StandardReasonCodeTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardReasonCodeTypeList ),
        FUN = function( col ) {
          if( is.character( StandardReasonCodeTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardReasonCodeTypeList,
                             j     = col,
                             value = trimws( x     = StandardReasonCodeTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardReasonCodeTypeList,
                 j     = vapply( X   = StandardReasonCodeTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardReasonCodeTypeList, overwrite = TRUE )
