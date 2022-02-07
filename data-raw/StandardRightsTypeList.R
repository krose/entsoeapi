library( magrittr )
## reading raw tsv
StandardRightsTypeList     <- data.table::fread( file = "data-raw/StandardRightsTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardRightsTypeList ),
        FUN = function( col ) {
          if( is.character( StandardRightsTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardRightsTypeList,
                             j     = col,
                             value = trimws( x     = StandardRightsTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardRightsTypeList,
                 j     = vapply( X   = StandardRightsTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardRightsTypeList, overwrite = TRUE )
