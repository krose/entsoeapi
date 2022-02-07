library( magrittr )
## reading raw tsv
StandardDocumentTypeList     <- data.table::fread( file = "data-raw/StandardDocumentTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardDocumentTypeList ),
        FUN = function( col ) {
          if( is.character( StandardDocumentTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardDocumentTypeList,
                             j     = col,
                             value = trimws( x     = StandardDocumentTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardDocumentTypeList,
                 j     = vapply( X   = StandardDocumentTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardDocumentTypeList, overwrite = TRUE )
