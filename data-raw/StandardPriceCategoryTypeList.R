library( magrittr )
## reading raw tsv
StandardPriceCategoryTypeList     <- data.table::fread( file = "data-raw/StandardPriceCategoryTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardPriceCategoryTypeList ),
        FUN = function( col ) {
          if( is.character( StandardPriceCategoryTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardPriceCategoryTypeList,
                             j     = col,
                             value = trimws( x     = StandardPriceCategoryTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardPriceCategoryTypeList,
                 j     = vapply( X   = StandardPriceCategoryTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardPriceCategoryTypeList, overwrite = TRUE )
