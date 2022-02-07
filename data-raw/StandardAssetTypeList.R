library( magrittr )
## reading raw tsv
StandardAssetTypeList     <- data.table::fread( file = "data-raw/StandardAssetTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardAssetTypeList ),
        FUN = function( col ) {
          if( is.character( StandardAssetTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardAssetTypeList,
                             j     = col,
                             value = trimws( x     = StandardAssetTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardAssetTypeList,
                 j     = vapply( X   = StandardAssetTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardAssetTypeList, overwrite = TRUE )
