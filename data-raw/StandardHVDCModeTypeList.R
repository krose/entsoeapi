library( magrittr )
## reading raw tsv
StandardHVDCModeTypeList     <- data.table::fread( file = "data-raw/StandardHVDCModeTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardHVDCModeTypeList ),
        FUN = function( col ) {
          if( is.character( StandardHVDCModeTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardHVDCModeTypeList,
                             j     = col,
                             value = trimws( x     = StandardHVDCModeTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardHVDCModeTypeList,
                 j     = vapply( X   = StandardHVDCModeTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardHVDCModeTypeList, overwrite = TRUE )
