library( magrittr )
## reading raw tsv
StandardAllocationModeTypeList     <- data.table::fread( file = "data-raw/StandardAllocationModeTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardAllocationModeTypeList ),
        FUN = function( col ) {
          if( is.character( StandardAllocationModeTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardAllocationModeTypeList,
                             j     = col,
                             value = trimws( x     = StandardAllocationModeTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardAllocationModeTypeList,
                 j     = vapply( X   = StandardAllocationModeTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardAllocationModeTypeList, overwrite = TRUE )
