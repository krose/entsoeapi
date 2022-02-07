library( magrittr )
## reading raw tsv
StandardContractTypeList     <- data.table::fread( file = "data-raw/StandardContractTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardContractTypeList ),
        FUN = function( col ) {
          if( is.character( StandardContractTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardContractTypeList,
                             j     = col,
                             value = trimws( x     = StandardContractTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardContractTypeList,
                 j     = vapply( X   = StandardContractTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardContractTypeList, overwrite = TRUE )
