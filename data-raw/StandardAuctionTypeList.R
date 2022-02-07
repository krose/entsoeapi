library( magrittr )
## reading raw tsv
StandardAuctionTypeList     <- data.table::fread( file = "data-raw/StandardAuctionTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardAuctionTypeList ),
        FUN = function( col ) {
          if( is.character( StandardAuctionTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardAuctionTypeList,
                             j     = col,
                             value = trimws( x     = StandardAuctionTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardAuctionTypeList,
                 j     = vapply( X   = StandardAuctionTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardAuctionTypeList, overwrite = TRUE )
