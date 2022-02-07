library( magrittr )
## reading raw tsv
StandardPaymentTermsTypeList     <- data.table::fread( file = "data-raw/StandardPaymentTermsTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardPaymentTermsTypeList ),
        FUN = function( col ) {
          if( is.character( StandardPaymentTermsTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardPaymentTermsTypeList,
                             j     = col,
                             value = trimws( x     = StandardPaymentTermsTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardPaymentTermsTypeList,
                 j     = vapply( X   = StandardPaymentTermsTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardPaymentTermsTypeList, overwrite = TRUE )
