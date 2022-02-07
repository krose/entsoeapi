library( magrittr )
## reading raw tsv
StandardCodingSchemeTypeList     <- data.table::fread( file = "data-raw/StandardCodingSchemeTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardCodingSchemeTypeList ),
        FUN = function( col ) {
          if( is.character( StandardCodingSchemeTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardCodingSchemeTypeList,
                             j     = col,
                             value = trimws( x     = StandardCodingSchemeTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardCodingSchemeTypeList,
                 j     = vapply( X   = StandardCodingSchemeTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardCodingSchemeTypeList, overwrite = TRUE )
