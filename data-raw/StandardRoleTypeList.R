library( magrittr )
## reading raw tsv
StandardRoleTypeList     <- data.table::fread( file = "data-raw/StandardRoleTypeList.tsv", encoding = "UTF-8" )

## trimming character columns
lapply( X   = names( x = StandardRoleTypeList ),
        FUN = function( col ) {
          if( is.character( StandardRoleTypeList[[ col ]] ) ) {
            data.table::set( x     = StandardRoleTypeList,
                             j     = col,
                             value = trimws( x     = StandardRoleTypeList[[ col ]],
                                             which = "both" ) )
          }
        } )

## removing empty columns
data.table::set( x     = StandardRoleTypeList,
                 j     = vapply( X   = StandardRoleTypeList,
                                 FUN = collapse::allNA,
                                 FUN.VALUE = TRUE ) %>% which() %>% names(),
                 value = NULL )

## save package data in the correct format
usethis::use_data( StandardRoleTypeList, overwrite = TRUE )
