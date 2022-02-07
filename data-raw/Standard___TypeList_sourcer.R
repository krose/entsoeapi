lapply( X   = fs::dir_ls( path = "data-raw", regexp = "\\/Standard.+TypeList\\.R" ),
        FUN = source )
