## calculate  x plus y for integers

plus <-
  function( x, y ) {
    res <- outer( as.integer( x ), as.integer( y ),
                  function( xx , yy )
                    as.integer( xx + yy ))
    dimnames(res) <-
      list( formatC( x, digits = 0, width = max( nchar( x ) ) ),
            formatC( y, digits = 0, width = max( nchar( y ) ) ))
    res
  }