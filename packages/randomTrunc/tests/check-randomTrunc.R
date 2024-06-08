library(randomTrunc)

## check the integral of dtrunc3
integrate( function(x){dtrunc3(x , 1)}, -Inf, Inf)

# check dtrunc3 by graphs
hist( rtrunc4( 10000, 1.5, 1e-6, 2.0, 1.0), breaks = 100, freq = FALSE )
curve( dtrunc3( x, 1.5, 2.0, 1.0), 0, 5, add = TRUE, col = "red")

## check the integral of dtrunc4
integrate( function(x){dtrunc4(x, 1.5, 0.1)}, -Inf, Inf)

# check dtrunc4 by graphs
hist( rtrunc4( 10000, 1.5, 0.1, 2.0, 1.0), breaks = 100, freq = FALSE )
curve( dtrunc4( x, 1.5, 0.1, 2.0, 1.0), 0, 5, add = TRUE, col = "red")

# check maxlik, which involves loglik3 and loglik4
x <- rtrunc4(10000, 1.5, 1e-6, 2.0, 1.2)
maxlik(x, model = "three", parm = c(1.0, 4, 1.0))
maxlik(x, model = "three", parm = c(1.0, 0.1, 1.0, 1.0))
x <- rtrunc4(10000, 1.5, 0.1, 2, 1)
maxlik(x, model = "four", parm = c(1.0, 0.1, 0, 1.0))
