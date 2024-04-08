print("hello world")
library(Rcpp)
cppFunction('int oneplus( int x ) {
    x++;
    return x;
          }') 
vecOneplus <- Vectorize(oneplus)
vecOneplus( 1:5 )
all.equal( vecOneplus( 1:5 ), 2:6)