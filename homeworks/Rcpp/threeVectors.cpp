#include <Rcpp.h>
using namespace Rcpp;
// Demonstrate how to `Vectorize' 3 arguments.
// Print the indexes and the element indexed.
// `void' type needs no `return' statement
// Rprintf is described in Writing R Extensions Section 6.5 Printing
void print3Values( int i, int ix, int iy, int iz, double xelt, double yelt, double zelt) {
  Rprintf("i = %d ix = %d x element = %f iy = %d y element = %f iz = %d z element = %f\n",
          i, ix, xelt, iy, yelt, iz, zelt);
}
// [[Rcpp::export]]
CharacterVector threeVectors(
    NumericVector x, NumericVector y, NumericVector z) {
  int xn = x.size();
  int yn = y.size();
  int zn = z.size();
  // if (xn == 0 || yn == 0 || zn == 0) special handling is needed such as
  // returning a zero length vector
  if (xn==0L || yn == 0L || zn == 0L)
    Rprintf(" Zero Length Detected: xn == %d, yn ==%d\n, zn ==%d\n", xn, yn, zn);
  int n = xn > yn ? xn : yn;
  n = n > zn ? n : zn;
  int ix=0, iy=0, iz=0;
  for (int i = 0; i<n; ++i) {
    // Do something here:
    print3Values( i, ix, iy, iz, x[ix], y[iy], z[iz]);
    // Update the indexes for the next pass:
    if (++ix == xn ) ix = 0;
    if (++iy == yn ) iy = 0;
    if (++iz == zn ) iz = 0;
  }
  return "Done";
}