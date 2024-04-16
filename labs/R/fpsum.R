# write a function
fpSum=function(a, b, digits=0)
{
  return(signif(a+b,digits = digits))
}

fpSum( 1.351, 2.534, digits = 2 )


# Generate two vectors of uniform random numbers of length 50 in the range [0, 1]
x=runif(50)
y=runif(50)
# call the function with the digits argument equal to 2
sumxysignif=fpSum( x, y, digits = 2 )
# calculate x+y
sumxy=x+y
# calculate absolute difference
Ab=abs(sumxysignif-sumxy)
# plot the histogram
hist(Ab)
# plot the density
dens=density(Ab)
plot(dens)


# guess that the maximum value would be 0.05
sort(Ab)[50] # the largest value is 0.049175, close to guess
# increase length of x and y and do the above again
x1=runif(10000)
y1=runif(10000)
sumxysignif1=fpSum( x1, y1, digits = 2 )
sumxy1=x1+y1
Ab1=abs(sumxysignif1-sumxy1)
sort(Ab1)[10000]# the largest value is 0.04997438, it's quite close to the guess


# repeat with digits = 3
x2=runif(10000)
y2=runif(10000)
sumxysignif2=fpSum( x2, y2, digits = 3 )
sumxy2=x2+y2
Ab2=abs(sumxysignif2-sumxy2)
sort(Ab2)[10000]# the largest value is 0.004998753, I expected it to be 0.005 when x and y are infinitely long


# repeat with digits = 4
x3=runif(10000)
y3=runif(10000)
sumxysignif3=fpSum( x3, y3, digits = 4 )
sumxy3=x3+y3
Ab3=abs(sumxysignif3-sumxy3)
sort(Ab3)[10000]# the largest value is 0.0004999091, I expected it to be 0.0005 when x and y are infinitely long


# In the above simulations, beta correspond to 10, and d correspond to number of digits, the same as digits in the above simulations.


# Use your fpSum function to add z to the result of fpSum( x, y, digits = 2) and return values with 2 significant digits. 
z=runif(10000)
sumxyzsignif=signif(z+sumxysignif1,digits = 2)
sumxyz=x1+y1+z
Ab4=abs(sumxyzsignif-sumxyz)
# plot the density
dens1=density(Ab4)
plot(dens1)
sort(Ab4)[10000]# the result is 0.09903581, the maximum value is approximately 0.1 when the vectors are infinitely long


# digits = 3
sumxyzsignif1=signif(z+sumxysignif2,digits = 3)
sumxyz1=x2+y2+z
Ab5=abs(sumxyzsignif1-sumxyz1)
sort(Ab5)[10000]# the result is 1.79684, I don't know why it's so strange
# expand the length to 20000
X3=runif(20000)
Y3=runif(20000)
Z=runif(20000)
sumXYsignif3=fpSum( X3, Y3, digits = 3 )
sumXYZsignif3=signif(Z+sumXYsignif3,digits = 3)
sumXYZ3=X3+Y3+Z
Ab6=abs(sumXYZsignif3-sumXYZ3)
sort(Ab6)[20000]# the result is 0.009826782,the maximum value is approximately 0.01 when the vectors are infinitely long


# digits = 4
sumxyzsignif2=signif(z+sumxysignif3,digits = 4)
sumxyz2=x3+y3+z
Ab7=abs(sumxyzsignif2-sumxyz2)
sort(Ab7)[10000]# the result is 0.0009759455, the maximum value is approximately 0.001 when the vectors are infinitely long

# I guess the bound would be (k-1)*machine epsilon. k is the number of vectors.

