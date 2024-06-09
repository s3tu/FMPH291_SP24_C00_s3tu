## File: lab2.R
## Author: Shengjia Tu
## Date: 2024-Apr-08
## Purpose: FMPH 291 Statistical Computing using R - lab 2 exercises 


hypot1 <- function(x,y) sqrt(x ^ 2 + y ^ 2)

hypot1(0, 0)# result=0
hypot1(6, 8)# result=10
hypot1(1:10, 20)# result 20.02498 20.09975 20.22375 20.39608 20.61553 20.88061 21.18962 21.54066 21.93171 22.36068


hypot1(3*10^3,4*10^3)# result=5000

hypot1(3e300, 4e300)# result=Inf
#The result of hypot1(3e300, 4e300) should be 5e300, but when we run it in r, it returns inf.


# Say why the value you computed and that of `hypot1` differ.
# Answer: When I compute this, I divided x and y into 2 parts, first calculate the hypot of 3 and 4 then add e300 behind.
# hypot1 use a different way. It calculates the square of 3e300 first, the result exceeds the largest representable number in r, so the calculation cannot continue.


# Figure the value when x=3×10−161 and y=4×10−161 and put it in a comment.
# the expected value should be 5e-161


hypot1(3e-161, 4e-161) # the result is 4.999972e-161
# The rough idea is, the square of 3e-161 and 4e-161 are still larger than the smallest representable number in r so the calculation can continue.
# Using > ?.Machine, the smallest non-zero normalized floating-point number is 2.225074e-308.
# Through a rough testing, hypot1(3e-163, 4e-163) returns 0.


hypot2 <- function(x, y) exp(1/2*(log( ((x/sqrt(1e-100+abs(x+y)))^2 + (y/sqrt(1e-100+abs(x+y)))^2) ) + log(1e-100+abs(x+y))))
hypot2(3e300, 4e300)# 5e+300
hypot2(3e-161, 4e-161) # 5e-161
hypot2( 3 * (0:10), -4 * (0:10))# [1]  0  5 10 15 20 25 30 35 40 45 50
all.equal(hypot2( 3 * (0:10), -4 * (0:10)),hypot1( 3 * (0:10), -4 * (0:10)))
# TRUE


logSumExp1 <- function(x,y) log( exp( x ) + exp( y ))

logSumExp1(1, 1)# result=1.693147

logSumExp1(100, 100)# result=100.6931

bigButFinite <- log(.Machine$double.xmax)# bigButFinite=709.7827

logSumExp1(bigButFinite-1, bigButFinite-1)# result=709.4759

logSumExp1(bigButFinite, bigButFinite) # result=Inf
# the result of last expression is inf is because it calculates exp() first, which is larger than the system can deal with,
#so it returns inf

# log(exp(x)+exp(y)) can be simplifed to x+log(1+exp(y-x)), letting exp(y)=exp(x)*exp(y-x)
# so v=x, w=exp(y-x)
logSumExp2 <- function(x,y)
{
  x1=pmax(x,y)
  y1=pmin(x,y)
  return(x1+log(1+exp(y1-x1)))
}
logSumExp2(1, 1)# result=1.693147, same as logSumExp1
logSumExp2(100, 100)# result=100.6931, same as logSumExp1
logSumExp2(bigButFinite-1, bigButFinite-1)# result=709.4759, same as logSumExp1
logSumExp2(bigButFinite, bigButFinite)# result=710.4759, reasonable, correspond with theoratical result


# read in the data
data_string <- "
x          y
1          1 
1       1000 
1000          1 
10         20 
100        200 
2e50       3e50
"
data=read.table(text = data_string,header=TRUE)
do.call(logSumExp2,data)
# the result is [1] 1.693147e+00 1.000000e+03 1.000000e+03 2.000005e+01 2.000000e+02 3.000000e+50
do.call(logSumExp1,data)
# the result is [1]   1.693147        Inf        Inf  20.000045 200.000000        Inf