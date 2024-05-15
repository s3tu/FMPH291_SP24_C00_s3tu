library(pareto)
stopifnot(all.equal(dpareto(3,2,1), 0.2222222222))
stopifnot(all.equal(dpareto(1,2,3), 0.0))
stopifnot(all.equal(dpareto(3:5,2, 1), c(0.2222222222, 0.1250000, 0.0800000)))
stopifnot(all.equal(dpareto(6,2:4, 1), c(0.05555555556, 0.08333333333, 0.11111111111)))
stopifnot(all.equal(dpareto(6,1,2:4), c(0.0092592593, 0.0023148148, 0.0005144033)))
stopifnot(all.equal(dpareto(1, 2, 1:2), c(0, 0)))


stopifnot(all.equal(ppareto(3:5,2, 1),c(0.3333333, 0.5000000, 0.6000000), tolerance = 1e-6))
stopifnot(all.equal(ppareto(1,2, 1),0))
stopifnot(all.equal(ppareto(0,0, 1),NaN))

stopifnot(all.equal(qpareto(1,1,1),Inf))
stopifnot(all.equal(qpareto(0.5,1,2), 1.414214, tolerance = 1e-6))