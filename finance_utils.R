# approximate net present value of a bond
bond.npv <- function(principal,duration,coupon.rate,discount.rate) {
  principal.npv <- principal / (1+discount.rate)**duration
  coupon.npv <- 0
  for (i in seq(1,duration,1)) {
    coupon.npv <- coupon.npv + principal*coupon.rate / (1+discount.rate)**i
  }
  return(principal.npv + coupon.npv)
}

# bond.npv(1000,10,0.04,0.04)
# bond.npv(1000,10,0.04,0.05)
# bond.npv(1000,10,0.04,0.03)