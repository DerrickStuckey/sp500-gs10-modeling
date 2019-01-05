# financial calculations utilities

# net present value of a bond with specified principal, remaining duration, coupon rate, and discount rate
def bond_npv(principal,duration,coupon_rate,discount_rate):
  principal_npv = principal / (1+discount_rate)**duration
  coupon_npv = 0
  for i in range(1,duration+1,1):
    coupon_npv = coupon_npv + principal*coupon_rate / (1+discount_rate)**i
  return(principal_npv + coupon_npv)
#TODO this is wrong, gives negative NPV when the interest rate stays the same

# def bond_return(duration,start_coupon_rate,)