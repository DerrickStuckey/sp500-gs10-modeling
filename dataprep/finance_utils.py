# financial calculations utilities

# net present value of a bond with specified principal, remaining duration, coupon rate, and discount rate
# assumes flat yield curve (new discount rate is valid at all durations)
def bond_npv(principal,duration,coupon_rate,discount_rate):
  principal_npv = principal / (1+discount_rate)**duration
  coupon_npv = principal * coupon_rate / discount_rate * (1 - 1/(1+discount_rate)**(duration))
  # equivalent to:
  #coupon_npv = 0
  #for i in range(1,duration+1,1):
  #  coupon_npv = coupon_npv + principal*coupon_rate / (1+discount_rate)**i
  return(principal_npv + coupon_npv)

# simply adds a single instance of the current coupon to bond npv
def bond_return_single_period(principal,original_duration,coupon_rate,discount_rate):
  new_duration = original_duration - 1
  npv = bond_npv(principal,new_duration,coupon_rate,discount_rate)
  total_return = npv / principal + coupon_rate
  return(total_return)
