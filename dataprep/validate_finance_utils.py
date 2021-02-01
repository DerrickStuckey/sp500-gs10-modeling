import finance_utils
duration = 120-1
principal = 1
coupon_rate = 3.12 / 100 / 12
discount_rate = 2.83 / 100 / 12
finance_utils.bond_npv(principal=principal,duration=duration,coupon_rate=coupon_rate,discount_rate=discount_rate)
# expect 1.025049797

finance_utils.bond_return(principal=principal,duration=duration,coupon_rate=coupon_rate,discount_rate=discount_rate)
# expect 1.027649797

duration = 120-1
principal = 1
coupon_rate = 3.12 / 100 / 12
discount_rate = 3.12 / 100 / 12
finance_utils.bond_npv(principal=principal,duration=duration,coupon_rate=coupon_rate,discount_rate=discount_rate)
# expect 1.0

