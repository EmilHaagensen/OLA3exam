#Sum up the yearly forbrug for 2015 and 2016 and see the difference

value2015 <- sum(forbrug_period[61:64,2])
value2016 <- sum(forbrug_period[65:68,2])

realvaekst_2016 <- ((value2016 - value2015) / value2015) * 100
# [1] 3.310185


diff <- ((realvaekst_2016 - 2) / 2) * 100
# [1] 65.50927

