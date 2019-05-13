# we are examining two samples (with or without phones)
# and we are comparing the reaction times between both

install.packages("pwr")
library(pwr)
power_information <- pwr.t.test(d = 0.8, 
                                sig.level = 0.01,
                                power = 0.95,
                                type = "two.sample",
                                alternative = "two.sided")
power_information
plot(power_information)

power_changes <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                            sig.level = 0.01,
                            power = 0.95)
power_changes
plot(power_changes)

# Cohen describes the effect size as "the degree to which the null
# hypothesis is false"
# In the coin flip example, this is the difference between 75% and the 50%

cohen.ES(test = "r", size = "small")
