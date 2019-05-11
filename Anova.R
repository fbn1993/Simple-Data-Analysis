# We'll use the insect sprays dataset
# tom compare means between groups

str(InsectSprays)

# optional test for normality
library(lattice)

# Visualizen the spray distribution

histogram(~count | spray, data = InsectSprays)

with(InsectSprays, tapply(count, spray, shapiro.test))
# p-value tells us that the chances that the
# sample comes from a normal distribution
# in this example, p is clearly > 0.05
# so it is normally distributed

# Build the anova model 
# We want to model the means of variable count
# as a function of variable spray
aov_model <- aov(count ~ spray, data = InsectSprays)
# The formula "count ~ spray" reads as count "as a function of spray"

aov_model




# Evaluate the differences
summary(aov_model)


# the ANOVA F test is significant
# as p < 0.0001, providing evidence that the 5 sprays
# are not all the same

# model.tables() function that examines the results of individual levels
# of factors
model.tables(aov_model, type = 'effects')
# Spray E on average had 6 times fewer bugs than the average over all fields
# on fields where spra A was used, farmers found on average
# 5 bugs more compared to the overall mean
model.tables(aov_model, type = 'mean')

# Spray A would not be bought, but what about D versus E and C?

# Test the pairwise differences between sprays
# Use the TukeyHSD() function 
comparisons <- TukeyHSD(aov_model)
comparisons$spray['D-C', ]
comparisons$spray['F-E', ]
comparisons$spray['B-A', ]

# Plot the differences - las = 1
plot(comparisons, las = 1)
# Each line represents mean difference between
# both groups with the according confidence level
# Whenever the CI doesn't include zero (the vertical line)
# the difference between groups is significant

#install.packages('multcomp')
library(multcomp)
str(cholesterol)
head(cholesterol)
attach(cholesterol)
aov_model <- aov(response ~ trt)
aov_model
summary(aov_model)
# The ANOVA F test for treatment (trt) is significant
# (p < 0.0001), providing evidence that the 5 treatments
# aren't equally effected
detach(cholesterol)

#install.packages('gplots')
library(gplots)
attach(cholesterol)
plotmeans(response ~ trt, 
          xlab = "Treatment",
          ylab = "Response",
          main = "Mean Plot\nwith 95% CI"
          )
detach(cholesterol)

# Lets examine the output from TukeyHSD() for pairwise differences
# between group means
plot_info <- TukeyHSD(aov_model)

par(mar = c(5,4,4,2))
plot(plot_info, las = 2)

# Using qq plot to determine normality
#install.packages("car")
library(car)
qqPlot(lm(response ~ trt, data = cholesterol),
       simulate = TRUE,
       main = "QQ plot",
       labels = FALSE)

# anova assumes equal variances across groups
#The Bartlett test is used to verify this assumption
bartlett.test(response ~ trt, data = cholesterol)
#this test indicates that the variances in the 5 groups
#don't differ significantly - p = 0.97

# anova is sensitive to outliers
# test for outliers using the outlierTest() function
#install.packages('car')
library(car)
outlierTest(aov_model)
