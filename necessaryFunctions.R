median(vectorOfValues)
mean(vectorOfValues)

# 95% Confidence interval
qt(0.025, sampleSize)
qt(0.975, sampleSize)

# Quantiles
z <- rnorm(1000, 0, 1)
quantile(z, c(0.025, 0.975))

# Checking skew
skew(vectorOfValues)

# Correlation
x <- rnorm(30)
y <- rnorm(30)
plot (x,y)

# Probability distributions
dnorm(x, mean=0, sd=1)
pnorm(rnorm(1000, 0, 1), mean=0, sd=1)
qnorm(p, mean=0, sd=1)
rnorm(n, mean=0, sd=1)

# x, q vector of quantiles
# p vector of probabilities
# n number of observations
# mean: vector of means
# sd: vector of standard deviations

# Statistical tests

var.test(vector1, vector2)
t.test(vector1, vector2)
wilcox.test(vector1, vector2)
prop.test(list1, list2)
cor.test(x, y)
chisq.test(matrix)

## If p<0.05, rejected null hypothesis that they are independent, 
## thus, they are associated.

## If counts less than 4, more appropriate to use fisher's test.

fisher.test(matrix)



# Checking whether the second model is better than the first

anova(model1, model2)
## if model significantly better, p<0.05
## null model is that the model is NOT significantly better

# Exploratory data analysis

## Plotting explanatory variables against each other
pairs(model)

## Plotting response against explanatory variables for different combinations of
## other variables that are kept fixed.
coplot(model)

## Fit non-parametric smoothing functions to look for evidence of curvature
## Fit tree models to get an idea of how the data relate and potential interaction
## factors

# Model using lm

model1 <- lm(ozone~temp*wind*rad + I(rad^2) + I(temp^2), data = dataFrame,
             na.action = na.omit)
summary(model1)

model2 <- update(model1, ~.-temp:wind:rad)
model3 <- step(model2)

coef(model3)
fitted(model3)
resid(model3)
AIC(model3)

## Subsetting
model4 <- lm(Fruit~Root, data= data, subset =(Grazed=="Grazed"))


plot(model3)

# Model using aov

model1 <- aov(y~soil, data= dataFrame)
summary(model1)
plot(model1)

## Alternative way to visualise the data
summary.lm(aov(y~soil))
## Usually used for model simplification


# Looking at variables in dataframe

names(dataframe)
dim(dataframe)
head(dataframe)

# Making barplots

tapply(variableToPlot, list(categoryToPlot), functionUsedLikeMean)


# Making box-and-whisker plots

plot(soil, y)

# Checking for collinearity

install.packages("car")
library(car)
vif(model1)


hist(rnorm(1000, 0, 1))

# Clippings

contrasts(clip$clipping) <- NULL
options(contrasts=c('contr.treatment', 'contr.poly'))
options(contrasts=c('contr.helmert', 'contr.poly'))

# Split plots (pseudoreplication for ANOVA)

model <- aov(yield~irritation*density*fertilizer + Error(block/irritation/
                                                           density), data=yields)
interaction.plot(yields$fertilizer, yields$irrigation, yields$yield)


# ANCOVA

plot(regrowth$Root,regrowth$Fruit,
     pch=16+as.numeric(regrowth$Grazing),
     col=c("blue","red")[as.numeric(regrowth$Grazing)])

abline(lm(Fruit[Grazing=="Grazed"]~Root[Grazing=="Grazed"],
          data=regrowth),lty=2,col="blue")

abline(lm(Fruit[Grazing=="Ungrazed"]~Root[Grazing=="Ungrazed"],
          data=regrowth),lty=2,col="red")

# Predict

# Power analysis

power.anova.test
power.prop.test
power.t.test(delta=0.1*5, sd=2^0.5, power=0.8, type='one.sample')

# Posthoc

TukeyHSD(model)
## ANOVA model only

