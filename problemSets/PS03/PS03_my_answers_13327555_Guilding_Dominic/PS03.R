#load data
data_gdp <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2023/main/datasets/gdpChange.csv")
data_mex <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2023/main/datasets/MexicoMuniData.csv")

#libraries
library(MASS)
library(nnet)

#1
#create factors 
plot(x=data_gdp$YEAR, y=data_gdp$GDPWdiff) 

data_gdp$OIL <- factor(data_gdp$OIL,
                       levels = c(0,1),
                       labels = c("Exports < 50%", "Exports > 50%"))
tail(data_gdp$OIL)
                             
data_gdp$GDPWdiff_cat <- cut(data_gdp$GDPWdiff, breaks = c(-Inf, -1, 1,Inf),
                                labels = c("Negative", "No Change", "Positive"))
                                
levels(data_gdp$GDPWdiff_cat)

#relevel
data_gdp$GDPWdiff_cat <- relevel(data_gdp$GDPWdiff_cat, ref = "No Change")

#(a)
multi_model <- multinom(GDPWdiff_cat ~ REG + OIL, data = data_gdp)
summary(multi_model)

#interpretation
#A one unit increase in oil exports (oil exports > 50% GDP) is associated with 
#an increase of 0.57 in the log odds of negative GDP growth vs no change in a 
#given country-year. A one unit increase in oil exports is associated with an increase 
#of 0.36 in the log odds of positive growth vs no change in a given country-year
#The quality of being a democracy (REG = 1) is associated with a 1.01 increase 
#in the log odds of negative growth vs no change in a given country-year.
#The quality of being a democracy (REG = 1) is associated with a 1.4 increase in 
#the log odds of positive growth vs no change in a given country-year.

#(b) 
ord_multi_model <- polr(GDPWdiff_cat ~ REG + OIL, data = data_gdp, Hess = TRUE) 
summary(ord_multi_model)
exp(coef(ord_multi_model))
#interpretation
#The quality of being a democracy is associated with an increase of 1.5 in the 
#odds of GDP increasing or decreasing in a given year.
#A one unit increase in oil export (exports > 50%) is associated with 0.82 increase
#in the odds of a country's GDP increasing or decreasing in a given year.

#2
#(a)
pan_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + 
                     PAN.governor.06, data=data_mex, family = poisson)
summary(pan_poisson)
exp(-0.08135)
#No, there is no evidence that PAN candidates visit swing districts more. The estimate 
#for swing districts 0.921 visits on average, but this result is not 
#statistically different from 0.

errors <- summary(pan_poisson)$coefficients[, 2]
cfs <- coef(pan_poisson)
pan_poisson_z <- cfs/errors
p <- (1 - pnorm(abs(pan_poisson_z), 0, 1))*2
p


#(b)
#marginality.06 : On average, a one-unit increase in the measure of poverty is 
#associated with a -2.08 decrease in the log odds of Calderon visiting to the 
#district in the run up to the 2009 elections, holding all other variables constant

#PAN.governor.06 : On average, the effect of a given district having a sitting 
#PAN governor is associated with a -0.31 decrease in the log odds of Calderon 
#visiting the district in the run up to the 2009 elections, holding all other 
#variables constant. Although these effects are not statistically different from 0.

#(c)
cfs <- coef(pan_poisson)
exp(cfs[1]*1 + cfs[2]*0 + cfs[3]*1)*100
#Estimated that Calderon would visit this hypothetical district 0.276 times in 
#the run up to the 2009 election.






