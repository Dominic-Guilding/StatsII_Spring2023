install.packages("eha")
install.packages("survival")
library("survival")
library(eha)
library("stargazer")
data("infants")
infants

#fit Cox Proportional Hazard model
mod <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)
summary(mod)

#duration plot
plot_mod <- coxreg(Surv(enter, exit, event) ~ age + sex, data = infants)
plot(plot_mod)

#plot cox model
cox_fit <- survfit(mod)
autoplot(cox_fit)

mod2 <- coxph(Surv(enter, exit, event) ~ sex, data = infants)

sub_infants <- with(infants, 
               data.frame(
                 sex = c("girl", "boy")))

#plot survival proportion by sex
plot(survfit(mod2, newdata = sub_infants),
     conf.int = F,
     ylim = c(0.2, 1),
     col = c("red", "blue"),
     xlab = "Time (Days)",
     ylab = "Survival Proportion",
     main = "Survival Plot by Sex")
legend("bottomleft",
       legend=c("Girl", "Boy"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

#exponentiate estimates
exp(-0.04)
exp(-0.485)


#Although the results are not statistically different from zero, interpreting the
#coefficients for boys indicates a decrease of 0.485 in the log likelihood of infant
#mortality compared to girls, holding the age of the mother constant. Taking the 
#exponent, the hazard ratio for boys is 0.61 that of girls -> 61 boys die in infancy
#for every hundred girls.
#Age, again not significant, but a one unit increase in the age of the mother is 
#associated with a decrease of 0.04 in the log likelihood of infant mortality, 
#holding the sex of the child constant.

hist(infants$age)
