# To run Tobit model 
library("AER")
library("sandwich") # for clustered standard errors
library("lmtest")
library("plm") # to specify the panel structure of the data frame

# To Create Lag Variables
library("DataCombine")
# To standardize the coefficients of regression models
library("reghelper")
# To Print table for Paper and Presentations
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tbm)
library(ggeffects)
library(gridExtra)
library(stargazer)
library(ggplot2)

# For Moran I Test
library(ape)
library(spdep)
library(mefa) # to transform matrix into listw object

#VIF
library(car)

#panel match
library(PanelMatch)


# Upload Data
APLA_Data<- read.csv("Data_APLA.csv", stringsAsFactors = TRUE)

#run tobit models
APLA_Data$Year<- as.factor(APLA_Data$Year) # Year as factor

APLA_Data<- plm.data(APLA_Data, index = c("Country", "Year")) # to Specify the panel data structure


TOB1<- tobit(Regulatory_Complexity ~ VDEM_Polyarchy+ Left1_Other0 + Country + Year , data= APLA_Data)
summary(TOB1)

TOB2<- tobit(Regulatory_Complexity ~ GrowthGDPperCap + Trade_Perc_GDP + Country + Year  , data= APLA_Data)

TOB3<- tobit(Regulatory_Complexity ~ IntMigStock+ Country + Year  , data= APLA_Data)

TOB4<- tobit(Regulatory_Complexity ~ RefAsPerc+ Country + Year ,data= APLA_Data)

TOB5<- tobit(Regulatory_Complexity ~ VDEM_Polyarchy + Left1_Other0 +GrowthGDPperCap+ Trade_Perc_GDP+ IntMigStock+ RefAsPerc + MigSpainUSPerc + Country + Year , data= APLA_Data)



# Calculate Clustered Standard Errors
TOB1coeffs_cl <- coeftest(TOB1, vcov = vcovCL, cluster = ~Country)
TOB2coeffs_cl <- coeftest(TOB2, vcov = vcovCL, cluster = ~Country)
TOB3coeffs_cl <- coeftest(TOB3, vcov = vcovCL, cluster = ~Country)
TOB4coeffs_cl <- coeftest(TOB4, vcov = vcovCL, cluster = ~Country)
TOB5coeffs_cl <- coeftest(TOB5, vcov = vcovCL, cluster = ~Country)
# Select Clustered Standard Errors
TOB1coeffs_cl<- TOB1coeffs_cl[,2]
TOB2coeffs_cl<- TOB2coeffs_cl[,2]
TOB3coeffs_cl<- TOB3coeffs_cl[,2]
TOB4coeffs_cl<- TOB4coeffs_cl[,2]
TOB5coeffs_cl<- TOB5coeffs_cl[,2]


tobit_table <- stargazer(TOB1, TOB2,TOB3, TOB4, TOB5,
          type = "text",  
          covariate.labels = c("VDEM Polyarchy", "Left-Wing Gov", "Change in GDP per capita", 
                               "Trade as perc. of GDP", "International Migration Stock",
                               "Refugees as perc. of pop.", "Emigrants in US and Spain"), 
          dep.var.labels = "Regulatory Complexity", 
          omit = c("Constant", "Country", "Year"), 
          digits = 2, title = "Tobit Model on Regulatory Complexity", align= TRUE, 
          add.lines = list( c("Country FE", "Yes", "Yes", "Yes", "Yes","Yes"),
                            c("Year FE", "Yes", "Yes", "Yes", "Yes","Yes"),
                            c("Country-Level\nClustered SE",  "Yes", "Yes", "Yes", "Yes", "Yes")),
          column.sep.width = "3pt", 
          omit.stat = "wald", 
          se = list (TOB1coeffs_cl, 
                     TOB2coeffs_cl,
                     TOB3coeffs_cl, TOB4coeffs_cl,  TOB5coeffs_cl), 
          notes = "These models use left-censoring at zero")

#plot residuals (base)
res_5 <- resid(TOB5)

plot(fitted(TOB5), res_5, xlab = "Fitted Values", ylab = "Residuals", 
       main = "Residual Plot for Model 5")
abline(0,0)

#are residuals normally distributed?
qqnorm(res_5)
qqline(res_5)

#density plot
plot(density(res_5), xlab = "Residuals", ylab = "Density", main = "Residual Density Plot")


#multicollinearity check (VIF)
vif_score <- vif(TOB5)
vif_score

cor_data <- Data_APLA[, c("VDEM_Polyarchy", "Left1_Other0", "GrowthGDPperCap", 
                          "Trade_Perc_GDP", "IntMigStock", "RefAsPerc", 
                          "MigSpainUSPerc", "Year")]
  
cor(cor_data)

?resid_panel

Data_APLA <- read.csv("Data_APLA.csv", stringsAsFactors = TRUE)

# Lag Regulatory Complexity by 1 and 3 years 
APLA_Data_1 <- slide(data=Data_APLA, Var= "Regulatory_Complexity", GroupVar= "Country", NewVar= "One_Year_Lag_Regulatory_Complexity", slideBy= -1)

APLA_Data_3 <- slide(data=Data_APLA, Var= "Regulatory_Complexity", GroupVar= "Country", NewVar= "Three_Year_Lag_Regulatory_Complexity", slideBy= -3)


# For some reason, AER:: required before tobit for it to work with Stargazer

LAGFEGLM1<- tobit(One_Year_Lag_Regulatory_Complexity ~ VDEM_Polyarchy + 
                    Left1_Other0 + Trade_Perc_GDP + Country + Year, data= APLA_Data_1)

LAGFEGLM2<- tobit(One_Year_Lag_Regulatory_Complexity ~ VDEM_Polyarchy + 
                    Left1_Other0 +GrowthGDPperCap+ Trade_Perc_GDP+ IntMigStock+ 
                    RefAsPerc + MigSpainUSPerc + Country + Year, data= APLA_Data_1)

LAGFEGLM3<- tobit(Three_Year_Lag_Regulatory_Complexity ~ VDEM_Polyarchy + 
                    Left1_Other0 + Trade_Perc_GDP + Country + Year, data= APLA_Data_3)

LAGFEGLM4<- tobit(Three_Year_Lag_Regulatory_Complexity ~ VDEM_Polyarchy + 
                    Left1_Other0 +GrowthGDPperCap+ Trade_Perc_GDP+ IntMigStock+ 
                    RefAsPerc + MigSpainUSPerc + Country + Year, data= APLA_Data_3)


# Calculate Clustered Standard Errors
LAGFEGLM1_cse <- coeftest(LAGFEGLM1, vcov = vcovCL, cluster = ~Country)
LAGFEGLM2_cse <- coeftest(LAGFEGLM2, vcov = vcovCL, cluster = ~Country)
LAGFEGLM3_cse <- coeftest(LAGFEGLM3, vcov = vcovCL, cluster = ~Country)
LAGFEGLM4_cse <- coeftest(LAGFEGLM4, vcov = vcovCL, cluster = ~Country)

# Select Clustered Standard Errors
LAGFEGLM1_cse<- LAGFEGLM1_cse[,2]
LAGFEGLM2_cse<- LAGFEGLM2_cse[,2]
LAGFEGLM3_cse<- LAGFEGLM3_cse[,2]
LAGFEGLM4_cse<- LAGFEGLM4_cse[,2]

class(Data_APLA[,"Year"])

#convert to dataframe
Data_APLA <- as.data.frame(Data_APLA)

#convert year variable 
Data_APLA$Year <- as.integer(Data_APLA$Year)

#display treatment graph
DisplayTreatment(unit.id = "Country",
                 time.id = "Year", legend.position = "none",
                 xlab = "Year", ylab = "Country", 
                 legend.labels = c("Right-wing", "Left-wing"),
                 x.angle = 90,treatment = "Left1_Other0", data = Data_APLA)
?DisplayTreatment


#create numeric unit.id variable
Data_APLA <- transform(Data_APLA, Country=as.integer(as.factor(Country)))

#match
PM_results <- PanelMatch(lag = 3, time.id = "Year", unit.id = "Country", #unit.id needs to be integer/numeric
                         treatment = "Left1_Other0",refinement.method = "mahalanobis",
                         data = Data_APLA, match.missing = TRUE,
                         covs.formula = ~ I(lag(Regulatory_Complexity, 1:3)),
                         size.match = 3, qoi = "att", outcome.var = "Regulatory_Complexity",
                         lead = 0:4, forbid.treatment.reversal = FALSE)

PM_results

#get set effects
set.effects <- get_set_treatment_effects(pm.obj = PM_results, data = Data_APLA, lead = 0)
set.effects

#plot distribution of matched sets
plot(PM_results$att, 
     col = "pink",
     ylab = "Frequency of Size",
     xlab = "Countries (Expressed As Integer For Matching)",
     xlim = c(0, 19),
     include.empty.sets = TRUE)

?hist


#get covariate balance
cov_bal <- get_covariate_balance(PM_results$att, Data_APLA, 
                                 covariates = c("Left1_Other0" , "Regulatory_Complexity"), plot = TRUE, 
                                 ylim = c(-1, 1))
summary(cov_bal)
?PanelMatch

#panel estimate
PE.results <- PanelEstimate(sets = PM_results, data = Data_APLA)
summary(PE.results)

#plot estimate of treatment effect over time
plot(PE.results)


