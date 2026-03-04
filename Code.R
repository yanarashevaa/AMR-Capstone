library(tidyverse)
library(readr)
library(plm)


#var1: healthcare expenditure in Bulgaria 
expenditure_data <- read.csv(
  "/Users/yanarasheva/Desktop/CapstoneData/health_expenditure.csv", 
  skip = 4, header = TRUE, check.names = FALSE)  #there are 4 rows of information above the data set
                                                 #the rest tells it to keep the columns as years (numbers) instead of automatically assigning them letters
colnames(expenditure)

expenditure <- expenditure_data[expenditure_data$"Country Name" == "Bulgaria",
as.character(2006:2023)]
expenditure
expenditure <- as.numeric(expenditure)


#var2: healthcare personnel density
personnel <- read.csv(
  "/Users/yanarasheva/Desktop/CapstoneData/personnel_density.csv")%>%  #import dataset (long format with year column)
  mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%                    #ensure year is numeric so comparisons work correctly
  filter(TIME_PERIOD >= 2006 & TIME_PERIOD <= 2023) %>%                #keep only observations for 2005–2023
  arrange(TIME_PERIOD) %>%                                             #sort chronologically to align with other variables
  pull(OBS_VALUE)                                                      #extract the values as a numeric vector for regression

personnel     #sanity check

#var3: gini coefficient
gini_data <- read.csv("/Users/yanarasheva/Desktop/CapstoneData/gini.csv", 
                 skip = 4, header = TRUE, check.names = FALSE)   #there are 4 rows of information above the data set
glimpse(gini)                                                    #the rest tells it to keep the columns as years (numbers) 
colnames(gini)

gini <- gini_data[gini_data$"Country Name" == "Bulgaria",
                           as.character(2006:2023)]
gini <- as.numeric(gini)
gini


#var4: out of pocket payments for medical expenses
out_of_pocket_data <- read.csv("/Users/yanarasheva/Desktop/CapstoneData/out_of_pocket.csv", 
                      skip = 4, header = TRUE, check.names = FALSE)   #there are 4 rows of information above the data set

glimpse(out_of_pocket_data)                                                         #the rest tells it to keep the columns as years (numbers) 
colnames(out_of_pocket_data)

out_of_pocket <- out_of_pocket_data[out_of_pocket_data$"Country Name" == "Bulgaria",
                  as.character(2006:2023)]
out_of_pocket <- as.numeric(out_of_pocket)
out_of_pocket

#var5: government effectuveness index 
govt_effectiveness_data <- read.csv("/Users/yanarasheva/Desktop/CapstoneData/govt_effectiveness.csv", sep = ";")  #the data set was initially read as a single column because it is semicolon-separated (;) rather than comma-separated (,)
govt_effectiveness_data <- govt_effectiveness_data[order(govt_effectiveness_data$Year), ] #making sure it is ordered

glimpse(govt_effectiveness_data)                                                                                  #read.csv() defaults to comma delimiters, so the entire row was interpreted as one character string
                                                                                                                  #sep = ";" correctly parses the file into separate columns
govt_effectiveness <- govt_effectiveness_data$Government.Effectiveness.Index[
  govt_effectiveness_data$Year >= 2006 & govt_effectiveness_data$Year <= 2021]

govt_effectiveness <- c(govt_effectiveness, -0.07, 0.11)  #the first data set only includes measurements until 2021
govt_effectiveness_data                                   #i meanually append values to the vector from a separate reliable source 

#var6: corruption perception index 
cpi_data <- read.csv("/Users/yanarasheva/Desktop/CapstoneData/cpi.csv", skip = 2)
cpi_data <- cpi_data[order(cpi_data$Year), ]  #making sure it's ordered by year

colnames(cpi_data)
cpi <- cpi_data$CPI[
  cpi_data$Year >= 2006 & cpi_data$Year <= 2023]
cpi


#var7: rule of law index
rule_of_law_data <- read.csv("/Users/yanarasheva/Desktop/CapstoneData/rol.csv", sep = ";")  #the data was taken from the same source as government effectiveness index, so i anticipated the same difficulties 
rule_of_law_data <- rule_of_law_data[order(rule_of_law_data$Year), ] #making sure it is ordered

glimpse(rule_of_law_data)

rol <- rule_of_law_data$Rule.of.Law.Index[
  rule_of_law_data$Year >= 2006 & rule_of_law_data$Year <= 2021]
rol

rol <- c(rol, -0.07, 0.11)  #the first data set only includes measurements until 2021
rol                         #last observation was also 2021, so i again manually appended the values for 2022 and 2023 from a different source 



#dependent variable: antimicrobial resistance 
resistance_data <- read.csv("/Users/yanarasheva/Desktop/CapstoneData/resistance.csv")
resistance_data$NumValue <- as.numeric(resistance_data$NumValue)  #observations in column NumVale were being saved as character/text
                                                                  #the function converts it to numeric and saves the converted version back into the dataframe

glimpse(resistance_data)
colnames(resistance_data)

resistance_data <- resistance_data[
  resistance_data$RegionName == "Bulgaria" &
    resistance_data$Time >= 2006 &
    resistance_data$Time <= 2023,]

resistance_data <- resistance_data[order(resistance_data$Time), ]
resistance <- resistance_data$NumValue

class(resistance)

#sanity check to make sure all variables have the same number of observations 
  length(expenditure)
  length(personnel)
  length(gini)
  length(out_of_pocket)
  length(govt_effectiveness)
  length(cpi)
  length(rol)
  length(resistance)

  
#creating a data frame with 
  years <- 2006:2023
  
  data <- data.frame(
    year = years,
    resistance = resistance,
    personnel = personnel,
    expenditure = expenditure,
    gini = gini,
    out_of_pocket = out_of_pocket,
    govt_effectiveness = govt_effectiveness,
    cpi = cpi,
    rol = rol
  )

str(data)


#model 1: institutional capacity:
model1 <- lm(resistance ~ expenditure + personnel +
               govt_effectiveness, data = data)

summary(model1)

#checking OLS assumptions
  #linearity
  plot(model1$fitted.values, model1$residuals,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")

  #no multicollinearity 
  library(car)
  vif(model1) #variance inflation factor

  #independence of errors / autocorrelation
  library(lmtest)
  dwtest(model1)

  #normality of residuals
  qqnorm(model1$residuals); qqline(model1$residuals)
  
  #heteroskedasticity
  bptest(model1)
  
  
#other model diagnostics
  #correlation matrix
  cor(data[, c("expenditure", "personnel", "govt_effectiveness")])
  
  #outliers / influential observations
  plot(model1, which = 4)   # Cook's distance
  
  

#model 2: socioeconomic barriers:
model2 <- lm(resistance ~ gini + out_of_pocket,
             data = data)
summary(model2)

#checking OLS assumptions
  #linearity
  plot(model2$fitted.values, model2$residuals,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")
  
  #no multicollinearity 
  vif(model2)
  
  #independence of errors / autocorrelation
  dwtest(model2)
  
  #normality of residuals
  qqnorm(model2$residuals); qqline(model2$residuals)
  
  #heteroskedasticity
  bptest(model2)

#other model diagnostics
  #correlation matrix
  cor(data[, c("gini", "out_of_pocket")])
  
  #outliers / influential observations
  plot(model2, which = 4)   # Cook's distance


#model 3: behavioral norms:
model3 <- lm(resistance ~ cpi + rol,
             data = data)
summary(model3)

#checking OLS assumptions
  #linearity
  plot(model3$fitted.values, model3$residuals,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")
  
  #no multicollinearity 
  vif(model3)
  
  #independence of errors / autocorrelation
  dwtest(model3)
  
  #normality of residuals
  qqnorm(model3$residuals); qqline(model3$residuals)
  
  #heteroskedasticity
  bptest(model3)

#other model diagnostics
  #correlation matrix
  cor(data[, c("cpi", "rol")])
  
  #outliers / influential observations
  plot(model3, which = 4)   # Cook's distance



#model 4: full combined model
model4 <- lm(resistance ~ expenditure +
               personnel +
               gini +
               out_of_pocket +
               govt_effectiveness +
               cpi +
               rol,
             data = data)

summary(model4)

#checking OLS assumptions
  #linearity
  plot(model4$fitted.values, model4$residuals,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")
  
  #no multicollinearity 
  vif(model4)
  
  #independence of errors / autocorrelation
  dwtest(model4)
  
  #normality of residuals
  qqnorm(model4$residuals); qqline(model4$residuals)
  
  #heteroskedasticity
  bptest(model4)

#other model diagnostics
  #correlation matrix
  cor(data[, c("expenditure", "personnel","govt_effectiveness", 
               "gini", "cpi", "rol")])
  
  #outliers / influential observations
  plot(model4, which = 4)   # Cook's distance

  
#nice output from regressions to include in my paper 
install.packages("stargazer")   # only once
library(stargazer)

stargazer(model1, model2, model3,
          type = "html",
          out = "regression_table:).html",
          title = "Determinants of Antimicrobial Resistance in Bulgaria (2006–2023)",
          column.labels = c("Institutional Capacity | ",
                            "Socioeconomic Barriers | ",
                            "Governance Quality"),
          dep.var.labels = "E. coli 3GC Resistance (%)",
          covariate.labels = c("Health Expenditure",
                               "Personnel Density",
                               "Government Effectiveness",
                               "Gini Coefficient",
                               "Out-of-Pocket Payments",
                               "Corruption Perceptions Index",
                               "Rule of Law"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          keep.stat = c("n", "rsq", "adj.rsq", "f"))


