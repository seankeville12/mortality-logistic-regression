#Preliminary cleaning of data & Collinearity testing

library(dplyr)

data<- data %>% filter(!is.na(sbp) & !is.na(dbp))

cor(data$sbp,data$dbp)

library(car)
colinearity_model1 <- lm(smokeintensity ~ smkintensity82_71 + smokeyrs ,data=data)
colinearity_model2 <- lm(smkintensity82_71 ~ smokeintensity + smokeyrs, data=data)
colinearity_model3 <- lm(smokeyrs ~ smokeintensity + smkintensity82_71 , data=data)
Corel_ting1 = vif(colinearity_model1)
Corel_ting2 = vif(colinearity_model2)
Corel_ting3 = vif(colinearity_model3)

vif(colinearity_model1)
vif(colinearity_model2)
vif(colinearity_model3)
vif(colinearitymodel_alco1)
vif(colinearitymodel_alco2)
vif(colinearitymodel_alco3)
model_bp <- lm(sbp ~ dbp , data = data )
vif(model_bp)

library(dplyr)
data$alcoholhowmuch[is.na(data$alcoholhowmuch)] <- 0
colinearitymodel_alco1 <- lm(alcoholhowmuch ~ alcoholfreq + alcoholtype, data=data)
colinearitymodel_alco2 <- lm(alcoholfreq ~ alcoholhowmuch + alcoholtype, data =data)
colinearitymodel_alco3 <- lm(alcoholtype ~ alcoholhowmuch + alcoholfreq, data =data)
data$pregnancies[is.na(data$pregnancies)] <- 0

income_data <- data$income
plit_income <- plot(density(data$income, na.rm = TRUE), main = "Density Plot of Income", xlab = "Income", ylab = "density") 
plot_histogram <- plot(hist(data$income, na.rm = TRUE), main = "Histogram Plot of Income", xlab = "Income", ylab = "Frequencey")
median_income <- median(data$income, na.rm = TRUE)
print(median_income)
data$income[is.na(data$income)]<- 19

#Looking at models

library(stargazer)

stargazer(model_orig, type="text", summary = TRUE)

summary(model_orig)

first_logitmodel <- glm(death ~ qsmk + sbp + sex + age + race + income + marital + school + education + ht + wt82_71 + smokeintensity + smkintensity82_71 + smokeyrs + asthma + bronch + tb + hf + hbp + pepticulcer + colitis + chroniccough + hayfever + diabetes + polio + tumor + nervousbreak + alcoholfreq + alcoholtype + alcoholhowmuch + pica + headache + otherpain + weakheart + allergies + nerves + lackpep + hbpmed + boweltrouble + wtloss + infection + active + exercise + birthcontrol + pregnancies + cholesterol, data =  data)

library(stargazer)
stargazer(first_logitmodel, type = "text", summary = TRUE)

#AIC for initial model is 1041
#We now cut out all variables deemed statsistacally insignificant: New model is as follows
refined_logit <- glm(death ~ sbp + age + income + education + ht + wt82_71 + smokeyrs + colitis + chroniccough + nervousbreak + headache + nerves + hbpmed + infection , data =data)
stargazer(refined_logit, type = "text", summary = TRUE)
#AIC for significant with eduaction is 1021
#Despite me including eduaction, it could be deemed insignificant: only 2 out of 4 levels deemed significant by stargazer
refined_withouteduc <-  glm(death ~ sbp + age + income + ht + wt82_71 + smokeyrs + colitis + chroniccough + nervousbreak + headache + nerves + hbpmed + infection , data =data)

#We will now try find the AIC for model without education 
stargazer(refined_withouteduc, type ="text", summary = TRUE)

#We now will take model without education as AIC is 1020

#Lets now look at heteroskedsatcity by using a lipsitz_model 

library(lmtest)
library(sandwich)

# Extract fitted values and residuals from the logistic regression model
fitted_accmodel <- fitted(refined_withouteduc)
residual_accmode <- residuals(refined_withouteduc, type = "deviance")

# Create a data frame for the Lipsitz model
test_data <- data.frame(
  residuals_squared = residual_accmode^2,
  fitted_values = fitted_accmodel
)

# Fit the Lipsitz model
lipitz_model <- lm(residuals_squared ~ fitted_values, data =test_data)

# Print the Lipsitz model summary
print(summary(lipitz_model))


library(lmtest)
library(sandwich)

fitted_accmodel <- fitted(refined_withouteduc)
residual_accmode<- residuals(refined_withouteduc, type = "deviance")

lipitz_model<- lm(residual_accmode^2-residual_accmode)

print(lipitz_model)

#This suggests that there is a relationship between the fitted values and residuals so we will use robust standard errors

library(sandwich)
library(lmtest)

robust_cov <- vcovHC(refined_withouteduc, type = "HC0")  # HC0, HC1, HC2, or HC3 (White's correction)

robust_results <- coeftest(refined_withouteduc, vcov = robust_cov)
print(robust_results)

#Based on these results we will omit "ht","colitis","nervousbreak","headache" on basisi of insignificant (PR>z)

new_model = glm(death ~ sbp + age + income + wt82_71 + smokeyrs + chroniccough + nerves + hbpmed , data = data)

#Lets now find the AIC for this model

stargazer(new_model, type = "text" , summary =TRUE)

#AIC now at 1023 so we will go back and cut vairbles one at a time

#Note here; we have increased AIC but cut out insignificant varibales:
#Value cutting out insignificant variables over increased AIC particularly since there is not much difference in the value
#We may have to check if variables are multi colinear

#We will now look at the autocorrelation of the residuals

residuals_new <- residuals(new_model, type = "deviance")
acf(residuals_new)

#ACF only produces a graph not a values

#Please look at graph to see there is now autocorellation (make sure of this)

# Plot residuals vs. fitted values to check for patterns

plot(fitted(new_model), residuals_new)

#We do a durban watson test, testinf for autocorrelation 

dw_test <- dwtest(new_model)
print(dw_test)

#As seem from the dw test, there is no autocrelation with p value = 0.4374 and DW=1.99 wich suggests no autocorrelation 

#Lets now do an anova verse a null

null_model <- glm(death ~ 1 , data = data, family = binomial)

anova(null_model, new_model, test = "Chisq")

#Suggests model is much better than null

#We will now run a psuedo R^2 test

1 - (logLik(new_model) / logLik(null_model))

# Lets now find the new robust se's for this model

robust_cov <- vcovHC(new_model, type = "HC0")  # HC0, HC1, HC2, or HC3 (White's correction)

robust_results <- coeftest(new_model, vcov = robust_cov)
print(robust_results)

#These are all significant variables with their respective se's produce when you execute the above code

#Now lets look at the vif of the updated model:

vif(new_model)

#Everything is below <5, indicates no vairable have signifiacnt multicollienarity

#Lets now run a 90% confindence interval for the model

confint(new_model, level = 0.90)  # For 90% CI

#Now lets look at the resididuals

plot(residuals(new_model, type = "deviance"))
abline(h = 0, col = "red", lty = 2) 

#Note slight skewness in the residual plot, residuals clustered on negative side

mean(residuals(new_model, type = "deviance"))

#Lets take a deeper dive into the model

new_model = glm(death ~ sbp + age + income + wt82_71 + smokeyrs + chroniccough + nerves + hbpmed , data = data)

#Breakthrough moment : age is a varible wich increases chance of death significantly the older you are
#Lets apply the transfromation (x^2)

transnew_model = glm(death ~ sbp + I(age^2) + income + wt82_71 + smokeyrs + chroniccough + nerves + hbpmed , data = data)

#We can then do same anaylsis such as looking at the AIC

stargazer(transnew_model, type = "text", summary= TRUE)

#We yield an AIC of 1,002

#However note that smokeyrs has the same effect on death, becoming a more promiment factor fro death the longer someone smokes
#Lets provide the same transformation as for age

new_newtrans=glm(death ~ sbp + I(age^2) + income + wt82_71 + I(smokeyrs^2) + chroniccough + hbpmed , data = data)

#Lets look at the AIC for this model

stargazer(new_newtrans, type = "text", summary = TRUE)

#AIC is 980 on this model, we should stick to this


#However we noticed an issue with our Wt82_71 column and had to adjusted the na values
#Decied to omit all rows not containing n/a

library(dplyr)
data <- data %>% filter(!is.na(wt82_71))

#Now lets look at our model again 

transform_model <- glm(death ~ I(sbp^2) + I(age^2) + income + wt82_71 + I(smokeyrs^2) + chroniccough + hbpmed, 
                       data = data, family = binomial)

stargazer(transform_model, type = "text", summary = TRUE)


#AIC is now 630

#AIC does not change significantly when we exlcude hbpmed and wt82_71, so we will keep them in for now

#Test for hetereoskedasticity

library(lmtest)
library(sandwich)

# Extract fitted values and residuals from the logistic regression model
fitted_newmodel <- fitted(transform_model)
residual_newmodel <- residuals(transform_model, type = "deviance")

# Create a data frame for the Lipsitz model
test_data1 <- data.frame(
  residuals_squaredn = residual_newmodel^2,
  fitted_valuesc = fitted_newmodel
)

# Fit the Lipsitz model
lipitz_model1 <- lm(residuals_squaredn ~ fitted_valuesc, data = test_data1)

# Print the Lipsitz model summary
print(summary(lipitz_model1))

#Output suggests hetereoskedastcity 

#Now we will use whites formula to calcuate robust SE for this model

robust_cov1 <- vcovHC(transform_model, type = "HC0")  
robust_results1 <- coeftest(transform_model, vcov = robust_cov1)


print(robust_results1)

#All deemed significant bar wt82_71 and hbpmed, however they dont affect the AIC significasntly

#We will now run a psuedo R^2 test

1 - (logLik(transform_model) / logLik(null_model))

#Log Lik has increased significantly from "new_model" to transform model , increasing from 0.31 to 0.577

#Now lets look for multicollinearity using VIF

vif(transform_model)

#All below 5 which is a positive sign 

#Lets now check for autocorrelation with the durban watson test

dw_test <- dwtest(transform_model)
print(dw_test)

#As DW=2.07 and p-value=0.8681 we can conclude no autocorrelation 

#We cannot do a likehood test with anova as null model and updated transform mdoel have different dimensions

#lets now check correlations between each variable

# Select continuous variables
continuous_vars <- data %>%
  select(sbp, age, smokeyrs, wt82_71) 

# Calculate correlation matrix
cor_matrix <- cor(continuous_vars, use = "complete.obs")

# Display the correlation matrix
cor_matrix

#High correlation between age and smoke_years of 0.87

#Lets make a combined smoking exposure metric

data$smoking_exposure <- data$smokeyrs/data$age
model_exposure <- glm(death ~ I(sbp^2) + I(age^2) + income + wt82_71 + I(smoking_exposure^2) + chroniccough + hbpmed, 
                      data = data, family = binomial)

stargazer(model_exposure, type ="text", summary = TRUE)

#AIC is now 631

#Lets run all the tests again


library(lmtest)
library(sandwich)

# Extract fitted values and residuals from the logistic regression model
fitted_exposedmodel <- fitted(model_exposure)
residual_exposedmodel <- residuals(model_exposure, type = "deviance")

# Create a data frame for the Lipsitz model
test_data2 <- data.frame(
  residuals_squarede = residual_exposedmodel^2,
  fitted_valuese = residual_exposedmodel
)

# Fit the Lipsitz model
lipitz_model1 <- lm(residuals_squarede ~ fitted_valuese, data = test_data2)

# Print the Lipsitz model summary
print(summary(lipitz_model1))

#Output suggests hetereoskedastcity 

#Now we will use whites formula to calcuate robust SE for this model

robust_cov2 <- vcovHC(model_exposure, type = "HC0")  
robust_results2 <- coeftest(model_exposure, vcov = robust_cov1)


print(robust_results1)

#All deemed significant bar wt82_71, hbpmed, and chroniccough however they dont affect the AIC significasntly

#We will now run a psuedo R^2 test

1 - (logLik(model_exposure) / logLik(null_model))

#Very similar result to the previous model

#Now lets look for multicollinearity using VIF

vif(model_exposure)

#All below 5 which is a positive sign 

#Lets now check for autocorrelation with the durban watson test

dw_test2 <- dwtest(model_exposure)
print(dw_test2)

#As DW=2.06 and p-value=0.8374 we can conclude no autocorrelation 

#We cannot do a likehood test with anova as null model and updated transform mdoel have different dimensions

#Now I will produce grpahs on this regression model

# Basic residual plots
plot_data <- data.frame(
  fitted = fitted_exposedmodel,
  residuals = residual_exposedmodel
)

ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Deviance Residuals")

# QQ plot for residuals
ggplot(plot_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals")

#ROC Curve and AUC
install.packages("pROC")
library(pROC)
roc_curve <- roc(data$death, fitted_exposedmodel)
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

#Variable Importance Plot
install.packages("vip")
library(vip)
vip(model_exposure, num_features = 10)

#Calibration Plot

library(ggplot2)
calibration_data <- data.frame(
  pred_prob = fitted_exposedmodel,
  actual = data$death
)

calibration_data$bin <- cut(calibration_data$pred_prob, breaks = seq(0, 1, by = 0.1))
calibration_summary <- aggregate(actual ~ bin, data = calibration_data, mean)

ggplot(calibration_summary, aes(x = as.numeric(bin), y = actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Calibration Plot",
       x = "Predicted Probability",
       y = "Observed Proportion")
