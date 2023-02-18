
library(tidyverse)
library(ggplot2)
library(dplyr)
df <- read.csv("credit_risk_dataset (1).csv")

str(df)
head(df)
summary(df)
df$loan_status <- as.factor(df$loan_status)
df$person_home_ownership <- as.factor(df$person_home_ownership)
df$loan_intent <- as.factor(df$loan_intent)
df$loan_grade <- as.factor(df$loan_grade)
df$cb_person_default_on_file <- as.factor(df$cb_person_default_on_file)

#### clean data
### removing missing values
clean_data <- df%>%filter(!is.na(person_emp_length)&!is.na(loan_int_rate))
### removing outliers
clean_data <- clean_data%>%filter(person_age<quantile(person_age,0.75)+2*IQR(person_age))%>%
  filter(person_emp_length<quantile(person_emp_length,0.75)+2*IQR(person_emp_length))%>%
  filter(person_income<quantile(person_income,0.75)+2*IQR(person_income))

summary(clean_data)

##############"modeling
logistic_model <- glm(loan_status ~ ., data = clean_data, family = binomial(link = 'logit'))
summary(logistic_model)


logistic_model2 <- glm(loan_status ~ person_age + person_income +person_home_ownership+person_emp_length+
                         loan_intent +loan_grade + loan_amnt + loan_int_rate,
                       data = clean_data,family = binomial)

glm.diag.plots(logistic_model2)

summary(logistic_model2)
plot(logistic_model2)

cbind(Estimate=round(coef(logistic_model2),3),
      'Odds Ratio' = round(exp(coef(logistic_model2)),3))
anova(logistic_model,logistic_model2, test="Chisq")
###accuracy
predicted <- predict(logistic_model2, type = "response")
table(clean_data$loan_status, predicted > 0.5)
table(clean_data$loan_status, predicted > 0.5) %>% prop.table()

View(clean_data)

# Binary model analysis
binary_model_interaction <- glm(loan_status ~ loan_int_rate*loan_percent_income, family = binomial, data = clean_data)
summary(binary_model_interaction)

binary_model_simple <- glm(loan_status ~ loan_int_rate + loan_percent_income, family = binomial, data = clean_data)
summary(binary_model_simple)
# Compare the models using an anova
anova(binary_model_simple, binary_model_interaction, test = "Chi")

library(boot)
glm.diag.plots(binary_model_interaction)

summary(binary_model_interaction)



# Plot the fitted model for each variable separately:
model_interaction_only <- glm(loan_status ~ loan_int_rate:loan_percent_income, family = binomial, data = clean_data)
model_loanint <- glm(loan_status ~ loan_int_rate, family = binomial, data = clean_data)
model_percentincome <- glm(loan_status ~ loan_percent_income, family = binomial, data = clean_data)

clean_data <-clean_data%>%mutate(loan_status=ifelse(loan_status==1,0,1))
par(mfrow=c(2,2))
xv <- seq(0, 20, 0.1)
yv <- predict(model_loanint, list(loan_int_rate = xv), type = "response")
plot(clean_data$loan_int_rate, clean_data$loan_status)
lines(xv, yv)

yv2 <- predict(model_percentincome, list(loan_percent_income = xv), type = "response")
plot(clean_data$loan_percent_income, clean_data$loan_status)
lines(xv, yv2)

yv3 <- predict(model_interaction_only, list(loan_percent_income = xv), type = "response")
plot(clean_data$loan_percent_income, clean_data$loan_status)


# Log-linear models for categorical data
library(gmodels)
CrossTable(clean_data$loan_status, clean_data$cb_person_default_on_file)
loglineardata <- read.csv("log linear data.csv")
loglineardata$loan_status <- as.factor(loglineardata$loan_status)
loglineardata$on_file <- as.factor(loglineardata$on_file)

model0 <- glm(y ~ loan_status + on_file, family = poisson, data = loglineardata)
model.matrix(model0)
summary(model0)
fitted(model0)

# Test for evidence for an interaction between loan status and default on file
model1 <- glm(y ~ loan_status*on_file, family = poisson, data = loglineardata)
model.matrix(model1)
fitted(model1)
summary(model1)

# The null hypothesis that model0 is correct is tested against the alternative hypothesis that model1 is correct using
# analysis of deviance

anova(model0, model1, test = "Chisq")

glm.diag.plots(binary_model_interaction)
glm.diag.plots(logistic_model2)

## A p-value of less than 0.05 suggest that model 2 is significantly better and we reject the null hypothesis that model0
# is better than model 1, thus resulting in a conclusion that there is an association between loan status and having
# a default on file.

# Checking fit of log-linear model
par(mfrow=c(2,2))
plot(model1)