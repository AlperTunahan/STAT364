# STAT 364 CODES FINAL PROJECT (18 June 2021)

# Alper Tunahan OZTURK    2290856
# Dayanch AKMYRADOV       2347292
# Hamza Anil YILDIRIM     2218386
# Kubilay TASYUREK        2218329

library(AID)
library(ggplot2)
library(sjPlot)
library(MASS)

#data handling
data <- read.csv("data.csv", sep = ";")
data$APR_offered = as.factor(data$APR_offered)

# removing unwanted parts
data <- data[,c(-1, -2, -9, -11)]

str(data)
summary(data)
attach(data)

####################################################################
### Research Question 1 ###

# fit the model
fit1 <- lm(Avg_monthly_spend ~ ., data = data)
summary(fit1)

# check for multicollinearity
car::vif(fit1)

# plot residual vs fitted and normal q-q
par(mfrow=c(1,2))
plot(fit1, c(1,2))

# normality test for avg_monthly_spend (response)
shapiro.test(Avg_monthly_spend)

# apply transformation
boxcoxnc(Avg_monthly_spend)

# Normality check for residuals
par(mfrow=c(1,1))
hist(fit1$residuals, probability = T)
lines(density(rnorm(1001, mean(fit1$residuals), sd(fit1$residuals))))

# Normality check for response
hist(Avg_monthly_spend, probability = T)
lines(density(rnorm(1001, mean(Avg_monthly_spend), sd(Avg_monthly_spend))))

# fit the model for transformed data
fit1 <- lm(Avg_monthly_spend^0.63 ~ ., data = data)
summary(fit1)

# Check for residual vs fitted 
plot(fit1, 1) # variance is not constant

# model output
tab_model(fit1)

# standardized estimates
effectsize::standardize_parameters(fit1)


####################################################################
### Research Question 2 ###

# boxplot for travel spend of age groups
ggplot(data, aes(x=Age_group, y=Travel_spend,fill=Age_group)) +
  geom_boxplot() + xlab("Age Group") + ylab("Travel Spend") +
  labs(fill = "Age Group") +
  ggtitle("Boxplot of Travel Spend vs Age Group")

# fit the model
fit2 = lm(Travel_spend  ~ Age_group, data)
summary(fit2) # estimates are not significant

# model output
tab_model(fit2)

# plot residual vs fitted and normal q-q
par(mfrow=c(1,2))
plot(fit2, c(1,2))

hist(Travel_spend)

# transformation on data
boxcoxnc(Travel_spend)

# robust regression
huber <- rlm(Travel_spend ~ Age_group, data)
summary(huber) # estimates are not significant

bisquare <- rlm(Travel_spend ~ Age_group, data, psi = psi.bisquare)
summary(bisquare) #estimates are not significant

####################################################################
### Research Question 3 ###

# boxplot for average monthly balance of apr offered
ggplot(data, aes(x=APR_offered, y=Avg_Monthly_balance,fill=APR_offered)) +
  geom_boxplot() + xlab("APR offered") + ylab("Average Monthly Balance") +
  labs(fill = "APR offered") +
  ggtitle("Annual Percentage Rate Offered vs Average Monthly Balance")

# fit the model
fit3 = lm(Avg_Monthly_balance ~ APR_offered, data)
summary(fit3) 

hist(Avg_Monthly_balance)
par(mfrow=c(1,2))
plot(fit3, c(1,2))

# transformation on data
boxcoxnc(Avg_Monthly_balance)

# fit the model for transformed data
fit3 = lm(Avg_Monthly_balance ^ 0.33 ~ APR_offered, data)
summary(fit3)

# model output
tab_model(fit3)

# residuals vs fitted (for constant variance)
par(mfrow=c(1,1))
plot(fit3, 1) #seems fine

####################################################################
### Research Question 4 ###

# boxplot for balance of student loan indicator
ggplot(data, aes(x=as.factor(student_loan_ind), y=Avg_Monthly_balance,fill=as.factor(student_loan_ind))) +
  geom_boxplot() +xlab("Student Loan Indicator")+ylab("Average Monthly Balance")+labs(fill="Student Loan Id")+
  ggtitle("Boxplot of Student Loan Indicator vs Average Monthly Balance ")

# boxplot for balance of income groups
data$Income_Group <- factor(data$Income_Group,levels=c("0-3000","3000-6000","6000-9000","9000-12000","12000+"))
ggplot(data, aes(x = Income_Group, y = Avg_Monthly_balance, fill = Income_Group)) +xlab("Income Group")+
  ylab("Average Monthly Balance")+ggtitle("Boxplot of Income Group vs Average Monthly Balance")+geom_boxplot() +
  labs(fill = "Income Group")

# no need for checking normality, see research question 3

# fit the model for transformed data
fit4 <- lm(Avg_Monthly_balance ^ 0.33  ~ student_loan_ind + Income_Group, data)
summary(fit4)

# model output
tab_model(fit4)



