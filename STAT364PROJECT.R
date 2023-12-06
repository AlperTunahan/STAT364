


#data handling
data <- read.csv("8_Module2_Sim2_Data_V1.csv", sep = ";")
attach(data)
data$APR_offered = as.factor(data$APR_offered)

# removing unwanted parts
data <- data[,c(-1, -2, -9, -11)]
str(data)

summary(data)

model1 <- lm(Avg_monthly_spend ~ ., data = data)
summary(model1)

plot(model1)

library(formattable)

AID::boxcoxnc(data$Avg_monthly_spend)

a <- car::vif(model1)
plot(model1)

# Normality check for resiudals
hist(model1$residuals, probability = T)
lines(density(rnorm(1001, mean(model1$residuals), sd(model1$residuals))))

# Normality check for response
set.seed(42)
hist(data$Avg_monthly_spend, probability = T)
lines(density(rnorm(1001, mean(data$Avg_monthly_spend), sd(data$Avg_monthly_spend))))

# Research Question 1

summary(model1)
install.packages("sjPlot")
library(sjPlot)
installed.packages("sjmisc")
library(sjmisc)
install.packages("sjlabelled")
library(sjlabelled)

plot(model1, c(1,2))

tab_model(model1)
effectsize::standardize_parameters(model1)

# Research Question 2

fit2 = lm(Travel_spend  ~ Age_group, data)
summary(fit2)

tab_model(fit2)

p<-ggplot(Travel_spend, aes(x=dose, y=len, color=Age_group)) +
  geom_boxplot()
p



plot(fit2 ,c (1,2))
hist(data$Travel_spend)
lines(density(rnorm(1001, mean(data$Travel_spend), sd(data$Travel_spend))))

AID::boxcoxnc(data$Travel_spend)


set.seed(42)
shapiro.test(sample(data$Travel_spend^0.43, 50))


ggplot(data, aes(x=Age_group, y=Travel_spend,fill=Age_group))+
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none",
        plot.title = element_text(size=11)
  )

library(viridis)
ggplot(data, aes(x=Age_group, y=Travel_spend,fill=Age_group)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme(legend.position="none",
        plot.title = element_text(size=11)
  )

ggplot(data, aes(x=Age_group, y=Travel_spend,fill=Age_group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(legend.position="none",
        plot.title = element_text(size=11)
  ) 


library(ggridges)
ggplot(data, aes(x = Travel_spend, y = Age_group, fill = Age_group)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# Research Question 3

ggplot(data, aes(x = APR_offered, y = Avg_Monthly_balance, fill = APR_offered)) +
  geom_boxplot()

hist(data$Avg_Monthly_balance)

AID::boxcoxnc(data$Avg_Monthly_balance)

fit3 = lm(Avg_Monthly_balance ^ 0.33 ~ APR_offered, data)
summary(fit3)

tab_model(fit3)

plot(fit3, c(1,2)) # No relation

# Research Question 4
data[data$Income_Group == "9000-12000",]

ggplot(data, aes(x = Income_Group, y = Avg_Monthly_balance, fill = Income_Group)) +
  geom_boxplot()

AID::boxcoxnc(data$Avg_monthyl_payrate)

fit4 <- lm(Avg_Monthly_balance ^ 0.33  ~ student_loan_ind + Income_Group, data)
summary(fit4)

AID::boxcoxnc(Avg_Monthly_balance)

tab_model(fit4)

plot(fit4)

hist(fit4$residuals, probability = T)

AID::boxcoxnc(Avg_Monthly_balance)


