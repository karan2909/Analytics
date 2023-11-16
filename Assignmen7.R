df = read.csv('6414-HW7-F23-Shop.csv')
head(df)
Year <- rep(2016:2022, each = 12)
df$Years = Year
# Load the necessary libraries
library(ggplot2)

# Create a time series plot with selective x-axis labels
ggplot(df, aes(x = factor(paste(Years, Month, sep = " "), levels = unique(paste(Years, Month, sep = " "))), y = Sales)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Sales of a Gift Shop",
       x = "Year and Month",
       y = "Sales") +
  theme_minimal() +
  scale_x_discrete(breaks = unique(paste(df$Years, df$Month, sep = " "))[c(1,12, 24,36, 48,60, 72,84)],
                   labels = c("2016 Jan","2016 Dec", "2017 Dec","2018 Dec","2019 Dec", "2020 Dec","2021 Dec","2022 Dec")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hist(as.numeric(df$Sales))

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Ensure that the "Month" factor variable has all 12 levels
df$Month = factor(df$Month, levels = month_levels)

# Create the "Time" variable
Time = 1:length(df$Sales)

# Fit the linear regression model
model = lm(log(df$Sales) ~ Time + I(Time^2) + factor(df$Month))

summary(model)

new_month_levels = c("Jan", "Feb")

# Create a new dataset for prediction
new_data <- data.frame(
  Time = c(85, 86),
  Month = factor(c("Jan", "Feb"), levels = new_month_levels))

# Predict the sales for month 85 (January) and month 86 (February)
predict(model, newdata = new_data)


Sales = df$Sales
ln_HR = log(Sales)
n=length(ln_HR)
t=1:n
#Monthly seasonality
mon = 1:12
mon = rep(mon,n/12)
model = lm(ln_HR~t+I(t^2)+factor(mon))
summary(model)

pred_sales=predict(model,newdata=data.frame(t=85:86,mon=1:2),interval="prediction")
exp(pred_sales)


#########
data = read.csv('6414-HW7-MPG.csv')

#install.packages("Metrics")
library(Metrics)
library(MASS)

# Fit the full model (all independent variables)
full_model <- lm(mpg ~ ., data = data)
summary(full_model)
# Forward Stepwise Regression
forward_stepwise = step(lm(mpg~1,data = data), scope=formula(full_model), direction="forward")
# Backward Stepwise Regression
backward_stepwise <- stepAIC(full_model, direction = "backward")

# Print the results
summary(forward_stepwise)
summary(backward_stepwise)


###
df = read.csv('6414-HW7-Discrim.csv')
df
model = glm(HIRE ~ .,data = df, family=binomial)
summary(model)

round(c(deviance(model), 1-pchisq(deviance(model),24)),2)
pearres4 = residuals(model,type="pearson")
pearson = sum(pearres4^2)
round(c(pearson, 1-pchisq(pearson,24)),2)
new_data <- data.frame(EDUC = 6, EXP = 5, GENDER = 0)
probability <- predict(model, newdata = new_data, type = "response")
probability
#probability <- predict(model, newdata = new_data, type = "response")

1 -pchisq(641.496-19.808,10)


