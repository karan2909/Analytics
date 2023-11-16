data = read.csv('6414-HW6-Shipment.csv')
head(data)
model <- lm(Cost ~ Weight + Distance, data=data)
summary(model)

mean(model$residuals)

par(mfrow=c(1,2))
qqnorm(residuals(model))
qqline(residuals(model))
hist(residuals(model), main="Histogram of residuals",xlab="Residuals")

par(mfrow=c(1,2))
plot(data$Cost, resid(model), main="Predictor_vs_Residuals")
abline(0, 0)
plot(fitted(model), resid(model), main="Fitted vs Residuals", xlab="Fitted Values")
abline(0, 0)


new_data = data.frame(Weight=c(6),Distance=c(60))
#CI
predict(model,new_data,interval = "confidence",level =0.95)
# PI
predict(model,new_data,interval = "prediction",level=0.95)

# Fit the full second-order model
full_model <- lm(Cost ~ Weight + Distance + I(Weight^2) + I(Weight*Distance), data = data)

# Check the summary of the full model
summary(full_model)

mean(full_model$residuals)
qqnorm(residuals(full_model))
qqline(residuals(full_model))

plot(fitted(full_model), resid(full_model), main="Fitted vs Residuals", xlab="Fitted Values")


## 
data = read.csv('6414-HW6-Hospital.csv')
head(data)

## Evaluate the scatter plot matrix of the data, ignoring the first column
par(mfrow = c(1, 1))
plot(data)
## Explore the correlation coefficients
round(cor(data), 2)
model <- lm(Hours ~ Xray+BedDays+Length, data=data)
summary(model)

rstandard(model)

options(repr.plot.width = 3, repr.plot.height = 3)
plot(fitted(model), rstandard(model))
abline(0,0)


cooks.distance(model)

options(repr.plot.width = 3, repr.plot.height = 3)
plot(cooks.distance(model))

library(car)
vif(model)

summary(model)$r.squared
