df = read.csv('6414-HW8Survey.csv')
head(df)
df
library(ggplot2)
df$ProportionSatisfied <- df$Satisfied / df$TotalCount

# Create a scatter plot
ggplot(df, aes(x = Duration, y = ProportionSatisfied, color = factor(Resolved))) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Duration vs Proportion of Satisfied Customers",
       x = "Duration",
       y = "Proportion of Satisfied Customers",
       color = "Resolved") +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Resolved", "Resolved")) +
  theme_minimal()


ggplot(df, aes(x = Duration, 
               y = log((Satisfied /TotalCount)/ (1 - (Satisfied /TotalCount)))
               , color = factor(Resolved))) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Duration vs Log odds(Proportion of Satisfied Customers)",
       x = "Duration",
       y = "Log odds (Proportion of Satisfied Customers)",
       color = "Resolved") +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Resolved", "Resolved")) +
  theme_minimal()

model <- glm(ProportionSatisfied ~ Duration + Resolved, data = df, family = binomial, weights = df$TotalCount)

# Display summary of the model
summary(model)

predict(model,data.frame(Duration=3,Resolved=1))
predict(model,data.frame(Duration=3,Resolved=0))


df = read.csv('6414-HW8-Shopper.csv')
head(df)
df


# Fit the Poisson regression model
poisson_model <- glm(PurchaseCount ~ PriorWeekPurchase + LastWeekCompPurchase + Age + Region,
                     data = df, family = poisson(), offset = log(Count))

# Display the summary of the model
summary(poisson_model)


c(deviance(poisson_model),  1-pchisq(deviance(poisson_model),55))

pearres3 = residuals(poisson_model,type="pearson")
pearson = sum(pearres3^2)
c(pearson, 1-pchisq(pearson,55))
