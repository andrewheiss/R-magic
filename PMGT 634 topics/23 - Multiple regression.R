library(car)
load("~/Documents/BYU 2011-2012/Winter 2012/PMGT 634/R magic/PMGT 634 datasets/stineFoster23.RData")

model <- lm(sales ~ income + competitors, data = mallSales)
summary(model)

sales.income <- lm(sales ~ income, data = mallSales)
competitors.income <- lm(sales ~ competitors, data = mallSales)

summary(sales.income)
summary(competitors.income)

# Calibration plot
plot(mallSales$sales ~ fitted(model))
calibration <- lm(mallSales$sales ~ fitted(model))
summary(calibration)  # same R² as summary(model)

# Residual plot
plot(residuals(model) ~ fitted(model))

# Or, using car - even better!
residualPlots(model)

# Normality
qqPlot(residuals(model))

# Summary of model
summary(model)
anova(model)

# 95% confidence intervals of coefficients
confint(model)

# Prediction intervals
explanatory <- data.frame(income = 70, competitors = 3)
predict(model, newdata = explanatory, interval = "confidence")
predict(model, newdata = explanatory, interval = "prediction")



# Weird ANOVA stuff
prestige.mod.1 <- lm(prestige ~ education + log2(income) + type, data=na.omit(Prestige))
summary(prestige.mod.1)

prestige.mod.0 <- update(prestige.mod.1, . ~ . - log2(income))
summary(prestige.mod.0)

anova(prestige.mod.0, prestige.mod.1)

# Sequential ANOVA
anova(prestige.mod.1)
# prestige ~ 1 vs. prestige ~ education
# prestige ~ education vs. prestige ~ education + log2(income)
# prestige ~ education + log2(income) vs. prestige ~ education + log2(income) + type

# ANOVA for the model itself vs. residuals (like in the book)
anova(model, update(model,~1)) 


