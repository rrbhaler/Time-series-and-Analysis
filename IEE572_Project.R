Project_data  <- read.csv(file='IEE572_Project',header=TRUE, sep=",")
Project_data
fit1 <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=Project_data)
summary(fit1)
anova(fit1)
step(lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=Project_data), direction = 'backward') #stepwise backward variable selection
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit1) 

fit2 <- lm(Y2 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=Project_data)
summary(fit2)
anova(fit2)
step(lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=Project_data), direction = 'backward') #stepwise backward variable selection
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit2)

fit3 <- lm(Y3 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=Project_data)
summary(fit3)
anova(fit3)
step(lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=Project_data), direction = 'backward') #stepwise backward variable selection
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit2)

# Other useful functions 
coefficients(fit1) # model coefficients
confint(fit1, level=0.95) # CIs for model parameters 
fitted(fit1) # predicted values
residuals(fit1) # residuals
vcov(fit1) # covariance matrix for model parameters 
influence(fit1) # regression diagnostics