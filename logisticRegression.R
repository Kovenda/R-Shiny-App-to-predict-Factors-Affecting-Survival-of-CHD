library(readr)
survival <- read_csv("S1Data_fixed.csv")

attach (survival)
head(survival)

na.omit(survival)

library (ggplot2)
library (tidyr)

#jpeg('skewnessplot.jpg')

ggplot(gather(survival), aes(value)) + 
  geom_histogram(bins = 8) + 
  facet_wrap(~key, scales = 'free_x')

#dev.off()

jpeg('Age_boxplot1.jpg')
ggplot(survival, aes(x=factor(Survival), y=Age, fill=factor(Survival))) + 
  geom_boxplot(width=0.6) +
  stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..), color=factor(Survival)),
               position=position_nudge(x=0.40), size=3.5) +
  theme_bw()
dev.off()

# Scatterplot matrix of predictors
#jpeg('Scatterplot_matrix1.jpg')
pairs (survival[,0:7], col= ifelse (Survival==1, "green", "red"))
#dev.off()

jpeg('Scatterplot_matrix2.jpg')
pairs (survival[,8:13], col= ifelse (Survival==1, "green", "red"))
dev.off()

transcorr = cor(survival [, c(1:10, 10, 12:13)], use= "complete.obs") 
library("corrplot")

#jpeg('correlation_matrix.jpg')
## corrplot 0.84 loaded
corrplot (transcorr, method = "number", number.cex=0.6)
#dev.off()

library(MASS) 
table1= xtabs(~Survival+EF)
table1

order1_fit1 <- glm (Survival  ~ as.factor(EF)  + `Serum Creatinine` + Diabetes + TIME + Gender + Smoking +  BP + Anaemia + Age  +  Sodium +  platelets + CPK, family = binomial, survival) 
summary(order1_fit1)

exp (order1_fit1$coefficients)

exp (confint (order1_fit1))

#jpeg('jitteredActualsvsPredictedLogits.jpg')
# Make a plot similar to Y vs Y-hat for linear regression
order1_fit1.logit = predict (order1_fit1)
plot (jitter (Survival, 0.2) ~ order1_fit1.logit, data=survival)
# Add the overall fitted logistic curve to the plot, and a lowess fit
pihat.order1_fit1 = predict (order1_fit1, type='response')
pihat.ord = order (pihat.order1_fit1)
lines (order1_fit1.logit [pihat.ord], pihat.order1_fit1 [pihat.ord])
lines (lowess (order1_fit1.logit [pihat.ord], pihat.order1_fit1 [pihat.ord]), col='red', lty=2)
#dev.off()

#jpeg('residualsplot.jpg')
plot(predict(order1_fit1),residuals(order1_fit1))
abline(h=0,lty=1,col="brown")
#dev.off()

#jpeg('residualsLEVERAGEplot.jpg')
par (mfrow=c(1,2)) 
plot (order1_fit1, which=c(5))
#dev.off()

survival$Residual = round (order1_fit1$residuals, 4)
survival$leverage = round (hatvalues(order1_fit1), 4)
survival$rstudent = round (rstudent(order1_fit1), 4)

high.levg.resd = survival [survival$leverage > 3*(order1_fit1$rank) / (order1_fit1$rank + order1_fit1$df.residual) |abs (order1_fit1$residuals) > 30 , ]

high.levg.resd[order(-high.levg.resd$rstudent),][c(1,9:17)]

high.Leverage = survival [survival$leverage > 0.13043478 & is.na(survival$leverage) == FALSE, ]
high.Leverage[order(-high.Leverage$leverage),][c(1,9:17)]

car::vif(order1_fit1)

step.AIC = step (order1_fit1, direction = "both")
summary(step.AIC)
beta2 = coefficients(step.AIC)
exp (beta2)
exp (confint (step.AIC))

step.BIC = step(order1_fit1,  direction = "both", k=log (order1_fit1$rank  + order1_fit1$df.residual))
summary(step.BIC)


summary(step.AIC)
summary(step.BIC)


anova ( step.BIC, step.AIC )
pchisq(2.0858, 1,lower.tail = FALSE)

# Center quantitative predictors and add interaction effects

my.center = function (y) y - mean (y)

survival$TIME.c = my.center (survival$TIME)
survival$Age.c = my.center (survival$Age)


fit1_InteractionEffects = glm (Survival ~ (as.factor(EF) + TIME.c + CPK + `Serum Creatinine` + Age.c)^2,
               data=survival, 
               family = binomial)
summary (fit1_InteractionEffects)

step.AIC_final = step (fit1_InteractionEffects, direction = "both")
summary(step.AIC_final)

step.BIC_final = step (fit1_InteractionEffects, direction = "both",k=log (fit1_InteractionEffects$rank  + fit1_InteractionEffects$df.residual))
summary(step.BIC_final)

summary(step.AIC_final)
summary(step.BIC_final)

anova (step.AIC_final, step.BIC_final)
pchisq(11.949, 3, lower.tail = FALSE)

final_model_1 = step.AIC_final
summary(final_model_1)


### Interaction plots

# New categorize function

categorize = function (x, quantiles=(1:3)/4) {
  cutoffs = quantile (x, quantiles)
  n.cutoffs = length (cutoffs)
  result = rep ("C1", length (x))
  for (j in 1:n.cutoffs) {
    result [x > cutoffs [j]] = paste ("C", j+1, sep="")
  }
  return (result)
}

library (ggplot2)
#jpeg('interactionplot1.jpg')
qplot (TIME, predict (final_model_1), data=survival, color=categorize (EF)) + geom_smooth (method="lm")
#dev.off()


categorize = function (x, quantiles=(1:3)/4) {
  cutoffs = quantile (x, quantiles)
  n.cutoffs = length (cutoffs)
  result = rep ("C1", length (x))
  for (j in 1:n.cutoffs) {
    result [x > cutoffs [j]] = paste ("C", j+1, sep="")
  }
  return (result)
}

library (ggplot2)
#jpeg('interactionplot2.jpg')
qplot (`Serum Creatinine`, predict (final_model_1), data=survival, color=categorize (TIME)) + geom_smooth (method="lm")
#dev.off()

exp (confint (final_model_1))

exp (final_model_1$coefficients)

preds = predict (final_model_1, se.fit = T) 
pred.df = cbind.data.frame (survival, as.data.frame (preds)) 
pred.df$lwr = pred.df$fit - 1.96 * pred.df$se.fit 
pred.df$upr = pred.df$fit + 1.96 * pred.df$se.fit 
pred.df$fit.pr = round (exp (pred.df$fit) / (1 + exp (pred.df$fit)), 3) 
pred.df$lwr.pr = round (exp (pred.df$lwr) / (1 + exp (pred.df$lwr)), 3) 
pred.df$upr.pr = round (exp (pred.df$upr) / (1 + exp (pred.df$upr)), 3)
head(pred.df)

# Predictions for patients in their 70s and older. 
pred.df[c(2, 17, 88, 48, 40),c(1,2,3,8,17:20)]

# Predictions for patients in their 40s and 50s  
pred.df[c(103, 66, 83, 21, 11),c(1,2,3,8,17:20)]

# Diagnostic plots for model hp9
#jpeg('final_Diagnosticplot.jpg')
par (mfrow=c(1,2))
plot (jitter (Survival, 0.2) ~ predict (final_model_1), data=survival)
logit.9 = predict (final_model_1)
ord9 = order (logit.9)
pihat.9 = exp (logit.9) / (1 + exp (logit.9))
lines (logit.9 [ord9], pihat.9 [ord9])

# Lowess fit
lines (lowess (logit.9, survival$Survival), col='red', lty=2)
plot (final_model_1, which=1)
#dev.off()

# Likelihood ratio test for first model

pchisq (order1_fit1$null.deviance - order1_fit1$deviance,
        order1_fit1$df.null - order1_fit1$df.residual, lower.tail = F)


# Likelihood ratio test for final model

pchisq (final_model_1$null.deviance - final_model_1$deviance,
        final_model_1$df.null - final_model_1$df.residual, lower.tail = F)
        
# Goodness of fit test

pchisq (final_model_1$deviance, final_model_1$df.residual, lower.tail = F)        
        
 #jpeg('final_residualLEVERAGE.jpg')
plot(final_model_1,which = 5)
#dev.off()

# ROC Curve for model final_model_1

# ROC curve - install package ROCR
#jpeg('final_ROCcurve.jpg')
par (mfrow=c(1,1))
library(ROCR)
pred1 <- prediction(final_model_1$fitted.values, final_model_1$y)
perf1 <- performance(pred1,"tpr","fpr")
auc1 <- performance(pred1,"auc")@y.values[[1]]
auc1
plot(perf1, lwd=2, col=2)
abline(0,1)
legend(0.6, 0.3, c(paste ("AUC=", round (auc1, 4), sep="")),   lwd=2, col=2)

# Extract the X and Y values from the ROC plot, as well as the probability cutoffs
roc.x = slot (perf1, "x.values") [[1]]
roc.y = slot (perf1, "y.values") [[1]]
cutoffs = slot (perf1, "alpha.values") [[1]]

auc.table = cbind.data.frame(cutoff=pred1@cutoffs, 
                             tp=pred1@tp, fp=pred1@fp, tn=pred1@tn, fn=pred1@fn)
names (auc.table) = c("Cutoff", "TP", "FP", "TN", "FN")
auc.table$sensitivity = auc.table$TP / (auc.table$TP + auc.table$FN)
auc.table$specificity = auc.table$TN / (auc.table$TN + auc.table$FP)
auc.table$FalsePosRate = 1 - auc.table$specificity
auc.table$sens_spec = auc.table$sensitivity + auc.table$specificity

# Find the row(s) in the AUC table where sensitivity + specificity is maximized

auc.best = auc.table [auc.table$sens_spec == max (auc.table$sens_spec),]
auc.best

# Plot the maximum point(s) on the ROC plot

points (auc.best$FalsePosRate, auc.best$sensitivity, cex=1.3)
#dev.off()

# saving the model
#saveRDS(final_model_1, file = "/Volumes/GoogleDrive/My Drive/Luther College 3rd Year/Fall 2020/Math 327/Projects/Project 2/realCHDclassiffier/chdsurvivalClassifeir/logistic_regression_model.rda")



        
        












































































