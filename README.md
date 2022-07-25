
# Survival of Cardiovascular Heart Disease (CHD)
>The main objective of this study is to determine the factors that are critical for survival among Cardiovascular Heart Disease (CHD) Patients.
* I created a **Logistic Regression model** in **R** using **RStudio** to predict the survival of CHD patients. 
* **Extracted** the **data** from the **PHIS database** using **SQL**; built **tableau dashboards**. 
> The **model predicted the survival of CHD** with an **AUC of over .90** and indicated that a higher **EF % is a major contributing factor**.

# Data 
* The study is based on 299 patients of Cardiovascular Heart Disease (CHD) comprising of 105 women and 194 men. 
* All the patients were more than 40 years old, having left ventricular systolic dysfunction and falling in NYHA class III and IV. 
* Cardiovascular Heart Disease (CHD) was diagnosed by cardiac echo report or notes written by physician. 
* The information related to risk factors were taken from blood reports while smoking status and blood pressure were taken from physician's notes.

## Loading Data
> with the readr library
```{r}
library(readr)
survival <- read_csv("S1Data_fixed.csv")

attach (survival)
head(survival)

```
|index|TIME|Survival|Gender|Smoking|Diabetes|BP|Anaemia|Age|EF |Sodium|Serum Creatinine|platelets|CPK|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|0|97|1|0|0|0|0|1|43\.0|2|1|1|1|2|
|1|180|1|1|1|1|0|1|73\.0|0|3|1|0|1|

## Data Characteristics:
**Response variable**:
> Survival - Cardiovascular Heart Disease (CHD) Pateints that were still alive after the follow up period by the physicians. If the patient was alive by the follow up date: 1 and if not: 0.

**Predictor variables**:

* > Age, serum sodium, serum creatinine, gender, smoking, Blood Pressure (BP), Ejection Fraction (EF), anemia, platelets, Creatinine Phosphokinase (CPK) and diabetes. 

**Continuos predictors**
* > Age & Time 

**Categorical preditcors**
* > EF, serum sodium, platelets, CPK and Serum Creatinine

**Predictor Descriptions**
* > Time is follow up time was 4 to 285 days with an average of 130 days.
* > EF was divided into three levels (i.e. EF<30 (coded as 0), 30<EF<45 (coded as 1) and EF>45 (coded as 2)) and platelets was also divided into three level on the basis of quartiles. 
* > Sodium was also coded into three levels based on the quartiles
* > Serum Creatinine greater than its normal level (1.5) is an indicator of renal dysfunction. Its effect on mortality was studied as creatinine >1.5 (coded as 0) vs <1.5 (coded as 1). 
* > Anemia in patients was assessed by their haematocrit level. The patients with haematocrit less than 36 (minimum normal level of haematocrit) were taken as anemic. 

# Data Cleaning & Exploratory Analysis
**Removing Null Values**
```{r}
na.omit(survival)
```
**Skewness and Correlation Analysis:** 
1. **Histograms (*ggplot* & *tidyr* library)**
```{r}

library (ggplot2)
library (tidyr)

ggplot(gather(survival), aes(value)) + 
  geom_histogram(bins = 8) + 
  facet_wrap(~key, scales = 'free_x')

```
![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/skewnessplot.jpg?raw=true)
> There is some skewness in the data, however, the plot shows that all the predictor variables are not exteremely right or left skewed. It is also noticeable from the plot that all the varaibles are catagorical variales expect for Age and Time.

2. **Scatterplot Matrix**
```{r}
pairs (survival[,0:7], col= ifelse (Survival==1, "green", "red"))
pairs (survival[,8:13], col= ifelse (Survival==1, "green", "red"))
```

![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/Scatterplot_matrix1.jpg?raw=true)
![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/Scatterplot_matrix2.jpg?raw=true)
> These plots further emphasize the minimum skewness in the data and the categorical sense of the majority of the variables.

2. **Correlation Matrix (using *corplot library*)**
```{r}
transcorr = cor(survival [, c(1:10, 10, 12:13)], use= "complete.obs") 
library("corrplot")
corrplot (transcorr, method = "number", number.cex=0.6)
```

![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/correlation_matrix.jpg?raw=true)
> The correlation matrix shows that the varaible Time and surviaval are higly correlated (0.53). Gender and Smoking have a high correlation as well(0.45). Survaial and Serum Creatinine appears to have a moderate correlation(0.37).

3. **Odd Tables (using *xtabs* in the *MASS* library)**
* **Survival vs EF**
```{r}
library(MASS) 
table1= xtabs(~Survival+EF)
table1

```
||EF 0|EF 1|EF 2|
|---|---|---|---|
|Survival 0|51|31|14|
|Survival 1|42|115|46|

> 93 patients had an Ejection Fraction of less than 30 %, while 146 have an Ejection Fraction of 30 to 45 % and only 60 have an Ejection Fraction higher than 45% . Of all the patients that had an ejection fraction less than 30 %, 55 % died of cardio-vascular heart disease. That is 32% more than the patients who died that had an ejection fraction higher than 45%. Patients with an ejection fraction between 30 and 45 % had the highest survival rate at 79 %. You have a higher chance of dying if your ejection fraction is less than 30 %.

* **Survival vs Gender**
```{r}
library(MASS) 
table2= xtabs(~Survival+Gender)
table2

```
||Female|Male|
|---|---|---|
|Survival 0|34|62|
|Survival 1|71|132|

> Odds of surviving:
> * Male 132/194 = 0.680
> * Female 71/105 = 0.676 
> * Odds Ratio for Male VS Female: 0.680/0.676 = 1.0059

> Female patients are 1.0059 times more likely to survive cardio-vascular heart diseases than male patients. This essentially means the chances of survival for male and female patients is approximately 1 to 1.

* **Survival vs Diabetes**
```{r}
library(MASS) 
table2= xtabs(~Survival+Diabetes)
table2

```
||Diabetes 0|Diabetes 1|
|---|---|---|
|Survival 0|56|40|
|Survival 1|118|85|

> Odds of not surviving:
> * Diabetic 40/125 = 0.32
> * Non-diabetic 56/174 = 0.32 
> * Odds Ratio for Diabetic VS Non-Diabetic: 0.32/0.32 = 1

> This essentially means the chances of not surviving for Diabetic and non-Diabetic patients is 1 to 1.


* **Survival vs Sodium**
```{r}
library(MASS) 
table2= xtabs(~Survival+Sodium)
table2

```
||Sodium 0|Sodium 1|Sodium 2| Sodium 3|
|---|---|---|---|---|
|Survival 0|42|24|19|11|
|Survival 1|41|70|61|31|

> Urinary Sodium to creatine ratio or Sodium levels 0 to 3 represent the quartiles of the collected Sodium ratios, 0 being less 25th quartile and 3 being greater than 75th quartile. We can see that when the patient's Sodium to Creatine ratio is less than the 25th quartile they have less than 50% chance of surving cardio-vascular heart disaese. While pateints with Sodium to Creatine ratio greater than the 25th quartile all have higher than 70% chance of surviving cardio-vascular heart disease.


# Fitting Initial Logistic Regression Model

**Fitting**
```{r}

order1_fit1 <- glm (Survival  ~ as.factor(EF)  + `Serum Creatinine` + Diabetes + TIME + Gender + Smoking +  BP + Anaemia + Age  +  Sodium +  platelets + CPK, family = binomial) 
summary(order1_fit1)
 
```
> Deviance Residuals: 

| Min    |   1Q  | Median    |   3Q    |  Max  |
|---|---|---|---|---|
|-2.4184 | -0.4308 |  0.1899|   0.5562 |  2.7121  | 

> Coefficients: 

||   Estimate |Std. Error |z value| Pr(>abs(z)) | Significance|
|---|---|---|---|---|---|
|(Intercept)     |   -1.358485  | 1.308658  |-1.038| 0.299235 ||   
|as.factor(EF)1|      1.836955 |  0.422611 |  4.347| 1.38e-05| ***|
|as.factor(EF)2 |     1.857695 |  0.518107 |  3.586| 0.000336 |***|
|Serum Creatinine|  1.678121|   0.429314 |  3.909 |9.27e-05 |***|
|Diabetes |          -0.192139 |  0.359440 | -0.535| 0.592961 ||   
|TIME |  0.022433  | 0.003194  | 7.024| 2.15e-12 |***|
|Gender      |        0.553463 |  0.436964  | 1.267| 0.205295  | | 
|Smoking|      -0.135759  | 0.430672 | -0.315| 0.752590 ||   
|BP| -0.009470 |  0.368440 | -0.026| 0.979495 ||   
|Anaemia| -0.049583  | 0.379851 | -0.131| 0.896146 ||   
|Age|-0.046199  | 0.016617 | -2.780 |0.005432| ** |
|Sodium |0.292238  | 0.184413 |  1.585 |0.113036||    
|platelets |0.114345 |  0.165775 |  0.690 |0.490344 ||   
|CPK |-0.397067 |  0.161646 | -2.456 |0.014034| *|

> Signifance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> Null deviance: 375.35  on 298  degrees of freedom
> Residual deviance: 209.93  on 285  degrees of freedom

**Getting Coeffiecents for the fit**
```{r}
exp (order1_fit1$coefficients)
```
||(Intercept) |as.factor(EF)1|as.factor(EF)2 |Serum Creatinine|Diabetes|TIME|Gender|Smoking|BP|Anaemia|Age|Sodium|platelets|CPK|
| --- |--- |---|--- |---|---|---|---|---|---|---|---|---|---|---|
|Coefficients:|0.2570498|6.2773944| 6.4089462|5.3554847|0.8251924|1.0226867|1.7392656|0.8730532|0.9905751|0.9516266|0.9548522|1.3394224|1.1211391|0.6722891|


**Getting Confidence Intervals**
```{r}
exp (confint (order1_fit1))
```
|                 |    2.5 %  |   97.5 %|
|---|---|---|
|(Intercept)   |     0.01872896 | 3.2432977|
|as.factor(EF)1  |   2.80878250| 14.8482312|
|as.factor(EF)2  |   2.40283407 |18.5011624|
|Serum Creatinine| 2.35640903 |12.7988731|
|Diabetes    |       0.40569020 | 1.6706351|
|TIME |    1.01669786 | 1.0295687|
|Gender |  0.74725273 | 4.1808057|
|Smoking | 0.37153732 | 2.0267428|
|BP |0.48198078 | 2.0566115|
|Anaemia |0.45160198 | 2.0164303|
|Age |0.92319656  |0.9856701|
|Sodium | 0.93674051 | 1.9377001|
|platelets|0.81029581 | 1.5569596|
|CPK |0.48416180 | 0.9155519|

> The model shows that pateints having an ejection fraction between 30 to 45 %  and higher 45 % are significantly different from patients who have an ejection fraction of less than 30 %. The model also shows that Serum Creatinine, Time, Age and CPK are significant predictors for predicting the survival of cardio-vascular heart disease. For every unit increase in Serum Creatinine the odds of surviving cardio-vascular disease go up by 5.3554847-folds. The odds of surviving cardio-vascular heart disease for male pateints vs female pateints is not only 1 to 1 as we predicted earlier but essentially ranges from 0.74725273 to  4.1808057 -folds. 

## Model Evalution

**Jittered *Actuals* vs *Predicted* logits**
```{r}

order1_fit1.logit = predict (order1_fit1)
plot (jitter (Survival, 0.2) ~ order1_fit1.logit, data=survival)

pihat.order1_fit1 = predict (order1_fit1, type='response')
pihat.ord = order (pihat.order1_fit1)
lines (order1_fit1.logit [pihat.ord], pihat.order1_fit1 [pihat.ord])
lines (lowess (order1_fit1.logit [pihat.ord], pihat.order1_fit1 [pihat.ord]), col='red', lty=2)

```
![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/jitteredActualsvsPredictedLogits.jpg?raw=true)
> The jittered response vs. predicted values with the fitted logistic curve and a lowess fit shows that the model fits the data aaproximately well since the solid and the jittered line are approximately similar in shape.

**Residuals Plots**
```{r}
plot(predict(order1_fit1),residuals(order1_fit1))
abline(h=0,lty=1,col="brown")

par (mfrow=c(1,2)) 
plot (order1_fit1, which=c(5))

```
![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/residualsplot.jpg?raw=true)
![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/residualsLEVERAGEplot.jpg?raw=true)
> The linaerity condition is statisfied because the plot is flat and right on zero. There are no points with high leverage or high Cook's Distance therefore those influence measures do not appear on the plot. The point index 240 has a standerized residual around 6. We will see whether it is an influential point.

## Inference Analysis to confirm that there is *no outliers*:
**Pull out the cases with high leverage and large residuals.**
```{r}
survival$Residual = round (order1_fit1$residuals, 4)
survival$leverage = round (hatvalues(order1_fit1), 4)
survival$rstudent = round (rstudent(order1_fit1), 4)

high.levg.resd = survival [survival$leverage > 3*(order1_fit1$rank) / (order1_fit1$rank + order1_fit1$df.residual) |abs (order1_fit1$residuals) > 30 , ]

high.levg.resd[order(-high.levg.resd$rstudent),][c(1,9:17)]

```
||TIME  |  EF| Sodium |Serum Creatinine |platelets  | CPK| index |Residual| leverage| rstudent|
|---|---|---|---|---|---|---|---|---|---|---|
|1 |30|0 |1|0 |3|3|240 |39.6|0.0131|2.81|
> Index 240 has a large residual value, however, its 0.0131 leverage value is not bigger than the Leverage Cutoff value at 0.13043478 (3(k+1)/n). Therefore, this point is an influential point. We will see whether there are any other influential points that do not have high residuals as index 240.

```{r}
high.Leverage = survival [survival$leverage > 0.13043478 & is.na(survival$leverage) == FALSE, ]
high.Leverage[order(-high.Leverage$leverage),][c(1,9:17)]

```
||TIME  |  EF| Sodium |Serum Creatinine |platelets  | CPK| index |Residual| leverage| rstudent|
|---|---|---|---|---|---|---|---|---|---|---|
|1  |  94  |   2  |    2 |  0     |    3  |   0 |  242   |  2.55 |   0.134  |   1.45|
|2 |  172   |  0   |   3 |  0    |     3  |   3  | 222  |  -1.65 |   0.132  |  -1.05|
> There are only 2 points with leverage values higher than the Leverage Cutoff value at 0.13043478 (3(k+1)/n). The point index 242 has the highest leverage value of 0.134.
## Test for Multi-colinearity

```{r}
car::vif(order1_fit1)

```
| |GVIF| Df| GVIF^(1/(2*Df))|
|---|---|---|---|
|as.factor(EF)   |   1.328792 | 2 |       1.073654|
|Serum Creatinine |1.166354 | 1    |    1.079979|
|Diabetes |          1.044061 | 1  |      1.021793|
|TIME     |          1.280348 | 1  |      1.131525|
|Gender    |         1.464522 | 1   |     1.210174|
|Smoking    |        1.343264 | 1    |    1.158993|
|BP     |            1.063805 | 1  |      1.031409|
|Anaemia    |        1.172183 | 1    |    1.082674|
|Age     |           1.208652 | 1  |      1.099387|
|Sodium     |        1.122267 | 1  |      1.059371|
|platelets  |        1.088017 | 1    |    1.043081|
|CPK      |          1.254166 | 1       | 1.119896|

> All of the VIF are less than 5. Hence, multi-colinearity is low in our model. We will be doing stepwise regresion to find the best model.

# Final Model Selction using Step Functions

## AIC Step Function

```{r}
step.AIC = step (order1_fit1, direction = "both")
summary(step.AIC)
beta2 = coefficients(step.AIC)
exp (beta2)
exp (confint (step.AIC))
```
## BIC Step Function
```{r}
step.BIC = step(order1_fit1,  direction = "both", k=log (order1_fit1$rank  + order1_fit1$df.residual))
summary(step.BIC)
```

## ANOVA for Initial AIC & BIC produced models
```{r}
anova ( step.BIC, step.AIC )
pchisq(2.0858, 1,lower.tail = FALSE)
```
**Analysis of Deviance Table**
|Model|Resid. Df| Resid. Dev |Df| Deviance|
|---|---|---|---|---|
|AIC |      292 |    214.42  |   |       |
|BIC |      291  |   212.34 | 1 |  2.0858|
ANOVA model pvalue: 0.1486743
> We choose the AIC model to do the interaction effects on becuase it has significant predictors and has a lower residual deviance of 214.42 Residual Deviance on 292 degrees of freedom. The ANOVA pvalue  (0.1486743) which is less than 0.05 (the typical cutoff).

## Adding Interaction Effects to the initial model
```{r}

# Center quantitative predictors and add interaction effects

my.center = function (y) y - mean (y)

survival$TIME.c = my.center (survival$TIME)
survival$Age.c = my.center (survival$Age)


fit1_InteractionEffects = glm (Survival ~ (as.factor(EF) + TIME.c + CPK + `Serum Creatinine` + Age.c)^2,
               data=survival, 
               family = binomial)
summary (fit1_InteractionEffects)

```
## Final Stepwise Regression 
**Step-wise with AIC**
```{r}
step.AIC_final = step (fit1_InteractionEffects, direction = "both")
summary(step.AIC_final)

```
**Step-wise with BIC**
```{r}
step.BIC_final = step (fit1_InteractionEffects, direction = "both",k=log (fit1_InteractionEffects$rank  + fit1_InteractionEffects$df.residual))
summary(step.BIC_final)
```
## ANOVA for Final AIC & BIC produced models
```{r}
anova (step.AIC_final, step.BIC_final)
pchisq(11.949, 3, lower.tail = FALSE)
```
**Analysis of Deviance Table**
|Model|Resid. Df| Resid. Dev |Df| Deviance|
|---|---|---|---|---|
|Final AIC |      289 |    202.47  |   |       |
|Final BIC |      292  |   214.42 | -3 | -11.949|
ANOVA model pvalue: 0.007559923

> We choose AIC model as final becuase it's Residual Deviance is 202.47 which less than the Residual Deviance 214.42 of the BIC model. The pvalue (0.0075599) is less than 0.05 which means that the two models are statistically differnt from each other.

# Final Model
```{r}
final_model_1 = step.AIC_final
summary(final_model_1)
```
> Deviance Residuals:

| Min   |     1Q  |  Median     |   3Q    |   Max|  
|---|---|---|---|---|
|-2.71103 | -0.49728 |  0.08546 |  0.50304 |  2.33555  |

> Coefficients:

|                        | Estimate| Std. Error |z value| Pr(>abs(z)) | Significance|  
|---|---|---|---|---|---|
|(Intercept) |  -0.683582 |  0.423220 | -1.615 | 0.10627 |  | 
|as.factor(EF)1| 2.555116  | 0.564167 |  4.529| 5.93e-06 |*** |
|as.factor(EF)2| 1.955026 |  0.694497 |  2.815 | 0.00488| ** |
|TIME.c| 0.008115|   0.005224  | 1.553  |0.12032 ||   
|CPK|-0.401538 |  0.156240 | -2.570  |0.01017| *  |
|Serum Creatinine | 1.924515 |  0.443882 |  4.336 |1.45e-05| *** |
|Age.c  |-0.050382  | 0.016646 | -3.027 | 0.00247| ** |
|as.factor(EF)1:TIME.c | 0.022274 |  0.008176 |  2.724 | 0.00644| ** |
|as.factor(EF)2:TIME.c | 0.008078  | 0.009461  | 0.854 | 0.39321 ||   
|TIME.c:Serum Creatinine | 0.010449 |  0.006271 |  1.666 | 0.09565| . |

> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Interaction Plots

```{r}
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

qplot (TIME, predict (final_model_1), data=survival, color=categorize (EF)) + geom_smooth (method="lm")

qplot (`Serum Creatinine`, predict (final_model_1), data=survival, color=categorize (TIME)) + geom_smooth (method="lm")

```

![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/interactionplot1.jpg?raw=true)
![alt text](https://github.com/kovenda/Survival-of-Cardiovascular-Heart-Disease-CHD-/blob/main/interactionplot2.jpg?raw=true)

> The interaction plot between time and Ejection Fracton shows that the predicted logits of suriving cardio-vascular disease increases over time for all ejection fraction (EF) categories(EF<30 (coded as 0), 30<EF<45 (coded as 1)). Patients with 30<EF<45 have the highest rise in the predicted logits of suriving cardio-vascular disease over time. We had mentioned earlier with our frequency table that patients with an ejection fraction (EF) between 30 and 45 % had the highest survival rate at 79 %. However, with this interaction plot with time we see that statement holds true only for patients whose follow up time was longer than 100 days. We see that for patients with a follow up time of less than 25 days, they have lowest predicted logits of suriving cardio-vascular disease. 

> The interaction plot between Serum Creatinine and time shows the predicted logits of surviving cardio-vascular disease increases over time because C1 (25th quartile of follow time) is the lowest and C4 (75th quartile of follow time) is the highest in the graph. This effect was already accounted for in the previuos interaction plot. Serum Creatinine was coded as follows, >1.5(indicator of renal dysfunction coded as 0) and < 1.5(normal level coded as 1). We can see from the positive slope of all four of the TIME graphs that all patients with renal dysfunction (0) have lower predicted logits of surviving than those who do not (1). All patients with follow up time in C4 (75 quartile),regardless of whether they have renal dysfuntion, have higher predicted logits of suriving cardio-vascular disease than all other patients in C1,2 & 3, regardless of whether those patients do not have renal dysfuntion. Patients with follow up time in C4 (75 quartile), also have the most noticeable change in the predicted logits for surviving cardio-vascular disease between patients with renal dysfunction(0) and patients without (0). 





