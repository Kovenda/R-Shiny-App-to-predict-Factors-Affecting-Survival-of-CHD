
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

## Data Cleaning
> *Removing Null Values*
```{r}
na.omit(survival)
```
> *Skewness Evaluation: Histograms (**ggplot** & **tidyr** library)*
```{r}

library (ggplot2)
library (tidyr)

ggplot(gather(survival), aes(value)) + 
  geom_histogram(bins = 8) + 
  facet_wrap(~key, scales = 'free_x')

```
![alt text](https://github.com/[kovenda]/[Survival-of-Cardiovascular-Heart-Disease-CHD-]/blob/[main]/skewnessplot.jpg?raw=true)



