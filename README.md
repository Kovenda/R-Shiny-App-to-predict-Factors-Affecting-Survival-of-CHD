
# Survival of Cardiovascular Heart Disease-CHD
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

## Data Cleaning
> *Removing Null Values*
```{r}
na.omit(survival)
```
