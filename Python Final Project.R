library(readxl)
library(corrplot)
library(caret)
library(DMwR)
library(glmnet)
library(missForest)
library(Hmisc)

dat1 <- read.csv("Desktop/Covidclinicaldata/0407.csv")
dat2 <- read.csv("Desktop/Covidclinicaldata/0414.csv")
dat3 <- read.csv("Desktop/Covidclinicaldata/0421.csv")
#dat4 <- read_excel("Desktop/Covidclinicaldata/COVID-19 Clinical Data Repository.xlsx", sheet = 1)

# combine all three datasets
clinical <- rbind(dat1,dat2,dat3)
dim(clinical) #  1611   45

table(clinical$covid19_test_results)  ## response variable has three factor levels
clinical$clinical.result <- ifelse(clinical$covid19_test_results == "Negative", 0, 1) 
table(clinical$clinical.result) #  1509 negative and 101 positive

# Delete one "Other" observation
other = (clinical$covid19_test_results=="Other")
clinical <- clinical[!other,]
clinical <- clinical[,-5]

dim(clinical) #  1610   45
# str(clinical)

sum(is.na(clinical))
# 16175 missing values in total

# response variable "clinical.result"
ggplot(clinical, aes(factor(clinical.result))) +
  geom_bar(fill = c("purple","red"))
prop.table(table(clinical$clinical.result))
# Here, only 6.273292% are positive
# This is the unbalanced data - need undersampling and oversampling

### Data preliminary investigation for some numeric variables without handing missing values and unbalanced data
# 1. temperature - 77 missing values
hist(clinical$temperature, breaks=20, col="red")
ggplot(data=clinical[!is.na(clinical$temperature),], aes(x=factor(clinical.result),y=temperature, fill=factor(clinical.result))) + 
  geom_boxplot() 

#ggplot(data=clinical[!is.na(clinical$temperature),], 
# aes(x=clinical.result, y=temperature, color=clinical.result)) + geom_point()

tapply(clinical$temperature, clinical$clinical.result, summary)

# 2. age - no missing values
hist(clinical$age, breaks=20, col="red")
ggplot(data=clinical[!is.na(clinical$age),], aes(x=age, color=factor(clinical.result))) + 
  geom_bar()

tapply(clinical$age, clinical$clinical.result, summary)


# 3. days_since_symptom_onset - 799 missing values (49.63 %)
hist(clinical$days_since_symptom_onset, breaks=20, col="red")
ggplot(data=clinical[!is.na(clinical$days_since_symptom_onset),], aes(x=factor(clinical.result), y=days_since_symptom_onset, color=clinical.result)) + 
  geom_boxplot(color=) 

tapply(clinical$days_since_symptom_onset, clinical$clinical.result, summary)

# 4. pulse (bpm) - 99 missing values
hist(clinical$pulse, breaks=20, col="red")
ggplot(data=clinical[!is.na(clinical$pulse),], aes(x=clinical.result, y=pulse, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical$pulse, clinical$clinical.result, summary)


# 5. sys (Systolic blood pressure) - 97 missing values
hist(clinical$sys, breaks=20, col="red")
ggplot(data=clinical[!is.na(clinical$sys),], aes(x=clinical.result, y=sys, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical$sys, clinical$clinical.result, summary)


# 6. dia (Diastolic blood pressure) - 97 missing values
hist(clinical$dia, breaks=20, col="red")
ggplot(data=clinical[!is.na(clinical$dia),], aes(x=clinical.result, y=dia, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical$dia, clinical$clinical.result, summary)

# 7. rr (Respiratory rate) - 338 missing values
hist(clinical$rr, breaks=10, col="red")

ggplot(data=clinical[!is.na(clinical$rr),], aes(x=clinical.result, y=rr, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical$rr, clinical$clinical.result, summary)

# 8. sats (Oxygen saturation) - 117 missing vlaues
hist(clinical$sats, breaks=10, col="red")

ggplot(data=clinical[!is.na(clinical$sats),], aes(x=clinical.result, y=sats, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical$sats, clinical$clinical.result, summary)


## Correlation
numericVars <- which(sapply(clinical, is.numeric)) # index of numeric variables
numericVarNames <- names(numericVars) # 9 numeric valriables
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- clinical[, numericVars]  # the subset only have numeric variables
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") # correlations of all numeric variables
#sort on decreasing correlations with clinical.result
cor_sorted <- as.matrix(sort(cor_numVar[,'clinical.result'], decreasing = TRUE))
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# From the above correaltion plot, temperature and sats (Oxygen saturation) have a relationship (but quite low) with 
# Covid diseases. dia and sys (both refer to blood pressure) have a strong relationship among them either.
# The above results do not show significant difference between "negative" and "positive" cases. We need to impute 
# missing values and slove unbalanced problem, then we investigate again.

##########################
### Relevel variables ####
##########################
clinical$high_risk_exposure_occupation  <- ifelse(clinical$high_risk_exposure_occupation == "TRUE", 1, 0)
clinical$high_risk_interactions  <- ifelse(clinical$high_risk_interactions == "TRUE", 1, 0)
clinical$diabetes <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$chd <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$htn <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$cancer <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$asthma <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$copd <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$autoimmune_dis <- ifelse(clinical$diabetes == "TRUE", 1, 0)
clinical$ctab <- ifelse(clinical$ctab == "TRUE", 1, 0)
clinical$labored_respiration <- ifelse(clinical$labored_respiration == "TRUE", 1, 0)
clinical$rhonchi <- ifelse(clinical$rhonchi == "TRUE", 1, 0)
clinical$wheezes <- ifelse(clinical$wheezes == "TRUE", 1, 0)
clinical$cough <- ifelse(clinical$cough == "TRUE", 1, 0)
clinical$fever <- ifelse(clinical$fever == "TRUE", 1, 0)
clinical$sob <- ifelse(clinical$sob == "TRUE", 1, 0)
clinical$diarrhea <- ifelse(clinical$diarrhea == "TRUE", 1, 0)
clinical$fatigue <- ifelse(clinical$fatigue == "TRUE", 1, 0)
clinical$headache <- ifelse(clinical$headache == "TRUE", 1, 0)
clinical$loss_of_smell <- ifelse(clinical$loss_of_smell == "TRUE", 1, 0)
clinical$loss_of_taste <- ifelse(clinical$loss_of_taste == "TRUE", 1, 0)
clinical$runny_nose <- ifelse(clinical$runny_nose == "TRUE", 1, 0)
clinical$muscle_sore <- ifelse(clinical$muscle_sore == "TRUE", 1, 0)
clinical$sore_throat <-ifelse(clinical$sore_throat == "TRUE", 1, 0)
clinical$er_referral <- ifelse(clinical$er_referral == "TRUE", 1, 0)

levels(clinical$rapid_flu_results)[1] <- "Unknown"
levels(clinical$rapid_strep_results)[1] <- "Unknown"
levels(clinical$cough_severity)[1] <- "Normal"
levels(clinical$sob_severity)[1] <- "Normal"



########################
#### Missing Values ####
########################
NAcol <- which(colSums(is.na(clinical)) > 0)
missing <- sort(colSums(sapply(clinical[NAcol], is.na)), decreasing = TRUE)

# delete 8 variables which over 70% are missing values
# loss_of_tast, loss_of_smell, diarrhea, runny_nose, muscle_sore, sore_throat, headache, fatigue
# also delete date_published, clinic_state, test_name, swab_typ, cxr_findings, cxr_impression, cxr_link
clinical1 <- clinical[,-c(1:4,28,31,33:43)]   # 1610 & 28

# 1. We delete fever since it is highly correlated to the temperature
# 2. We delete cough since we will use cough_severity
# 3. We delete sob since we will use sob_severity

# For the rest of missing values, we use Hmisc to handle missing values
#seed missing values (10%)
clinical1.mis <- prodNA(clinical1, noNA = 0.1)
summary(clinical1.mis)

impute_arg <- aregImpute(~fever+high_risk_exposure_occupation+high_risk_interactions+days_since_symptom_onset+
                           rhonchi+rr+wheezes+ctab+sats+labored_respiration+pulse+sys+dia+temperature,data = clinical1.mis, n.impute = 5)
imputed <-impute.transcan(impute_arg, data=clinical1.mis, imputation=1, list.out=TRUE, pr=FALSE, check=FALSE)
imputed.data <- as.data.frame(do.call(cbind,imputed))


### combine to our orginal data
name <- colnames(imputed.data)
clinical2 <- clinical1[ , -which(names(clinical1) %in% name)] 
clinical3 <- cbind(clinical2,imputed.data)

sum(is.na(clinical3))   # now no missing values.

#str(clinical3)
clinical3$diabetes <- as.factor(clinical3$diabetes)
clinical3$chd <- as.factor(clinical3$chd)
clinical3$htn <- as.factor(clinical3$htn)
clinical3$cancer <- as.factor(clinical3$cancer )
clinical3$asthma <- as.factor(clinical3$asthma)
clinical3$copd <- as.factor(clinical3$copd)
clinical3$autoimmune_dis <- as.factor(clinical3$autoimmune_dis)
clinical3$er_referral <- as.factor(clinical3$er_referral)
clinical3$fever <- as.factor(clinical3$fever)
clinical3$high_risk_exposure_occupation <- as.factor(clinical3$high_risk_exposure_occupation)
clinical3$high_risk_interactions <- as.factor(clinical3$high_risk_interactions)
clinical3$rhonchi <- as.factor(clinical3$rhonchi)
clinical3$wheezes <- as.factor(clinical3$wheezes)
clinical3$ctab <- as.factor(clinical3$ctab)
clinical3$labored_respiration <- as.factor(clinical3$labored_respiration)
clinical3$clinical.result <- as.factor(clinical3$clinical.result)

str(clinical3)

## chd, htn, cancer, asthma, copd, autoimmune_dis, er_referral only have level, delete
clinical4 <- clinical3[,-c(3:8, 13)]
dim(clinical4)  # 21 columns
str(clinical4)


### Second data preliminary investigation for some numeric variables without handing missing values and unbalanced data
# 1. temperature
hist(clinical4$temperature, breaks=20, col="red")
ggplot(data=clinical4[!is.na(clinical4$temperature),], aes(x=clinical.result,y=temperature, fill=factor(clinical.result))) + 
  geom_boxplot() 

tapply(clinical3$temperature, clinical3$clinical.result, summary)

# 2. age
hist(clinical4$age, breaks=20, col="red")
ggplot(data=clinical4[!is.na(clinical4$age),], aes(x=age, color=clinical.result)) + 
  geom_bar()

tapply(clinical4$age, clinical4$clinical.result, summary)


# 3. days_since_symptom_onset
hist(clinical4$days_since_symptom_onset, breaks=20, col="red")
ggplot(data=clinical4[!is.na(clinical4$days_since_symptom_onset),], aes(x=clinical.result, y=days_since_symptom_onset, color=clinical.result)) + 
  geom_boxplot() 

tapply(clinical4$days_since_symptom_onset, clinical4$clinical.result, summary)

# 4. pulse (bpm) 
hist(clinical4$pulse, breaks=20, col="red")
ggplot(data=clinical4[!is.na(clinical4$pulse),], aes(x=clinical.result, y=pulse)) + 
  geom_boxplot()

tapply(clinical4$pulse, clinical4$clinical.result, summary)


# 5. sys (Systolic blood pressure) 
hist(clinical3$sys, breaks=20, col="red")
ggplot(data=clinical4[!is.na(clinical4$sys),], aes(x=factor(clinical.result), y=sys)) + 
  geom_boxplot()

tapply(clinical4$sys, clinical4$clinical.result, summary)


# 6. dia (Diastolic blood pressure) - 97 missing values
hist(clinical4$dia, breaks=20, col="red")
ggplot(data=clinical4[!is.na(clinical4$dia),], aes(x=clinical.result, y=dia, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical4$dia, clinical4$clinical.result, summary)

# 7. rr (Respiratory rate) - 338 missing values
hist(clinical4$rr, breaks=10, col="red")

ggplot(data=clinical4[!is.na(clinical4$rr),], aes(x=clinical.result, y=rr, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical4$rr, clinical4$clinical.result, summary)

# 8. sats (Oxygen saturation) - 117 missing vlaues
hist(clinical4$sats, breaks=10, col="red")

ggplot(data=clinical4[!is.na(clinical4$sats),], aes(x=clinical.result, y=sats, color=clinical.result)) + 
  geom_boxplot()

tapply(clinical4$sats, clinical4$clinical.result, summary)


## Correlation - may be useless
numericVars1 <- which(sapply(clinical4, is.numeric)) # index of numeric variables
numericVarNames1 <- names(numericVars1) # 9 numeric valriables
cat('There are', length(numericVars1), 'numeric variables')
all_numVar1 <- clinical4[, numericVars1]  # the subset only have numeric variables
cor_numVar1 <- cor(all_numVar1, use="pairwise.complete.obs") # correlations of all numeric variables
#sort on decreasing correlations with clinical.result
cor_sorted1 <- as.matrix(sort(cor_numVar1[,'clinical.result'], decreasing = TRUE))
corrplot.mixed(cor_numVar1, tl.col="black", tl.pos = "lt")


###################
## Data Spliting ##
####################

########################
##Use down-sampling#####
########################
# south code - Week 10
set.seed(9560)
down_train <- downSample(x = imbal_train[, -ncol(imbal_train)],
                         y = imbal_train$Class)
table(down_train$Class)

########################
### Our final model ####
########################
# website: https://shiring.github.io/machine_learning/2017/04/02/unbalanced
### We start with the undersampling and will try to classify using these Models:
# 1. Decision Tree Classifier/ Random Forest Classifier
# 2. Logistic regression
# 3. SVM
# 4. XGboost