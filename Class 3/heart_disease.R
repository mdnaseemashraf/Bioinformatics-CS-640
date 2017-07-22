# 1. #3 (age): age in years 
# 2. #4 (sex): sex (1 = male; 0 = female) 
# 3. #9 (cp): chest pain type   ***
# -- Value 1: typical angina 
# -- Value 2: atypical angina 
# -- Value 3: non-anginal pain 
# -- Value 4: asymptomatic 
# 4. #10 (trestbps): resting blood pressure (in mm Hg on admission to the hospital) 
# 5. #12 (chol): serum cholestoral in mg/dl 
# 6. #16 (fbs): (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false) *****
# 7. #19 (restecg): resting electrocardiographic results 
# -- Value 0: normal 
# -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) 
# -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria
# 
# 8. #32 (thalach): maximum heart rate achieved 
# 9. #38 (exang): exercise induced angina (1 = yes; 0 = no) 
#  Info on ST segment of ECG: http://lifeinthefastlane.com/ecg-library/st-segment/
# 10. #40 (oldpeak): = ST depression induced by exercise relative to rest (number indicates amount of depression)(?1 = yes; 0 = no?) 
# 11. #41 (slope): the slope of the peak exercise ST segment 
# -- Value 1: upsloping 
# -- Value 2: flat 
# -- Value 3: downsloping 
# 12. #44 (ca): number of major vessels (0-3) colored by flourosopy 
# 13. #51 (thal): 3 = normal; 6 = fixed defect; 7 = reversable defect 
# 14. #58 (num): diagnosis of heart disease (angiographic disease status) 
# -- Value 0: < 50% diameter narrowing 
# -- Value 1: > 50% diameter narrowing

setwd("C:/Users/Pat/Documents/R/heart_disease")
myData <- read.csv(file="reprocessed.hungarian.csv", header=TRUE, sep=",")
hist(myData$num)
hist(myData$slope, 20)
nrow(subset(myData, slope==2))
summary(myData)
myData$num  <- ifelse(myData$num>0, 1, 0)
myData$cp <- factor(myData$cp)
myData$restecg <- factor(myData$restecg)
lr = glm(myData$num ~ ., data = myData, family = "binomial")
summary(lr)


# Use regression to model the  serum cholesterol chol as a function off age, sex,  trestbps,fbs,restecg, and thalach
# Briefly discuss your results from #2