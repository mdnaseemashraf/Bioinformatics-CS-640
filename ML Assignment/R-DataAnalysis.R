# setwd("C:/Users/Naseem Ashraf/Desktop 1/Fall 16/Bioinformatics/ML Assignment")

library("foreign")
raw <- read.arff("disease2a.arff")
head(raw)
summary(raw)
dim(raw)

plot(raw$location1, ylab = 'Frequency', xlab='Factors of Location1')
plot(raw$location2, ylab = "Frequency", xlab="Factors of Location2")
plot(raw$location3, ylab = "Frequency", xlab="Factors of Location3")
plot(raw$location4, ylab = "Frequency", xlab="Factors of Location4")
plot(raw$location5, ylab = "Frequency", xlab="Factors of Location5")
plot(raw$location6, ylab = "Frequency", xlab="Factors of Location6")
plot(raw$location7, ylab = "Frequency", xlab="Factors of Location7")
plot(raw$location8, ylab = "Frequency", xlab="Factors of Location8")
plot(raw$location9, ylab = "Frequency", xlab="Factors of Location9")
plot(raw$location10, ylab = "Frequency", xlab="Factors of Location10")
plot(raw$class, ylab = "Frequency", xlab="Factors of Classification")

##Dividing Test and Training Data Sets into 1/3 and 2/3 fo the Data Set
train_size <- floor((2/3) * nrow(raw))
set.seed(123)
train_ind <- sample(seq_len(nrow(raw)), size = train_size)
train <- raw[train_ind, ]
test <- raw[-train_ind, ]
test_ind <- !(1:nrow(raw) %in% train_ind)

## Modelling for all main systems
glm.fit=glm(class~., data=train, family=binomial)
summary(glm.fit)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5450  -1.1831   0.9139   1.0894   1.5874  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.87702    0.23878  -3.673 0.000240 ***
# location11  -0.20856    0.11901  -1.752 0.079688 .  
# location12  -0.15719    0.19844  -0.792 0.428290    
# location21   0.08566    0.11830   0.724 0.468986    
# location22   0.31936    0.25910   1.233 0.217734    
# location31   0.10230    0.12171   0.841 0.400610    
# location32   0.12435    0.17158   0.725 0.468619    
# location41   0.04738    0.12450   0.381 0.703561    
# location42   0.02220    0.30425   0.073 0.941831    
# location51   0.65077    0.15874   4.099 4.14e-05 ***
# location52   0.74447    0.18741   3.972 7.12e-05 ***
# location61  -0.00711    0.11967  -0.059 0.952622    
# location62   0.12777    0.18416   0.694 0.487803    
# location71   0.05025    0.12002   0.419 0.675440    
# location72   0.02282    0.22677   0.101 0.919856    
# location81   0.09711    0.12895   0.753 0.451412    
# location82  -0.49160    0.37346  -1.316 0.188061    
# location91  -0.13174    0.11923  -1.105 0.269195    
# location92  -0.02094    0.18857  -0.111 0.911581    
# location101  0.52823    0.15619   3.382 0.000719 ***
# location102  0.29815    0.19121   1.559 0.118928    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1847.1  on 1332  degrees of freedom
# Residual deviance: 1798.8  on 1312  degrees of freedom
# AIC: 1840.8 ##All Main Effects + Intercept
# 
# Number of Fisher Scoring iterations: 4

##NOTE: Statistical significance of Intercept, location11, location51, location52 and location101 used later.

pred_test=predict(glm.fit, test, type="response")

thresh <- 0.5 # threshold for categorizing predicted probabilities
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy

# glm.pred   0   1
#        0 136 124
#        1 164 243
#
## Accuracy = 56.82159 % Accuracy with a 0.5 threshold

thresh <- 1
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy

# glm.pred   0   1
#        0 300 367
#
## Accuracy = 44.97751 % Accuracy with a 0.5 threshold

##Modelling with without the intercept
glm.fitno=glm(class~.-1, data=train, family=binomial)
summary(glm.fitno)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5450  -1.1831   0.9139   1.0894   1.5874  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# location10  -0.87702    0.23878  -3.673 0.000240 ***
# location11  -1.08558    0.24710  -4.393 1.12e-05 ***
# location12  -1.03420    0.29408  -3.517 0.000437 ***
# location21   0.08566    0.11830   0.724 0.468986    
# location22   0.31936    0.25910   1.233 0.217734    
# location31   0.10230    0.12171   0.841 0.400610    
# location32   0.12435    0.17158   0.725 0.468619    
# location41   0.04738    0.12450   0.381 0.703561    
# location42   0.02220    0.30425   0.073 0.941831    
# location51   0.65077    0.15874   4.099 4.14e-05 ***
# location52   0.74447    0.18741   3.972 7.12e-05 ***
# location61  -0.00711    0.11967  -0.059 0.952622    
# location62   0.12777    0.18416   0.694 0.487803    
# location71   0.05025    0.12002   0.419 0.675440    
# location72   0.02282    0.22677   0.101 0.919856    
# location81   0.09711    0.12895   0.753 0.451412    
# location82  -0.49160    0.37346  -1.316 0.188061    
# location91  -0.13174    0.11923  -1.105 0.269195    
# location92  -0.02094    0.18857  -0.111 0.911581    
# location101  0.52823    0.15619   3.382 0.000719 ***
# location102  0.29815    0.19121   1.559 0.118928    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1847.9  on 1333  degrees of freedom
# Residual deviance: 1798.8  on 1312  degrees of freedom
# AIC: 1840.8 ##All Main Effects - Intercept
# 
# Number of Fisher Scoring iterations: 4
##NOTE: Statistical significance of location10, location11, location12, location51, location52 and location101

pred_test=predict(glm.fitno, test, type="response")

thresh <- 0.5 # threshold for categorizing predicted probabilities
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy

# glm.pred   0   1
#        0 136 124
#        1 164 243
#
## Accuracy = 56.82159 % Accuracy with a 0.5 threshold

thresh <- 1
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy

# glm.pred   0   1
#        0 300 367
#
## Accuracy = 44.97751 % Accuracy with a 0.5 threshold

##Modelling with without the intercept

##Modelling with statistically significant predictors without the intercept
glm.fit1=glm(class~location1+location5+location10-1, data=train, family=binomial)
summary(glm.fit1)

# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -1.395  -1.171   1.010   1.091   1.459  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# location10   -0.7521     0.1894  -3.971 7.15e-05 ***
#   location11   -0.9526     0.1997  -4.770 1.84e-06 ***
#   location12   -0.9092     0.2549  -3.567 0.000361 ***
#   location51    0.6254     0.1574   3.973 7.10e-05 ***
#   location52    0.7153     0.1855   3.855 0.000116 ***
#   location101   0.5344     0.1549   3.449 0.000562 ***
#   location102   0.3120     0.1894   1.647 0.099540 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1847.9  on 1333  degrees of freedom
# Residual deviance: 1806.3  on 1326  degrees of freedom
# AIC: 1820.3 ##Statistically dignificant predictors without Intercept
# 
# Number of Fisher Scoring iterations: 4

##Best fit by lowest AIC.

pred_test=predict(glm.fit1, test, type="response")

thresh <- 0.5 # threshold for categorizing predicted probabilities
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy
## glm.pred   0   1
##        0 160 131
##        1 168 208
#
## 55.17241 % Accuracy with a 0.5 threshold

## Modelling all two way interactions without the intercept
glm.fitinteractions=glm(class~ .*.-1, data=train, family=binomial)
summary(glm.fitinteractions)
## Summary skipping some insignificant and verbose interactions
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7121   0.0000   0.0000   0.6255   1.8824  
# 
# Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)   
# location10             -1.983e+02  1.213e+04  -0.016  0.98696   
# location11             -1.814e+02  1.172e+04  -0.015  0.98765
# ...
# location11:location21  -5.120e-01  4.105e-01  -1.247  0.21230   
# location12:location21  -1.836e+00  8.126e-01  -2.260  0.02383 * 
# location11:location22   2.495e+00  2.607e+00   0.957  0.33847   
# ...
# location11:location32  -5.243e-01  6.580e-01  -0.797  0.42562   
# location12:location32  -2.767e+00  1.067e+00  -2.593  0.00953 **
# location11:location41  -7.767e-01  4.350e-01  -1.785  0.07420 . 
# location12:location41  -1.482e+00  9.665e-01  -1.533  0.12520   
# ...
# location11:location81   2.382e-01  4.883e-01   0.488  0.62568   
# location12:location81  -1.757e+00  8.839e-01  -1.987  0.04687 * 
# location11:location82  -1.228e+01  3.902e+04   0.000  0.99975   
# location12:location82  -4.571e+01  4.745e+07   0.000  1.00000   
# location11:location91   5.615e-01  4.027e-01   1.394  0.16318   
# location12:location91  -1.526e+00  8.724e-01  -1.750  0.08017 . 
# location11:location92   7.624e-01  7.424e-01   1.027  0.30441   
# ...
# location22:location52  -5.252e+01  2.787e+05   0.000  0.99985   
# location21:location61  -9.037e-01  4.143e-01  -2.181  0.02916 * 
# location22:location61  -3.009e+00  1.358e+00  -2.216  0.02669 * 
# location21:location62  -6.420e-01  8.054e-01  -0.797  0.42538   
# ...
# location22:location71   1.159e+00  1.414e+00   0.820  0.41222   
# location21:location72   1.762e+00  1.014e+00   1.738  0.08222 . 
# location22:location72  -3.190e+01  5.716e+04  -0.001  0.99955   
# location21:location81   1.420e-01  4.847e-01   0.293  0.76962   
# location22:location81  -5.570e+00  2.528e+00  -2.204  0.02756 * 
# location21:location82   3.024e+01  7.768e+04   0.000  0.99969   
# ...
# location22:location92   2.159e+00  2.006e+00   1.077  0.28159   
# location21:location101  2.288e+00  9.366e-01   2.442  0.01459 * 
# location22:location101 -2.393e+01  7.322e+04   0.000  0.99974   
# ...
# location32:location62   9.591e-01  1.477e+00   0.649  0.51618   
# location31:location71  -7.721e-01  4.241e-01  -1.820  0.06870 . 
# location32:location71  -8.671e-01  6.292e-01  -1.378  0.16818   
# ...
# location31:location81   7.293e-01  4.787e-01   1.524  0.12762   
# location32:location81   1.481e+00  8.273e-01   1.790  0.07342 . 
# location31:location82  -1.093e+01  3.755e+04   0.000  0.99977   
# location32:location82   4.183e+02  1.366e+05   0.003  0.99756   
# ...
# location42:location72   2.477e+02  7.763e+04   0.003  0.99745   
# location41:location81   9.516e-01  5.443e-01   1.748  0.08043 . 
# location42:location81   2.058e+02  1.315e+04   0.016  0.98751   
# ...
# location52:location102 -3.463e+02  1.726e+04  -0.020  0.98400   
# location61:location71   8.349e-01  4.206e-01   1.985  0.04713 * 
# location62:location71  -5.198e-02  8.548e-01  -0.061  0.95152   
# location61:location72  -2.628e+00  1.309e+00  -2.008  0.04461 * 
# location62:location72  -4.434e+00  1.436e+00  -3.087  0.00202 **
# location61:location81   5.191e-01  4.895e-01   1.060  0.28901   
# ...
# location71:location81   2.158e-01  5.046e-01   0.428  0.66896   
# location72:location81  -2.022e+00  1.105e+00  -1.830  0.06729 . 
# location71:location82  -1.534e+01  1.010e+05   0.000  0.99988   
# ...
# location91:location102 -6.085e+01  1.751e+04  -0.003  0.99723   
# location92:location102 -1.877e+02  1.994e+04  -0.009  0.99249   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1847.9  on 1333  degrees of freedom
# Residual deviance:  855.5  on 1132  degrees of freedom
# AIC: 1257.5 ##All two way interactions without the Intercept
# 
# Number of Fisher Scoring iterations: 23

pred_test=predict(glm.fitinteractions, test, type="response")

thresh <- 0.5 # threshold for categorizing predicted probabilities
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy

# glm.pred   0   1
#        0 201  58
#        1  99 309

## 76.46177 % Accuracy with a 0.5 threshold

##Best of Both
glm.fitbest=glm(class~ location1+location5+location10+.*.-1, data=train, family=binomial)
summary(glm.fitbest)

pred_test=predict(glm.fitinteractions, test, type="response")

thresh <- 0.5 # threshold for categorizing predicted probabilities
glm.pred=ifelse(pred_test >thresh,1,0)
confusion_matrix <- table(glm.pred, test$class)
confusion_matrix
accuracy <- mean(glm.pred==test$class)*100
accuracy