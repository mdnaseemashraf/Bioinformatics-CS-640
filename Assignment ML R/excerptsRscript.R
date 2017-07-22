library (foreign)
library(ggplot2)

disease2 <-read.arff ( file="disease2a.arff")
summary(disease2)

# To see how R deals with the categorical variables use contrasts() 
#  shows how the variables have been dummyfied, how to interpret them
contrasts(disease2$location5)
contrasts(disease2$location10)
#   1 2
# 0 0 0
# 1 1 0
# 2 0 1


# Written by Andy Field
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}


# get table of predicted probabilities of having the disease, add to
#  data frame of genes that are significant to the disease and the class:
preddisease2<- predict(fitdisease2,type="response")
disease22<-data.frame(disease2$location5,disease2$location10,disease2$class,preddisease2)
g<- ggplot(data = disease22)
g + geom_jitter(aes(x=disease2$class, y = preddisease2))
g + geom_jitter(aes(x=disease2$class, y = preddisease2))
g + geom_point(aes(x=disease2$class, y = preddisease2))
plot(disease22)



#redo using ordered factor varibles for 2 of the gene loci
disease2b <- data.frame(disease2[c("location5", "location10","class")])
disease2b$location5 <- as.ordered(disease2b$location5)
disease2b$location10 <- as.ordered(disease2b$location10)
# to convert from numeric location5 (but may not need specify levels): 
# factor(disease2b$location5,levels=c(0,1,2),ordered=TRUE)


# To see how R deals with the categorical variables use contrasts() 
#  shows how the variables have been dummyfied, how to interpret them
contrasts(disease2b$location5)
contrasts(disease2b$location10)
#                 .L         .Q
# [1,] -7.071068e-01  0.4082483
# [2,] -7.850462e-17 -0.8164966
# [3,]  7.071068e-01  0.4082483


table (disease2b)
table (disease2$location5, disease2$location10, disease2$class)


###########################################################################################
# comparison logistic regression with factor vs ordered factor
###########################################################################################


###########################################################################################
# interpretation using disease2 (categorical) fits:  disease22
###########################################################################################

# If p(disease) = p(no disease), then odds(disease) = 1 (or 1 to 1, or 1:1).
# If p(disease) < p(no disease)), then odds(disease) < 1.
# If p(disease) > p(no disease)), then odds(disease) > 1.
# Unlike probability, which cannot exceed 1, there is no upper bound on odds.

# the natural log of odds is called the logit (aka "log odds.),
# logit(p) = ln(p/q) 
# If odds(disease) = 1, then logit(p) = 0. 
# If odds(disease) < 1, then logit(p) < 0.
# If odds(disease) > 1, then logit(p) > 0.
# The logit transform fails if p = 0.
# if  coef in binomial regression is zero, 
# ln(p/q) = intercept + coef x
# then x is associated with even odds of having disease

#Do anova tests for significance:
anova(fitdisease22, test="Chisq")
anova(fitdisease2, test="Chisq")

# Interpret coefficients of fitdisease22 and fitdisease23:
# These are the regression coefficients for each predictor in the model, with
# the base level of each factor (ex: loc5=0)  being suppressed in model with intercept. 
# We are predicting log odds, so we antilog the coefficients. 
summary(fitdisease22) 
# if continuous predictor (like age):
#"for every one year increase in age the odds of having the disease increase by exp(coef) times."
#   (when all other variables in the model are held constant)

# if categorical dummy variable predictor (like location5 ==1)
#   then we get the pairwise diff between the levels:
# If location5 ==1 then the odds of having the disease 
# are coef times the odds when location5 is the reference"
# (when all other variables in the model are held constant)

exp(-1.47331) # 0.2291657 this is the intercept for fitdisease22
exp(2.76381) # 15.86016 this is location52:location100 
exp(fitdisease22$coef)
summary(fitdisease2b2)
exp(fitdisease23$coef)

# **********************************************************************
# The odds of having disease for location52:location100 
# is 15.86 times the odds having disease of location52:location102
# ln(odds) increase by a constant amount (2.76381),
#    odds increase by a constant multiplicative factor (15.86).
# **********************************************************************

# coef of location52:location100 is:
# location52:location102(ie:intercept) + location52:location100

# p val signifies if each level's mean is significantly different
#   from the reference level's mean.

###########################################################################################
# interpretation
###########################################################################################


