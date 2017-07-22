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

## Modelling for all main systems
glm.fit=glm(class~., data=raw, family=binomial)
summary(glm.fit)
par(mfrow=c(2,2))
plot(glm.fit,1)
plot(glm.fit,2)
plot(glm.fit,3)
plot(glm.fit,4)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5601  -1.1938   0.9208   1.0755   1.5054  
# 
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.690896   0.192695  -3.585 0.000337 ***
# location11  -0.248695   0.096346  -2.581 0.009844 ** 
# location12  -0.065724   0.168515  -0.390 0.696524    
# ...
# location42   0.009934   0.236499   0.042 0.966494    
# location51   0.485951   0.129270   3.759 0.000170 ***
# location52   0.585198   0.153317   3.817 0.000135 ***
# location61   0.022736   0.097615   0.233 0.815827    
# location62   0.198102   0.148934   1.330 0.183475    
# location71   0.033391   0.097068   0.344 0.730847    
# location72   0.071946   0.192655   0.373 0.708817    
# location81   0.077968   0.104523   0.746 0.455701    
# location82  -0.181098   0.292559  -0.619 0.535908    
# location91  -0.141754   0.097229  -1.458 0.144858    
# location92   0.070989   0.153832   0.461 0.644458    
# location101  0.624517   0.128318   4.867 1.13e-06 ***
# location102  0.336888   0.154433   2.181 0.029150 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2767.6  on 1999  degrees of freedom
# Residual deviance: 2700.9  on 1979  degrees of freedom
# AIC: 2742.9
# 
# Number of Fisher Scoring iterations: 4


##Modelling Main Effects without the intercept
glm.fitno=glm(class~.-1, data=raw, family=binomial)
summary(glm.fitno)

par(mfrow=c(2,2))
plot(glm.fitno,1)
plot(glm.fitno,2)
plot(glm.fitno,3)
plot(glm.fitno,4)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5601  -1.1938   0.9208   1.0755   1.5054  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# location10  -0.690896   0.192695  -3.585 0.000337 ***
#   location11  -0.939591   0.198204  -4.741 2.13e-06 ***
#   location12  -0.756620   0.242192  -3.124 0.001784 ** 
#   location21   0.039915   0.096852   0.412 0.680245    
# location22   0.196396   0.200122   0.981 0.326404    
# location31   0.011102   0.099288   0.112 0.910966    
# location32   0.018708   0.140100   0.134 0.893773    
# location41   0.039896   0.100743   0.396 0.692094    
# location42   0.009934   0.236499   0.042 0.966494    
# location51   0.485951   0.129270   3.759 0.000170 ***
#   location52   0.585198   0.153317   3.817 0.000135 ***
#   location61   0.022736   0.097615   0.233 0.815827    
# location62   0.198102   0.148934   1.330 0.183475    
# location71   0.033391   0.097068   0.344 0.730847    
# location72   0.071946   0.192655   0.373 0.708817    
# location81   0.077968   0.104523   0.746 0.455701    
# location82  -0.181098   0.292559  -0.619 0.535908    
# location91  -0.141754   0.097229  -1.458 0.144858    
# location92   0.070989   0.153832   0.461 0.644458    
# location101  0.624517   0.128318   4.867 1.13e-06 ***
#   location102  0.336888   0.154433   2.181 0.029150 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2772.6  on 2000  degrees of freedom
# Residual deviance: 2700.9  on 1979  degrees of freedom
# AIC: 2742.9
# 
# Number of Fisher Scoring iterations: 4

##Modelling Main Effects with statistically significant predictors without the intercept
glm.fit1=glm(class~location1+location5+location10-1, data=raw, family=binomial)
summary(glm.fit1)
exp(glm.fit1$coefficients)


par(mfrow=c(2,2))
plot(glm.fit1,1)
plot(glm.fit1,2)
plot(glm.fit1,3)
plot(glm.fit1,4)

# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -1.428  -1.177   0.983   1.079   1.406  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# location10   -0.6315     0.1546  -4.085 4.40e-05 ***
#   location11   -0.8715     0.1609  -5.416 6.08e-08 ***
#   location12   -0.7050     0.2140  -3.295 0.000985 ***
#   location51    0.4759     0.1281   3.714 0.000204 ***
#   location52    0.5713     0.1524   3.749 0.000178 ***
#   location101   0.6317     0.1277   4.945 7.60e-07 ***
#   location102   0.3478     0.1536   2.265 0.023537 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2772.6  on 2000  degrees of freedom
# Residual deviance: 2708.6  on 1993  degrees of freedom
# AIC: 2722.6
# 
# Number of Fisher Scoring iterations: 4

### Coefficients:
# location10  location11  location12  location51  location52 location101 location102 
# 0.5318079   0.4183030   0.4941323   1.6093885   1.7706004   1.8807271   1.4159356 


## Modelling all two way interactions without the intercept
glm.fitinteractions=glm(class~ .*.-1, data=raw, family=binomial)
summary(glm.fitinteractions)

par(mfrow=c(2,2))
plot(glm.fitinteractions,1)
plot(glm.fitinteractions,2)
plot(glm.fitinteractions,3)
plot(glm.fitinteractions,4)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6654   0.0000   0.1625   0.6724   2.4397  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# location10             -4.025e+01  2.905e+03  -0.014 0.988943    
# location11             -3.995e+01  2.905e+03  -0.014 0.989026    
# location12             -1.887e+01  2.634e+03  -0.007 0.994284    
# location21             -3.128e+00  1.478e+00  -2.116 0.034328 *  
# location22              1.136e+01  1.572e+03   0.007 0.994237    
# ...
# location62             -4.225e-01  2.140e+00  -0.197 0.843509    
# location71             -2.094e+00  1.684e+00  -1.244 0.213594    
# location72             -4.694e+00  2.518e+00  -1.864 0.062314 .  
# location81             -3.000e+00  1.588e+00  -1.889 0.058903 .  
# location82             -1.064e+02  1.037e+04  -0.010 0.991815    
# ...
# location102             4.109e+01  2.905e+03   0.014 0.988713    
# location11:location21  -5.842e-01  3.144e-01  -1.858 0.063121 .  
# location12:location21  -1.995e+00  6.499e-01  -3.070 0.002140 ** 
# location11:location22   2.991e-01  6.845e-01   0.437 0.662145    
# ...
# location11:location32  -4.744e-01  4.671e-01  -1.016 0.309855    
# location12:location32  -2.004e+00  8.465e-01  -2.367 0.017932 *  
# location11:location41  -5.887e-01  3.236e-01  -1.819 0.068836 .  
# location12:location41  -1.416e+00  7.335e-01  -1.931 0.053534 .  
# location11:location42  -3.322e+00  1.388e+00  -2.392 0.016738 *  
# location12:location42  -3.150e+00  1.687e+00  -1.867 0.061965 .  
# location11:location51  -2.506e-01  1.415e+00  -0.177 0.859395    
# ...
# location12:location71   5.678e-01  6.532e-01   0.869 0.384665    
# location11:location72  -1.874e+00  7.297e-01  -2.569 0.010213 *  
#   location12:location72  -7.173e-01  1.146e+00  -0.626 0.531288    
# location11:location81  -3.343e-01  3.478e-01  -0.961 0.336430    
# location12:location81  -2.041e+00  7.407e-01  -2.756 0.005858 ** 
# location11:location82  -1.459e+01  1.514e+03  -0.010 0.992310    
# location12:location82  -7.343e+00  4.745e+07   0.000 1.000000    
# location11:location91   2.345e-01  3.067e-01   0.765 0.444510    
# location12:location91  -1.263e+00  6.868e-01  -1.840 0.065828 .  
# location11:location92   3.150e-01  5.367e-01   0.587 0.557301    
# ...
# location22:location51  -1.434e+01  1.572e+03  -0.009 0.992721    
# location21:location52   2.593e+00  1.377e+00   1.883 0.059657 .  
# location22:location52  -1.248e+01  1.572e+03  -0.008 0.993665    
# location21:location61  -7.728e-01  3.170e-01  -2.438 0.014776 *  
# location22:location61  -1.569e+00  6.955e-01  -2.256 0.024087 *  
# location21:location62   6.303e-02  5.733e-01   0.110 0.912443    
# location22:location62  -1.858e+00  9.244e-01  -2.010 0.044453 *  
# location21:location71   1.434e-01  3.175e-01   0.452 0.651615    
# location22:location71  -5.971e-02  6.445e-01  -0.093 0.926185    
# location21:location72   1.254e+00  7.349e-01   1.706 0.088079 .  
# location22:location72  -3.065e+00  1.707e+00  -1.796 0.072550 .  
# location21:location81   1.930e-01  3.609e-01   0.535 0.592738    
# ...
# location21:location101  1.003e+00  6.734e-01   1.489 0.136471    
# location22:location101  3.716e+00  1.251e+00   2.971 0.002968 ** 
# location21:location102  3.290e+00  1.368e+00   2.405 0.016153 *  
# location22:location102 -1.019e+01  1.572e+03  -0.006 0.994828    
# ...
# location32:location62   5.716e-01  8.520e-01   0.671 0.502291    
# location31:location71  -5.581e-01  3.250e-01  -1.717 0.085902 .  
# location32:location71  -8.519e-01  4.664e-01  -1.826 0.067784 .  
# location31:location72   1.081e+00  7.283e-01   1.485 0.137556    
# ...
# location42:location62   1.896e+00  1.907e+00   0.995 0.319963    
# location41:location71   5.613e-01  3.359e-01   1.671 0.094713 .  
# location42:location71  -9.265e-01  1.045e+00  -0.886 0.375425    
# location41:location72   9.417e-01  7.763e-01   1.213 0.225099    
# location42:location72   1.536e+01  1.952e+04   0.001 0.999372    
# location41:location81   6.904e-01  3.948e-01   1.749 0.080330 .  
# location42:location81   2.460e+00  1.285e+00   1.915 0.055506 .  
# location41:location82  -1.299e+01  3.013e+03  -0.004 0.996561    
# location42:location82   1.767e+01  1.206e+04   0.001 0.998831    
# location41:location91  -4.806e-01  3.347e-01  -1.436 0.151054    
# location42:location91  -1.869e+00  1.106e+00  -1.691 0.090926 .  
# location41:location92  -8.334e-02  5.352e-01  -0.156 0.876270    
# ...
# location52:location71   1.102e+00  1.618e+00   0.681 0.495852    
# location51:location72   4.484e+00  2.712e+00   1.654 0.098219 .  
# location52:location72   4.656e+00  2.241e+00   2.078 0.037725 *  
#   location51:location81   3.052e+00  1.625e+00   1.878 0.060423 .  
# location52:location81   2.132e+00  1.486e+00   1.435 0.151310    
# ...
# location51:location102  4.636e+01  5.169e+03   0.009 0.992844    
# location52:location102 -4.896e+01  2.905e+03  -0.017 0.986553    
# location61:location71   6.896e-01  3.126e-01   2.206 0.027377 *  
#   location62:location71  -1.608e-01  5.687e-01  -0.283 0.777373    
# location61:location72  -1.478e+00  8.085e-01  -1.828 0.067521 .  
# location62:location72  -3.600e+00  1.071e+00  -3.363 0.000772 ***
#   location61:location81   4.636e-02  3.572e-01   0.130 0.896729    
# location62:location81  -4.655e-01  5.870e-01  -0.793 0.427847    
# location61:location82   1.783e+00  2.306e+03   0.001 0.999383    
# location62:location82   4.358e+01  4.238e+03   0.010 0.991795    
# location61:location91  -2.792e-01  3.117e-01  -0.896 0.370469    
# location62:location91  -1.444e+00  5.783e-01  -2.498 0.012502 *  
#   location61:location92  -3.512e-01  5.295e-01  -0.663 0.507080    
# ...
# location71:location81  -5.873e-02  3.618e-01  -0.162 0.871023    
# location72:location81  -1.299e+00  7.276e-01  -1.785 0.074267 .  
# location71:location82  -1.524e+01  2.113e+03  -0.007 0.994244    
# location72:location82  -1.349e+02  7.314e+03  -0.018 0.985284    
# location71:location91   5.768e-01  3.150e-01   1.831 0.067115 .  
# location72:location91   1.393e+00  8.114e-01   1.716 0.086103 .  
# location71:location92   1.128e+00  5.735e-01   1.967 0.049187 *  
#   location72:location92   1.261e+00  9.601e-01   1.314 0.188977    
# location71:location101  1.476e-01  6.856e-01   0.215 0.829545    
# location72:location101  1.557e+00  1.653e+00   0.942 0.346378    
# location71:location102  1.518e+00  1.606e+00   0.945 0.344635    
# location72:location102  3.900e+00  2.208e+00   1.766 0.077369 .  
# location81:location91   1.193e-01  3.570e-01   0.334 0.738161    
# ...
# location82:location101 -7.261e+01  2.608e+03  -0.028 0.977786    
# location81:location102  3.091e+00  1.462e+00   2.115 0.034463 *  
#   location82:location102 -1.557e+02  9.069e+03  -0.017 0.986301    
# location91:location101 -9.128e-01  6.822e-01  -1.338 0.180865    
# location92:location101 -1.017e+00  1.257e+00  -0.810 0.418207    
# location91:location102 -1.491e+00  1.178e+00  -1.266 0.205533    
# location92:location102 -1.927e+01  2.262e+03  -0.009 0.993203    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2772.6  on 2000  degrees of freedom
# Residual deviance: 1430.5  on 1799  degrees of freedom
# AIC: 1832.5
# 
# Number of Fisher Scoring iterations: 20

##Best of Both
glm.fitbest=glm(class~ location1+location5+location10+.*.-1, data=raw, family=binomial)
summary(glm.fitbest)


# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6654   0.0000   0.1625   0.6724   2.4397  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# location10             -4.025e+01  2.905e+03  -0.014 0.988943    
# location11             -3.995e+01  2.905e+03  -0.014 0.989026    
# location12             -1.887e+01  2.634e+03  -0.007 0.994284    
# location51             -9.377e+01  4.733e+03  -0.020 0.984195    
# location52              4.234e+01  2.905e+03   0.015 0.988370    
# location101            -1.100e+01  3.329e+03  -0.003 0.997363    
# location102             4.109e+01  2.905e+03   0.014 0.988713    
# location21             -3.128e+00  1.478e+00  -2.116 0.034328 *  
#   location22              1.136e+01  1.572e+03   0.007 0.994237    
# ...
# location62             -4.225e-01  2.140e+00  -0.197 0.843509    
# location71             -2.094e+00  1.684e+00  -1.244 0.213594    
# location72             -4.694e+00  2.518e+00  -1.864 0.062314 .  
# location81             -3.000e+00  1.588e+00  -1.889 0.058903 .  
# location82             -1.064e+02  1.037e+04  -0.010 0.991815    
# location91              9.332e-01  1.297e+00   0.719 0.471849    
# location92              2.110e+01  2.262e+03   0.009 0.992560    
# location11:location21  -5.842e-01  3.144e-01  -1.858 0.063121 .  
# location12:location21  -1.995e+00  6.499e-01  -3.070 0.002140 ** 
#   location11:location22   2.991e-01  6.845e-01   0.437 0.662145    
# ...
# location12:location31  -1.981e-01  6.378e-01  -0.311 0.756049    
# location11:location32  -4.744e-01  4.671e-01  -1.016 0.309855    
# location12:location32  -2.004e+00  8.465e-01  -2.367 0.017932 *  
#   location11:location41  -5.887e-01  3.236e-01  -1.819 0.068836 .  
# location12:location41  -1.416e+00  7.335e-01  -1.931 0.053534 .  
# location11:location42  -3.322e+00  1.388e+00  -2.392 0.016738 *  
#   location12:location42  -3.150e+00  1.687e+00  -1.867 0.061965 .  
# location11:location51  -2.506e-01  1.415e+00  -0.177 0.859395    
# ...
# location12:location71   5.678e-01  6.532e-01   0.869 0.384665    
# location11:location72  -1.874e+00  7.297e-01  -2.569 0.010213 *  
#   location12:location72  -7.173e-01  1.146e+00  -0.626 0.531288    
# location11:location81  -3.343e-01  3.478e-01  -0.961 0.336430    
# location12:location81  -2.041e+00  7.407e-01  -2.756 0.005858 ** 
#   location11:location82  -1.459e+01  1.514e+03  -0.010 0.992310    
# location12:location82  -7.343e+00  4.745e+07   0.000 1.000000    
# location11:location91   2.345e-01  3.067e-01   0.765 0.444510    
# location12:location91  -1.263e+00  6.868e-01  -1.840 0.065828 .  
# location11:location92   3.150e-01  5.367e-01   0.587 0.557301    
# ...
# location51:location21   2.211e+00  1.485e+00   1.489 0.136478    
# location52:location21   2.593e+00  1.377e+00   1.883 0.059657 .  
# location51:location22  -1.434e+01  1.572e+03  -0.009 0.992721    
# location52:location22  -1.248e+01  1.572e+03  -0.008 0.993665    
# location21:location61  -7.728e-01  3.170e-01  -2.438 0.014776 *  
#   location22:location61  -1.569e+00  6.955e-01  -2.256 0.024087 *  
#   location21:location62   6.303e-02  5.733e-01   0.110 0.912443    
# location22:location62  -1.858e+00  9.244e-01  -2.010 0.044453 *  
#   location21:location71   1.434e-01  3.175e-01   0.452 0.651615    
# location22:location71  -5.971e-02  6.445e-01  -0.093 0.926185    
# location21:location72   1.254e+00  7.349e-01   1.706 0.088079 .  
# location22:location72  -3.065e+00  1.707e+00  -1.796 0.072550 .  
# location21:location81   1.930e-01  3.609e-01   0.535 0.592738    
# ...
# location22:location92   1.334e+00  1.105e+00   1.207 0.227423    
# location101:location21  1.003e+00  6.734e-01   1.489 0.136471    
# location102:location21  3.290e+00  1.368e+00   2.405 0.016153 *  
#   location101:location22  3.716e+00  1.251e+00   2.971 0.002968 ** 
#   location102:location22 -1.019e+01  1.572e+03  -0.006 0.994828    
# ...
# location32:location62   5.716e-01  8.520e-01   0.671 0.502291    
# location31:location71  -5.581e-01  3.250e-01  -1.717 0.085902 .  
# location32:location71  -8.519e-01  4.664e-01  -1.826 0.067784 .  
# location31:location72   1.081e+00  7.283e-01   1.485 0.137556    
# ...
# location42:location62   1.896e+00  1.907e+00   0.995 0.319963    
# location41:location71   5.613e-01  3.359e-01   1.671 0.094713 .  
# location42:location71  -9.265e-01  1.045e+00  -0.886 0.375425    
# location41:location72   9.417e-01  7.763e-01   1.213 0.225099    
# location42:location72   1.536e+01  1.952e+04   0.001 0.999372    
# location41:location81   6.904e-01  3.948e-01   1.749 0.080330 .  
# location42:location81   2.460e+00  1.285e+00   1.915 0.055506 .  
# location41:location82  -1.299e+01  3.013e+03  -0.004 0.996561    
# location42:location82   1.767e+01  1.206e+04   0.001 0.998831    
# location41:location91  -4.806e-01  3.347e-01  -1.436 0.151054    
# location42:location91  -1.869e+00  1.106e+00  -1.691 0.090926 .  
# location41:location92  -8.334e-02  5.352e-01  -0.156 0.876270    
# ...
# location51:location72   4.484e+00  2.712e+00   1.654 0.098219 .  
# location52:location72   4.656e+00  2.241e+00   2.078 0.037725 *  
#   location51:location81   3.052e+00  1.625e+00   1.878 0.060423 .  
# location52:location81   2.132e+00  1.486e+00   1.435 0.151310    
# ...
# location52:location102 -4.896e+01  2.905e+03  -0.017 0.986553    
# location61:location71   6.896e-01  3.126e-01   2.206 0.027377 *  
#   location62:location71  -1.608e-01  5.687e-01  -0.283 0.777373    
# location61:location72  -1.478e+00  8.085e-01  -1.828 0.067521 .  
# location62:location72  -3.600e+00  1.071e+00  -3.363 0.000772 ***
#   location61:location81   4.636e-02  3.572e-01   0.130 0.896729    
# ...
# location61:location91  -2.792e-01  3.117e-01  -0.896 0.370469    
# location62:location91  -1.444e+00  5.783e-01  -2.498 0.012502 *  
#   location61:location92  -3.512e-01  5.295e-01  -0.663 0.507080    
# ...
# location71:location81  -5.873e-02  3.618e-01  -0.162 0.871023    
# location72:location81  -1.299e+00  7.276e-01  -1.785 0.074267 .  
# location71:location82  -1.524e+01  2.113e+03  -0.007 0.994244    
# location72:location82  -1.349e+02  7.314e+03  -0.018 0.985284    
# location71:location91   5.768e-01  3.150e-01   1.831 0.067115 .  
# location72:location91   1.393e+00  8.114e-01   1.716 0.086103 .  
# location71:location92   1.128e+00  5.735e-01   1.967 0.049187 *  
#   location72:location92   1.261e+00  9.601e-01   1.314 0.188977    
# ...
# location101:location72  1.557e+00  1.653e+00   0.942 0.346378    
# location102:location72  3.900e+00  2.208e+00   1.766 0.077369 .  
# location81:location91   1.193e-01  3.570e-01   0.334 0.738161    
# ...
# location102:location81  3.091e+00  1.462e+00   2.115 0.034463 *  
#   location101:location82 -7.261e+01  2.608e+03  -0.028 0.977786    
# location102:location82 -1.557e+02  9.069e+03  -0.017 0.986301    
# ...
# location102:location92 -1.927e+01  2.262e+03  -0.009 0.993203    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2772.6  on 2000  degrees of freedom
# Residual deviance: 1430.5  on 1799  degrees of freedom
# AIC: 1832.5
# 
# Number of Fisher Scoring iterations: 20

##Full Model Analysis
glm.fitfull=glm(class~ .+.*.-1, data=raw, family=binomial)
summary(glm.fitfull)

par(mfrow=c(2,2))
plot(glm.fitfull,1)
plot(glm.fitfull,2)
plot(glm.fitfull,3)
plot(glm.fitfull,4)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6654   0.0000   0.1625   0.6724   2.4397  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# location10             -4.025e+01  2.905e+03  -0.014 0.988943    
# location11             -3.995e+01  2.905e+03  -0.014 0.989026    
# location12             -1.887e+01  2.634e+03  -0.007 0.994284    
# location21             -3.128e+00  1.478e+00  -2.116 0.034328 *  
# location22              1.136e+01  1.572e+03   0.007 0.994237    
# ...
# location71             -2.094e+00  1.684e+00  -1.244 0.213594    
# location72             -4.694e+00  2.518e+00  -1.864 0.062314 .  
# location81             -3.000e+00  1.588e+00  -1.889 0.058903 .  
# location82             -1.064e+02  1.037e+04  -0.010 0.991815    
# ...
# location11:location21  -5.842e-01  3.144e-01  -1.858 0.063121 .  
# location12:location21  -1.995e+00  6.499e-01  -3.070 0.002140 ** 
# location11:location22   2.991e-01  6.845e-01   0.437 0.662145    
# ...
# location11:location32  -4.744e-01  4.671e-01  -1.016 0.309855    
# location12:location32  -2.004e+00  8.465e-01  -2.367 0.017932 *  
# location11:location41  -5.887e-01  3.236e-01  -1.819 0.068836 .  
# location12:location41  -1.416e+00  7.335e-01  -1.931 0.053534 .  
# location11:location42  -3.322e+00  1.388e+00  -2.392 0.016738 *  
# location12:location42  -3.150e+00  1.687e+00  -1.867 0.061965 .  
# location11:location51  -2.506e-01  1.415e+00  -0.177 0.859395    
# ...
# location12:location71   5.678e-01  6.532e-01   0.869 0.384665    
# location11:location72  -1.874e+00  7.297e-01  -2.569 0.010213 *  
# location12:location72  -7.173e-01  1.146e+00  -0.626 0.531288    
# location11:location81  -3.343e-01  3.478e-01  -0.961 0.336430    
# location12:location81  -2.041e+00  7.407e-01  -2.756 0.005858 ** 
# location11:location82  -1.459e+01  1.514e+03  -0.010 0.992310    
# location12:location82  -7.343e+00  4.745e+07   0.000 1.000000    
# location11:location91   2.345e-01  3.067e-01   0.765 0.444510    
# location12:location91  -1.263e+00  6.868e-01  -1.840 0.065828 .  
# location11:location92   3.150e-01  5.367e-01   0.587 0.557301    
# ...
# location21:location52   2.593e+00  1.377e+00   1.883 0.059657 .  
# location22:location52  -1.248e+01  1.572e+03  -0.008 0.993665    
# location21:location61  -7.728e-01  3.170e-01  -2.438 0.014776 *  
# location22:location61  -1.569e+00  6.955e-01  -2.256 0.024087 *  
# location21:location62   6.303e-02  5.733e-01   0.110 0.912443    
# location22:location62  -1.858e+00  9.244e-01  -2.010 0.044453 *  
# location21:location71   1.434e-01  3.175e-01   0.452 0.651615    
# location22:location71  -5.971e-02  6.445e-01  -0.093 0.926185    
# location21:location72   1.254e+00  7.349e-01   1.706 0.088079 .  
# location22:location72  -3.065e+00  1.707e+00  -1.796 0.072550 .  
# location21:location81   1.930e-01  3.609e-01   0.535 0.592738    
# location22:location81   5.850e-01  9.212e-01   0.635 0.525432    
# ...
# location21:location101  1.003e+00  6.734e-01   1.489 0.136471    
# location22:location101  3.716e+00  1.251e+00   2.971 0.002968 ** 
# location21:location102  3.290e+00  1.368e+00   2.405 0.016153 *  
# location22:location102 -1.019e+01  1.572e+03  -0.006 0.994828    
# location31:location41   8.609e-02  3.467e-01   0.248 0.803910    
# ...
# location31:location71  -5.581e-01  3.250e-01  -1.717 0.085902 .  
# location32:location71  -8.519e-01  4.664e-01  -1.826 0.067784 .  
# location31:location72   1.081e+00  7.283e-01   1.485 0.137556    
# location32:location72   1.697e-01  1.097e+00   0.155 0.877021    
# ...
# location41:location71   5.613e-01  3.359e-01   1.671 0.094713 .  
# location42:location71  -9.265e-01  1.045e+00  -0.886 0.375425    
# location41:location72   9.417e-01  7.763e-01   1.213 0.225099    
# location42:location72   1.536e+01  1.952e+04   0.001 0.999372    
# location41:location81   6.904e-01  3.948e-01   1.749 0.080330 .  
# location42:location81   2.460e+00  1.285e+00   1.915 0.055506 .  
# location41:location82  -1.299e+01  3.013e+03  -0.004 0.996561    
# location42:location82   1.767e+01  1.206e+04   0.001 0.998831    
# location41:location91  -4.806e-01  3.347e-01  -1.436 0.151054    
# location42:location91  -1.869e+00  1.106e+00  -1.691 0.090926 .  
# location41:location92  -8.334e-02  5.352e-01  -0.156 0.876270    
# ...
# location51:location72   4.484e+00  2.712e+00   1.654 0.098219 .  
# location52:location72   4.656e+00  2.241e+00   2.078 0.037725 *  
# location51:location81   3.052e+00  1.625e+00   1.878 0.060423 .  
# location52:location81   2.132e+00  1.486e+00   1.435 0.151310    
# ...
# location52:location102 -4.896e+01  2.905e+03  -0.017 0.986553    
# location61:location71   6.896e-01  3.126e-01   2.206 0.027377 *  
# location62:location71  -1.608e-01  5.687e-01  -0.283 0.777373    
# location61:location72  -1.478e+00  8.085e-01  -1.828 0.067521 .  
# location62:location72  -3.600e+00  1.071e+00  -3.363 0.000772 ***
# location61:location81   4.636e-02  3.572e-01   0.130 0.896729    
# ...
# location61:location91  -2.792e-01  3.117e-01  -0.896 0.370469    
# location62:location91  -1.444e+00  5.783e-01  -2.498 0.012502 *  
# location61:location92  -3.512e-01  5.295e-01  -0.663 0.507080    
# ...
# location71:location81  -5.873e-02  3.618e-01  -0.162 0.871023    
# location72:location81  -1.299e+00  7.276e-01  -1.785 0.074267 .  
# location71:location82  -1.524e+01  2.113e+03  -0.007 0.994244    
# location72:location82  -1.349e+02  7.314e+03  -0.018 0.985284    
# location71:location91   5.768e-01  3.150e-01   1.831 0.067115 .  
# location72:location91   1.393e+00  8.114e-01   1.716 0.086103 .  
# location71:location92   1.128e+00  5.735e-01   1.967 0.049187 *  
# location72:location92   1.261e+00  9.601e-01   1.314 0.188977    
# ...
# location72:location102  3.900e+00  2.208e+00   1.766 0.077369 .  
# location81:location91   1.193e-01  3.570e-01   0.334 0.738161    
# ...
# location82:location101 -7.261e+01  2.608e+03  -0.028 0.977786    
# location81:location102  3.091e+00  1.462e+00   2.115 0.034463 *  
# location82:location102 -1.557e+02  9.069e+03  -0.017 0.986301    
# ...
# location92:location102 -1.927e+01  2.262e+03  -0.009 0.993203    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2772.6  on 2000  degrees of freedom
# Residual deviance: 1430.5  on 1799  degrees of freedom
# AIC: 1832.5
# 
# Number of Fisher Scoring iterations: 20

