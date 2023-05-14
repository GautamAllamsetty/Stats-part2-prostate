library(glmnet)#lasso and ridge
library(ggplot2)
library(reshape2)
# Used R studio to open the prostate2(4).Rdata
load("/Users/namrataxxx/Desktop/Second_semester_materials/Statistical_methods_for_Bioinformatics/Part_2/Assignment/prostate2(4).Rdata")
dim(prostate)
sum (is.na(prostate$Cscore))
# Predictor variable svi takes only two values (0 & 1) so we can convert it into qualitative variable.
svi <- as.factor(prostate$svi)

# Q.1 Study and describe the predictor variables. Do you see any issues that are relevant for making predictions?


# Q.2 Generate your best linear regression model using only linear effects. Are there any indications that assumptions
#     underlying inferences with the model are violated? Evaluate the effect of any influential point, or outlier.



# Q.3 Make an appropriate LASSO model, with the appropriate link and error function, and evaluate the prediction performance.
#     Do you see evidence that over-learning is an issue?



# Q.4 Look at the coefficient for “lcavol” in your LASSO model. Does this coefficient correspond
#     to how well it can predict Cscore? Explain your observation.



# Q.5 Fit your best model with appropriate non-linear effects. Report a comparison of performance to LASSO and your model 
#     reported under question 2. Explain what you find, and indicate relevant issues or limitations of your analysis.

# Q.1 Study and describe the predictor variables. Do you see any issues that are relevant for making predictions?

# Ans correlation of predictors , etc.

# Predictor variables:
# The predictor variables in the prostate dataset are as follows:
# lcavol: log cancer volume
# lweight: log prostate weight
# age: age of the patient
# lbph: log of the amount of benign prostatic hyperplasia
# svi: seminal vesicle invasion
# lcp: log capsular penetration
# lpsa: log prostate-specific antigen

# Load the prostate dataset
# used option "open with R studio" to load the dataset.

# Examine the predictor variables
var_summary <- summary(prostate[, 2:8])
var_summary

#are there correlations between variables?
cormat =cor(prostate[, 2:8])

# make a plot
ggplot(data = melt(cormat), aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")+ geom_tile()
pairs(prostate[, 2:8])

# plot for Cscore vs each predictor(whether there is a relationship or not with response)
# 




# conclusion - Most of the correlations are positive with very little negative correlation between
# svi and lbph. There are only two variables,lbph and lcp which are not correlated. Among positive
# correlations lcavol,svi,lcp and lpsa are the one with very strong correlation. This correlation
# among predictors can inflate the variability of the estimated coefficients and will
# lead to overfitting of the dataset reducing generalizibility of the model to other dataset and thus
# prediction ability. The presence of collinearity can pose problems in the regression context, since it
# can be difficult to separate out the individual effects of collinear variables on the response.
# Since collinearity reduces the accuracy of the estimates of the regression coefficients, it causes the 
# standard error for βˆj to grow.

#When we fit a linear regression model to a particular data set, many problems may occur. Most common among these are the following:
#1. Non-linearity of the response-predictor relationships.- Residual plot can be used to study this.A
#   strong pattern in the residuals indicates non-linearity in the data. In our case the residuals show a pattern instead of scatter.

#2. Correlation of error terms. - An important assumption of the linear regression model is that the error
#   terms, e1, e2,.., en, are uncorrelated.if the errors are uncorrelated, then the fact that ei is positive provides
#   little or no information about the sign of ei+1. The standard errors that are computed for the estimated regression coefficients or the fitted values
#   are based on the assumption of uncorrelated error terms. If in fact there is correlation among the error terms, then the estimated standard errors
#   will tend to underestimate the true standard errors. As a result, confidence and prediction intervals will be narrower than they should be. 
#   p-values associated with the model will be lower than they should be; this could cause us to erroneously conclude that a parameter is statistically
#   significant. In short, if the error terms are correlated, we may have an unwarranted sense of confidence in our model.
#   Such correlations frequently occur in the context of time series data. I am not sure if this will apply in our case as our data is not time series.

#3. Non-constant variance of error terms.- Another important assumption of the linear regression model is that the
#  error terms have a constant variance, Var(ei) = σ2. The standard errors, confidence intervals, and hypothesis tests
#  associated with the linear model rely upon this assumption.Unfortunately, it is often the case that the variances of the error terms are
#  non-constant. For instance, the variances of the error terms may increase with the value of the response. One can identify non-constant variances in
#  the errors, or heteroscedasticity, from the presence of a funnel shape in the residual plot. In our case the funnel shape is not visible so the variance of error term is constant.

#4. Outliers- There may be outliers in the data that could skew the results of the prediction model.An outlier 
# is a point for which yi is far from the value predicted by the model. Outliers can arise for a variety of 
# reasons, such as incorrect recording of an observation during data collection.Residual plots can be used to identify outliers.
# But in practice, it can be difficult to decide how large a residual needs to be before we consider the point to be an outlier. To address
# this problem, instead of plotting the residuals, we can plot the studentized residuals, computed by dividing each residual ei by 
# its estimated standard studentized error. Observations whose studentized residuals are greater than 3 in absolute value are possible outliers.
# In our case there is one outlier. If we believe that an outlier has occurred due to an error in data collection or recording, then one solution
# is to simply remove the observation. However, care should be taken, since an outlier may instead indicate a deficiency with the model, such as a missing predictor.

#5. High-leverage points- observations with high leverage have an unusual value for xi. In fact, high leverage observations tend to have
#  a sizable impact on the estimated regression line. It is cause for concern if the least squares line is heavily affected by just a couple of observations,
#  because any problems with these points may invalidate the entire fit. For this reason, it is important to identify high leverage observations. 
# In order to quantify an observation’s leverage, we compute the "leverage statistic". A large value of this statistic indicates an observation with high leverage.

#6. Collinearity- Collinearity refers to the situation in which two or more predictor variables collinearity are closely related to one another.
#   The presence of collinearity can pose problems in the regression context, since it can be difficult to separate out the individual effects of
#   collinear variables on the response. Since collinearity reduces the accuracy of the estimates of the regression coefficients, 
#   it causes the standard error for βˆj to grow.collinearity results in a decline in the t-statistic. As a result, in the presence of collinearity,
#   we may fail to reject H0 : βj = 0. This means that the power of the hypothesis test—the probability of correctly detecting a non-zero coefficient—is reduced by collinearity.
#   A simple way to detect collinearity is to look at the correlation matrix of the predictors. An element of this matrix that is large in absolute value
#  indicates a pair of highly correlated variables, and therefore a collinearity problem in the data. Unfortunately, not all collinearity problems can be
#  detected by inspection of the correlation matrix: it is possible for collinearity to exist between three or more variables even if no pair of variables
#  has a particularly high correlation. We call this situation multicollinearity. Instead of inspecting the correlation matrix, a better way to assess 
#  multi- collinearity collinearity is to compute the variance inflation factor (VIF).The VIF is variance the ratio of the variance of βˆj when fitting the full model divided by 
#  the variance of βˆj if fit on its own. The smallest possible value for VIF is 1, which indicates the complete absence of collinearity.
# In our case most VIF's are low (1-3).  As a rule ofthumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity.

# Q.2 Generate your best linear regression model using only linear effects. Are there any indications that assumptions
#     underlying inferences with the model are violated? Evaluate the effect of any influential point, or outlier.
# Full model to check for outlier:
lm_model <- lm(Cscore ~., data = prostate)
summary(lm_model)$coef

# Outlier effect:
par(mfrow =c(2 ,2))
plot(lm_model)
plot( predict (lm_model), residuals (lm_model))
plot( predict (lm_model), rstudent (lm_model)) # Observations whose studentized residuals are greater than 3 in absolute value are possible outliers.
# plot to find high leverage points.
plot( hatvalues (lm_model))
# Conclusion - From the residual plot, observation number 96 is an outlier. So we removed it 
# and then do the variable selection.

# Best subset selection on full dataset:
dim(prostate)
library(leaps)
regfit.full = regsubsets (Cscore~.,data=prostate)
reg.summary=summary(regfit.full)
reg.summary
names(reg.summary)
reg.summary$adjr2
par (mfrow =c(2 ,2))
plot(reg.summary$rss , xlab =" Number of Variables ", ylab =" RSS ",
     type ="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
     ylab =" Adjusted RSq ", type ="l")

which.max(reg.summary$adjr2) # 5 Variables
plot(reg.summary$cp ,xlab =" Number of Variables ", ylab =" Cp",
     type='l')

which.min(reg.summary$cp) # 5 Variables
points (5, reg.summary$cp[5], col =" red ", cex =2, pch =20)

which.min (reg.summary$bic ) # 2 variables
plot(reg.summary$bic , xlab =" Number of Variables ", ylab =" BIC ",
     type='l')
points (2, reg.summary$bic [2], col =" red ",cex =2, pch =20)

plot(regfit.full,scale ="r2")
plot(regfit.full,scale="adjr2") # age and lbph removed
plot(regfit.full,scale="Cp")  # age and lbph removed
plot(regfit.full,scale="bic") # only svi and lpsa kept
coef(regfit.full,5) # no. of variables chosen according to cp and adjr2
reg.summary$adjr2
# adjr2 is 0.569

#K fold cross validation on full dataset
k =10
set.seed (1)
folds= sample (1:k,nrow(prostate),replace=TRUE)
cv.errors=matrix (NA ,k ,7, dimnames=list(NULL,paste(1:7)))

for (j in 1:k){
  best.fit = regsubsets(Cscore~., data=prostate[folds !=j ,],)
  for (i in 1:7) {
    pred= predict(best.fit,prostate[ folds ==j,],id=i)
    cv.errors [j,i]= mean((prostate$Cscore[ folds ==j]- pred)^2)
  }
}
mean.cv.errors = apply(cv.errors,2,mean)
mean.cv.errors  # predicted cv error(test MSE) with 5 variables is 1525.874
par ( mfrow =c(1 ,1) )
plot(mean.cv.errors ,type ='b')
reg.best= regsubsets(Cscore~., data=prostate)
coef(reg.best,5)

# After best subset selection(a/c to adjr2 and Cp) and K-fold validation on full dataset, 5 variables were selected
# removing age and lbph.
# Linear model after removing age and lbph:
lm_model_full_best <- lm(Cscore ~ lcavol+lweight+svi+lcp+lpsa, data = prostate)
lm_model_full_best.summary <- summary(lm_model_full_best)
lm_model_full_best.summary$adj.r.squared
# adjr2 is 0.569 (same as reg.summary)
 

# Removing the outlier:
prostate.less <- prostate[-96,]
dim(prostate.less)
lm_model_less <- lm(Cscore ~.,data = prostate.less) 
lm_model_less.summary<- summary(lm_model_less)
lm_model_less.summary
lm_model_less.summary$adj.r.squared

# After removing the outlier the adjr2 is 0.6346

# Conclusion - Now lcp and lpsa both are significant.The SE of coefficients reduced
# in the model with reduced dataset.

# Best subset selection on reduced dataset:
dim(prostate.less)
library(leaps)
regfit.less = regsubsets(Cscore~.,data=prostate.less)
reg.summary.less=summary(regfit.less)
reg.summary.less
names(reg.summary.less)
reg.summary.less$adjr2

par (mfrow =c(2 ,2))
plot(reg.summary.less$rss , xlab =" Number of Variables ", ylab =" RSS ",
       type ="l")
plot(reg.summary.less$adjr2 ,xlab =" Number of Variables ",
       ylab =" Adjusted RSq ", type ="l")

which.max(reg.summary.less$adjr2) # 5 Variables
plot(reg.summary$cp ,xlab =" Number of Variables ", ylab =" Cp",
     type='l')

which.min(reg.summary.less$cp) # 4 Variables
points (5, reg.summary.less$cp[4], col =" red ", cex =2, pch =20)

which.min (reg.summary.less$bic ) # 2 variables
plot(reg.summary.less$bic , xlab =" Number of Variables ", ylab =" BIC ",
     type='l')
points (2, reg.summary.less$bic [2], col =" red ",cex =2, pch =20)

plot(regfit.less,scale ="r2")
plot(regfit.less,scale="adjr2") # removed lweight and age
plot(regfit.less,scale="Cp") # removed lweight, age and svi
plot(regfit.less,scale="bic") # kept only lcp and lpsa
coef(regfit.less,2) # How many variables to take?????? a/c to Cp and adjr2 variable number is 5 and 4 and a/c to bic 2.
                    # selected number of variables according to bic


# Cross-validation: 
set.seed (1)
train=sample (c(TRUE , FALSE ), nrow(prostate.less),rep =TRUE )
test =(! train )

# not useful
regfit.best = regsubsets(Cscore~., data=prostate.less[train,])
test.mat = model.matrix(Cscore~., data=prostate.less[test,])
val.errors =rep (NA,7)
for (i in 1:7){  
  coefi = coef (regfit.best ,id=i)   # throwing error saying object coefi not found
  pred =test.mat[,names(coefi)]%*% coefi
  val.errors [i]= mean (( prostate.less$Cscore[test]- pred )^2)
}
val.errors
which.min(val.errors)
coef( regfit.best ,1) # Is number of variables correct.

# Function to for prediction:
predict.regsubsets = function (object, newdata, id ,...) {
  form=as.formula(object$call[[2]])
  mat = model.matrix(form,newdata )
  coefi =coef(object, id=id)
  xvars =names (coefi)
  mat[,xvars]%*% coefi}


# Best subset selection on full data with outlier removed
regfit.best= regsubsets(Cscore~., data=prostate.less)
coef(regfit.best ,2) # number of variables wa selected a/c to val.error result.

#K fold cross validation on reduced dataset
k =10
set.seed (1)
folds = sample (1:k,nrow(prostate.less),replace=TRUE)
cv.errors=matrix (NA ,k ,7, dimnames=list(NULL,paste(1:7)))

for (j in 1:k){
  best.fit = regsubsets(Cscore~., data=prostate.less[folds !=j ,],)
  for (i in 1:7) {
     pred= predict(best.fit,prostate.less[ folds ==j,],id=i)
     cv.errors [j,i]= mean((prostate.less$Cscore[ folds ==j]- pred)^2)
     }
}
mean.cv.errors = apply(cv.errors,2,mean)
mean.cv.errors  # predicted cv error(test MSE) with 2 variables is 678.440
par ( mfrow =c(1 ,1) )
plot(mean.cv.errors ,type ='b')
reg.best= regsubsets(Cscore~., data=prostate.less)
coef(reg.best,2) 

# After best subset selection in reduced dataset, a/c to adjr2 5 variables and a/c to cp
# value 4 variables are important. A/c to K-fold only 2 variables are important(lcp and lpsa)
plot(Cscore~lcp, data=prostate.less)
plot(Cscore~lbph, data=prostate.less)
lm_reduced_best <- lm(Cscore~lcp+lbph, data=prostate.less)
summary(lm_reduced_best)
par(mfrow =c(2 ,2))
plot(lm_reduced_best)
plot( predict (lm_reduced_best), residuals (lm_reduced_best))


##################################################
# Unnecessary part
install.packages("car")
library(car)
vif (lm_model) # Most VIF's are low.

# Check for non-linearity (residual plot), shapiro test for normality assumption. check for homoscedasity of predictor variable, multicollinearity,


# Non-linear relationship of response and predictors can be studied to evaluate underlying assumptions.Residual plot can
# be studied to evaluate any influential point and if found the model can be regenerated after removing the influential point
# and residual plot of this new model can be plotted to check if it improves the model fit or not?


# USing bootstrap to estimate the standard error for the coefficients estimates.
# Bootstrap is more accurate than the standard error estimated through the standard formula ["summary(lm_model)"]
# function because these estimates depend on sigma^2 (noise variance) which depends on the model(calculated using RSS).
# And if the underlying model is not correct then sigma^2 will be incorrect and thus the SE estimations.
# Also the standard formula assumes that all the variability in data comes from variation in the errors "epsilon".
# Bootstrap does not rely on any of these assumptions.



# Q.3 Make an appropriate LASSO model, with the appropriate link and error function, and evaluate the prediction performance.
#     Do you see evidence that over-learning is an issue?
x=model.matrix(Cscore~.,prostate.less)[,-1]
y=prostate.less$Cscore
library(glmnet)
# LASSO model:
#define series of values for lambda over large range, then perform ridge fit; alpha=0, lasso would be alpha=1
grid=10^seq(10,-2,length=100) 
#make test and training set
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]


#LASSO
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid) #alpha is 1 for lasso!
#observe the % of deviance explained over different value of lambda and # of components
print(lasso.mod)
plot(lasso.mod)#the coefficents go to zero
plot(lasso.mod,xvar="lambda",label=TRUE)#behaviour coef plotted against log lambda
plot(lasso.mod,xvar="dev",label=TRUE)#behaviour coef plotted against percentage deviance explained
#perform cross-validation for lambda, then measure performance and look at selected coefficients
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
cv.out$
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:8,]
lasso.coef
lasso.coef[ lasso.coef !=0]
# The prediction performance of the LASSO model was evaluated using mean squared error (MSE).

# Checking MSE for least square model (lm() can also be used instead of using lasso.pred and s=0)
lasso.pred_leastsq= predict(lasso.mod ,s=0, newx=x[test ,])
mean ((lasso.pred_leastsq -y.test)^2)
# MSE using least square model is... while using lasso is...  and this shows that overlearning.....
# No overlearning in Lasso prediction. t-test(Lasso with training and lasso with test)

# Q.4 Look at the coefficient for “lcavol” in your LASSO model. Does this coefficient correspond
#     to how well it can predict Cscore? Explain your observation.

# Ans -The coefficient for lcavol is -4.7189. 

# Q.5 Fit your best model with appropriate non-linear effects. Report a comparison of performance to LASSO and your model 
#     reported under question 2. Explain what you find, and indicate relevant issues or limitations of your analysis.

# Best model with non-linear effects:
# The best model with appropriate non-linear effects was generated using a generalized additive model (GAM) with a Gaussian error distribution and a log link function. The model included the same variables as the linear model, but with non-linear terms added for "lcavol" and "lpsa". 
# The prediction performance of the GAM model can be evaluated using MSE and R-s.
# To incorporate non-linear effects we can use natural spline or smoothing spine as non-linear functions
# for all the predictors. Anova can be used to check which model is better(nested models).Do we need to consider interaction terms also?



#B-D. Fit a GAM, plot the results, evaluate the model. Are there non-linear effects?
#
library(gam)
# used same training and test dataset as used in Lasso
x=model.matrix(Cscore~.,prostate.less)[,-1]
y=prostate.less$Cscore
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

regfit.fwd=regsubsets(Cscore~.,prostate.less[train,],method="forward")
summary(regfit.fwd)
plot(summary(regfit.fwd)$bic,type="b",ylab="BIC")#we select 4 variables
plot(regfit.fwd, scale="bic")

regfit.best=regsubsets(Cscore~.,prostate.less[train,])
summary(regfit.best)
plot(summary(regfit.fwd)$bic,type="b",ylab="BIC")#we select 4 variables
plot(regfit.best, scale="bic")



# removed "age" as observed from lasso fitting
gam1 = gam(Cscore~svi+s(age,4)+s(lcavol,4) +s(lweight,4)+ s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
#ignore the gam "non-list contrasts" warning; it's a (harmless) bug
par(mfrow=c(2,4))
plot(gam1,se=TRUE,col="purple")#lweight,lbph,lcp mostly non-linear, Terminal non-linear effect has rel. high error so lweight has high error, lcavol and lpsa looks non-linear
summary(gam1)# lweight,lbph,lcp seems to have non-linear effect, lpsa linear, not sure about lcavol
predgam = predict(gam1, newdata=prostate.less[test,]) 
msegam1 = mean((predgam-prostate.less[test,"Cscore"])^2)
msegam1
# Fitting GAM model again by removing age
gam2 = gam(Cscore~svi+s(lcavol,4)+s(lweight,4)+s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
plot(gam2,se=TRUE,col="purple")#lpsa mostly linear, lbph and lweight has non-linear effect with high error, lcp looks non-linear, not sure about lcavol
summary(gam2)#only lpsa seems to have linear effect
predgam2 = predict(gam2, newdata=prostate.less[test,]) 
msegam2 = mean((predgam2-prostate.less[test,"Cscore"])^2)#not identical to msegam1 for my train/test?
msegam2
anova(gam1,gam2)#simplification justified as expected.?

# Fitting GAM model again by removing age and making lcavol and lweight linear
gam3 = gam(Cscore~svi+lcavol+lweight+ s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
plot(gam3,se=TRUE,col="purple")#lpsa mostly linear, lbph and lweight has non-linear effect with high error, lcp looks non-linear, not sure about lcavol
summary(gam3)#only lpsa seems to have linear effect
predgam3 = predict(gam3, newdata=prostate.less[test,]) 
msegam3 = mean((predgam3-prostate.less[test,"Cscore"])^2)#not identical to msegam1 for my train/test?
msegam3
anova(gam1,gam2,gam3)

# used lm function 
lm1 = lm(Cscore~svi+lcavol+lweight+lbph+lcp+lpsa,data=prostate.less,subset=train)
predlm1= predict(lm1, newdata=prostate.less[test,]) 
mselm1 = mean((predlm1-prostate.less[test,"Cscore"])^2)
mselm1

lm2 = lm(Cscore~svi+s(lcavol,4)+s(lweight,4)+ s(lbph,4)+s(lcp,4)+lpsa,data=prostate.less,subset=train)
predlm2 = predict(lm2, newdata=prostate.less[test,]) 
mselm2 = mean((predlm2-prostate.less[test,"Cscore"])^2) # worse or better than msegam2?
mselm2

# both lm1 and lm2 gives same mse?????