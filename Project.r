### STATISTICS FOR BIOINFORMATICS PART2 ASSIGNMENT ##############
library(glmnet)
library(ggplot2)
library(ggcorrplot)
library(reshape2)
library(dplyr)
library(tidyr)
library(leaps)
library(car)
library(gam)

# Open the prostate2(4).Rdata
load("/Users/namrataxxx/Desktop/Second_semester_materials/Statistical_methods_for_Bioinformatics/Part_2/Assignment/prostate2(4).Rdata")
dim(prostate)
sum (is.na(prostate$Cscore))

# Q.1 Study and describe the predictor variables. Do you see any issues that are relevant for making predictions?

# EXAMINING PREDICTORS
var_summary <- summary(prostate[, 2:8])
var_summary

# CORRELATION B/W VARIABLES
cormat =cor(prostate[, 2:8])
cormat

# CORRELATION MATRIX
par (mfrow =c(1 ,2))
reduced <- prostate[, -1]
reduced$svi <- as.numeric(reduced$svi)
cormat <- round(cor(reduced), 2)
ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE) +
  theme(panel.grid.major = element_blank())

# CSCORE vs EACH PREDICTOR
prostate_tibble <- as_tibble(prostate)
head(prostate_tibble)

prostate_tibble_new <- prostate_tibble %>%
  select(!svi) %>% 
  pivot_longer(cols = c(lcavol, lweight, age, lbph, lcp, lpsa),
               names_to = "Variable",
               values_to = "Value")
ggplot(prostate_tibble_new, aes(x = Value, y = Cscore)) +
  geom_point(color = "firebrick") +
  ggtitle("C-score vs. individual predictors") +
  facet_wrap(~Variable, scales = "free") +
  ylab("C-score") +
  theme_bw()

# BOX PLOT OF EACH PREDICTOR.
ggplot(prostate_tibble_new, aes(y = Value)) +
  facet_wrap(~Variable, scales = "free")+ 
  theme_bw()+
  labs(title = "Boxplot of each predictor vs response variable", 
       x = "Predictor Variables", y = "Cscore") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(outlier.shape = 1,  fill = "skyblue", width = 0.5) +
  xlim(c(-1, 1))



# Q.2 Generate your best linear regression model using only linear effects. Are there any indications that assumptions
#     underlying inferences with the model are violated? Evaluate the effect of any influential point, or outlier.

# BEST SUBSET SELECTION ON FULL DATASET
regfit.full = regsubsets (Cscore~.,data=prostate)
reg.summary=summary(regfit.full)
reg.summary

par (mfrow =c(2 ,2))
plot(reg.summary$rss , xlab =" Number of predictor variables ", ylab =" RSS ",
     type ="l")

plot(reg.summary$adjr2 ,xlab =" Number of predictor variables ",
     ylab =" Adjusted R square", type ="l")
which.max(reg.summary$adjr2) # 5 Variables
points (5, reg.summary$adjr2[5], col =" red ", cex =2, pch =20)

plot(reg.summary$cp ,xlab =" Number of predictor variables ", ylab =" Cp",
     type='l')
which.min(reg.summary$cp) # 5 Variables
points (5, reg.summary$cp[5], col =" red ", cex =2, pch =20)

which.min (reg.summary$bic ) # 2 variables
plot(reg.summary$bic , xlab =" Number of predictor variables ", ylab =" BIC ",
     type='l')
points (2, reg.summary$bic [2], col =" red ",cex =2, pch =20)

plot(regfit.full,scale ="r2")
plot(regfit.full,scale="adjr2") 
plot(regfit.full,scale="Cp")  
plot(regfit.full,scale="bic") 
coef(regfit.full,5) 
reg.summary$adjr2


# PREDICTION FUNCTION
predict.regsubsets = function (object, newdata, id ,...) {
  form=as.formula(object$call[[2]])
  mat = model.matrix(form,newdata )
  coefi =coef(object, id=id)
  xvars =names (coefi)
  mat[,xvars]%*% coefi}

# K-FOLD CROSS-VALIDATION ON FULL DATASET
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
mean.cv.errors 
par ( mfrow =c(1 ,1) )
plot(mean.cv.errors ,type ='b',xlab =" Number of predictor variables ",ylab = " Mean CV error")
points (5, mean.cv.errors[5], col =" red ",cex =2, pch =20)
reg.best= regsubsets(Cscore~., data=prostate)
coef(reg.best,5)

# BEST LINEAR MODEL ON FULL DATASET AFTER REMOVING AGE AND LBPH (K-FOLD AND BEST SUBSET SELECTION)
lm_model_full_best <- lm(Cscore ~ lcavol+lweight+svi+lcp+lpsa, data = prostate)
lm_model_full_best.summary <- summary(lm_model_full_best)
lm_model_full_best.summary
lm_model_full_best.summary$adj.r.squared

# OUTLIER EFFECT
par(mfrow =c(2 ,2))
plot(lm_model_full_best)
plot( predict (lm_model_full_best), residuals (lm_model_full_best))
plot( predict (lm_model_full_best), rstudent (lm_model_full_best)) 

# OUTLIER REMOVAL
prostate.less <- prostate[-96,]
prostate.less$svi <- as.factor(prostate.less$svi)


# BEST SUBSET SELECTION ON REDUCED DATASET
dim(prostate.less)
regfit.less = regsubsets(Cscore~.,data=prostate.less)
reg.summary.less=summary(regfit.less)
reg.summary.less

par (mfrow =c(2 ,2))
plot(reg.summary.less$rss , xlab =" Number of predictor variables ", ylab =" RSS ",
     type ="l")
plot(reg.summary.less$adjr2 ,xlab =" Number of predictor variables ",
     ylab =" Adjusted R Square ", type ="l")
which.max(reg.summary.less$adjr2) 
points (5, reg.summary.less$adjr2[5], col =" red ", cex =2, pch =20)


plot(reg.summary.less$cp ,xlab =" Number of predictor variables ", ylab =" Cp",
     type='l')
which.min(reg.summary.less$cp) 
points (4, reg.summary.less$cp[4], col =" red ", cex =2, pch =20)

which.min (reg.summary.less$bic ) 
plot(reg.summary.less$bic , xlab =" Number of predictor variables ", ylab =" BIC ",
     type='l')
points (2, reg.summary.less$bic [2], col =" red ",cex =2, pch =20)

plot(regfit.less,scale ="r2")
plot(regfit.less,scale="adjr2")
plot(regfit.less,scale="Cp") 
plot(regfit.less,scale="bic") 
coef(regfit.less,2) 
reg.summary.less$adjr2

# K-FOLD CROSS-VALIDATION ON REDUCED DATASET
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
mean.cv.errors
par ( mfrow =c(1 ,1) )
plot(mean.cv.errors ,type ='b',xlab =" Number of predictor variables ",ylab = " Mean CV error")
points (2, mean.cv.errors[2], col =" red ",cex =2, pch =20)
reg.best= regsubsets(Cscore~., data=prostate.less)
coef(reg.best,2) 

# BEST LINEAR MODEL ON REDUCED DATASET KEEPING ONLY LCP AND LPSA (K-FOLD AND BEST SUBSET SELECTION)
lin_model <- lm(Cscore ~ lcp + lpsa, data = prostate.less)
lin_model_best.summary <- summary(lin_model)
lin_model_best.summary 
mean((lin_model$fitted.values - prostate.less$Cscore)^2)

##### OBSERVING LINEAR MODEL ASSUMPTIONS ##########################

## CHECKING NORMALITY OF RESIDUALS
par(mfrow=c(1,2))
shapiro.test(lin_model$residuals)
hist(lin_model$residuals, 
     main = "Histogram of residuals", 
     xlab = "Residuals",
     col = "skyblue")

plot(lin_model, 2)

## CHECKING HOMOSCEDASTICITY

plot(fitted(lin_model), rstandard(lin_model))

## CHECKING LINEARITY OF RESIDUALS

plot(lin_model, 1)

# Q.3 Make an appropriate LASSO model, with the appropriate link and error function, and evaluate the prediction performance.
#     Do you see evidence that over-learning is an issue?

x=model.matrix(Cscore~.,prostate.less)[,-1]
y=prostate.less$Cscore

# LASSO MODEL
grid=10^seq(10,-2,length=100) 
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
par(mfrow=c(1,3))
plot(lasso.mod,xvar="lambda",label=TRUE)
plot(lasso.mod,xvar="dev",label=TRUE)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:8,]
lasso.coef
lasso.coef[ lasso.coef !=0]

# Q.4 Look at the coefficient for “lcavol” in your LASSO model. Does this coefficient correspond
#     to how well it can predict Cscore? Explain your observation.
lasso.coef[2]
# Ans -The coefficient for lcavol is -4.7189. 

# Q.5 Fit your best model with appropriate non-linear effects. Report a comparison of performance to LASSO and your model 
#     reported under question 2. Explain what you find, and indicate relevant issues or limitations of your analysis.
x=model.matrix(Cscore~.,prostate.less)[,-1]
y=prostate.less$Cscore
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# FORWARD SELECTION ON REDUCED DATASET FOR GAM
regfit.fwd=regsubsets(Cscore~.,prostate.less[train,],method="forward")
summary(regfit.fwd)
plot(summary(regfit.fwd)$bic,type="b",ylab="BIC")#we select 4 variables
plot(regfit.fwd, scale="bic")
coef( regfit.fwd ,4)

# BEST SUBSET SELECTION ON REDUCED DATASET FOR GAM
regfit.best=regsubsets(Cscore~.,prostate.less[train,])
summary(regfit.best)
plot(summary(regfit.fwd)$bic,type="b",ylab="BIC")#we select 4 variables
plot(regfit.best, scale="bic")
coef( regfit.best ,4)

#FITTING GAM BY REMOVING AGE,LWEIGHT AND LBPH (A/C TO BEST SUBSET & FORWARD SELECTION) AND EVERY OTHER PREDICTOR AS NON-LINEAR TERM
gam3 = gam(Cscore~svi+s(lcavol,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train)
summary(gam3)
predgam3 = predict(gam3, newdata=prostate.less[test,]) 
msegam3 = mean((predgam3-prostate.less[test,"Cscore"])^2)
msegam3 

##################### FINAL BEST GAM ##########################################
#FITTING GAM BY REMOVING AGE,LWEIGHT AND LBPH (A/C TO BEST SUBSET & FORWARD SELECTION) & KEEPING LCAVOL LINEAR
gam4 = gam(Cscore~svi+lcavol+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam3,se=TRUE,col="purple")
mtext("Final model with lcavol, lcp, svi and lpsa predictors", 
      side = 3, 
      line = -2,
      outer = TRUE)
plot(gam4,se=TRUE,col="purple")
mtext("Final model with linear lcavol predictor", 
      side = 3, 
      line = -21,
      outer = TRUE)
summary(gam4)
predgam4 = predict(gam4, newdata=prostate.less[test,]) 
msegam4 = mean((predgam4-prostate.less[test,"Cscore"])^2)
msegam4
anova(gam3,gam4)

###############################################################################################
# COMPARING DIFFERENT GAMs WITH VARIOUS COMBINATION OF PREDICTORS(BOTH LINEAR AND NON-LINEAR TERMS)

# FITTING FULL GAM 
gam1 = gam(Cscore~svi+s(age,4)+s(lcavol,4) +s(lweight,4)+ s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam1,se=TRUE,col="purple")
summary(gam1)
predgam1 = predict(gam1, newdata=prostate.less[test,]) 
msegam1 = mean((predgam1-prostate.less[test,"Cscore"])^2)
msegam1  

# FITTING GAM BY REMOVING AGE (A/C TO LASSO) AND EVERY OTHER PREDICTOR AS NON-LINEAR TERM
gam2 = gam(Cscore~svi+s(lcavol,4)+s(lweight,4)+s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam2,se=TRUE,col="purple")
summary(gam2)
predgam2 = predict(gam2, newdata=prostate.less[test,]) 
msegam2 = mean((predgam2-prostate.less[test,"Cscore"])^2) 
msegam2 
anova(gam1,gam2)

# FITTING GAM WITH ONLY LCP AND LPSA AS NON-LINEAR 
gam5 = gam(Cscore~s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam5,se=TRUE,col="purple")
summary(gam5)
predgam5 = predict(gam5, newdata=prostate.less[test,]) 
msegam5 = mean((predgam5-prostate.less[test,"Cscore"])^2)
msegam5 
anova(gam5,gam4)

# FITTING GAM WITH AGE AS LINEAR & REST NON-LINEAR
gam6 = gam(Cscore~svi+age+s(lcavol,4)+s(lweight,4)+s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam6,se=TRUE,col="purple")
summary(gam6)
predgam6 = predict(gam6, newdata=prostate.less[test,]) 
msegam6 = mean((predgam6-prostate.less[test,"Cscore"])^2)
msegam6 

# FITTING GAM WITH AGE & LCAVOL AS LINEAR & REST NON-LINEAR
gam7 = gam(Cscore~svi+age+lcavol+s(lweight,4)+s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam7,se=TRUE,col="purple")
summary(gam7)
predgam7 = predict(gam7, newdata=prostate.less[test,]) 
msegam7 = mean((predgam7-prostate.less[test,"Cscore"])^2)
msegam7


# FITTING GAM WITH AGE, LCAVOL & LWEIGHT AS LINEAR & REST NON-LINEAR
gam8 = gam(Cscore~svi+age+lcavol+lweight+s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam8,se=TRUE,col="purple")
summary(gam8)
predgam8 = predict(gam8, newdata=prostate.less[test,]) 
msegam8 = mean((predgam8-prostate.less[test,"Cscore"])^2)
msegam8 


#############################################################################
# FITTING GAM AFTER REMOVING LWEIGHT & KEEPING AGE, LCAVOL AS LINEAR & REST NON-LINEAR 
gam9 = gam(Cscore~svi+age+lcavol+s(lbph,4)+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam9,se=TRUE,col="purple")
summary(gam9)
predgam9 = predict(gam9, newdata=prostate.less[test,]) 
msegam9 = mean((predgam9-prostate.less[test,"Cscore"])^2)
msegam9 

# FITTING GAM AFTER REMOVING LWEIGHT & LBPH & KEEPING AGE, LCAVOL AS LINEAR & REST NON-LINEAR 
gam10 = gam(Cscore~svi+age+lcavol+s(lcp,4)+s(lpsa,4),data=prostate.less,subset=train) 
par(mfrow=c(2,4))
plot(gam10,se=TRUE,col="purple")
summary(gam10)
predgam10 = predict(gam10, newdata=prostate.less[test,]) 
msegam10 = mean((predgam10-prostate.less[test,"Cscore"])^2)
msegam10
anova(gam9,gam10)
