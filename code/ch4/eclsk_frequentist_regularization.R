
#ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/ecls_DM.dat', na='.');
ecls.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');
#ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');

#library(synthpop)

#ecls_syn <- syn(ecls.1)$syn
#write.csv(ecls_syn,"G:/My Drive/PSY-ML-Fall19/4.Scripts/ecls_syn.csv")

# or pull in synthetic version
# note will give slightly different results
#ecls.1 <- read.csv(file.choose())

names(ecls.1) = c('gender','kage',
                  'k_read_irt','k_read1','k_read2','k_read3','k_read4',
                  'k_print','k_read_tht',
                  'k_math_irt','k_math1','k_math2','k_math3','k_math4',
                  'k_math_tht',
                  'k_gk_irt','k_gk_tht',
                  'f_mtr','g_mtr',
                  'P1LEARN','P1CONTRO','P1SOCIAL','P1SADLON','P1IMPULS',
                  'ars_lit','ars_mth','ars_gk',
                  'T1LEARN','T1CONTRO','T1INTERP','T1EXTERN','T1INTERN',
                  'height','weight','bmi',
                  'hisp','na_amer','asian','black','pac_isl','white','m_race',
                  'ses_c','ses_cat','poor','income',
                  'g8_read','g8_read_tht','g8_math','g8_math_tht',
                  'g8_sci','g8_sci_tht')

# subset of variables
x.vars = c('k_math_irt','k_gk_irt',"income","bmi","gender")
set.seed(1)
x.noise <- matrix( rnorm(nrow(ecls.1)*20,mean=0,sd=1), nrow(ecls.1), 20)
y.vars = 'g8_sci'
ecls.1$gender = ecls.1$gender - 1
XX <- data.matrix(cbind(ecls.1[,x.vars],x.noise))
XX.std <- data.matrix(cbind(scale(ecls.1[,x.vars]),x.noise))
YY <- as.numeric(scale(ecls.1[,y.vars]))



# linear regression
dat.comb <- data.frame(YY,XX.std)
lm.out <- lm.fit(XX,YY)
summary(lm.out)
lm.out




library(glmnet)
# ?glmnet
# return in original scale
lasso.out <- glmnet(XX,YY,family="gaussian",alpha=1)
plot(lasso.out,col="black")

lasso.cv <- cv.glmnet(XX,YY,family="gaussian",alpha=1)
plot(lasso.cv,size=2)

round(coef(lasso.cv,lasso.cv$lambda.1se),3)
round(coef(lasso.cv,lasso.cv$lambda.min),3)


relax.out <- lm(scale(g8_sci) ~ scale(income) + scale(k_math_irt) + scale(k_gk_irt),ecls_syn)
summary(relax.out)
round(coef(relax.out),3)

# ridge

ridge.out <- glmnet(XX,YY,family="gaussian",alpha=0)
plot(ridge.out,col="black")

ridge.cv <- cv.glmnet(XX,YY,family="gaussian",alpha=0)
plot(ridge.cv)
round(coef(ridge.cv,ridge.cv$lambda.1se),3)


#lasso.std <- glmnet(XX.std,YY,family="gaussian",alpha=1,intercept=F,standardize=F)
#plot(lasso.std)

# penalized package
#library(penalized)
#fit1 <- penalized(YY, penalized=XX, unpenalized=~0, standardize=TRUE)
#round(coefficients(fit1),3)


# lasso p-values

library(covTest) # have to install from cran arxiv https://cran.r-project.org/src/contrib/Archive/covTest/
a=lars.en(XX,YY,lambda2=0)
cov.out = covTest(a,XX,YY)
cov.out$results[1:4,1] # get predictor numbers

head(XX[,c(2,1,3,5)])




# hierarchical lasso
set.seed(1)
ids <- sample(1:nrow(XX.std),nrow(XX.std)*.5)
XX.train <- XX.std[ids,]
XX.test <- XX.std[-ids,]
YY.train <- YY[ids]
YY.test <- YY[-ids]

library(hierNet)

out = hierNet.path(XX.train,YY.train,strong=TRUE)
out.cv = hierNet.cv(out,XX.train,YY.train)

plot(out.cv)
out.cv

out$th[,,11] # interactions and quadratic effects
out$bp[,11] # positive main effects
out$bn[,11] # negative main effects

# test if weak identifies others.
out.weak = hierNet.path(XX.train,YY.train,strong=FALSE)
out.cv.weak = hierNet.cv(out.weak,XX.train,YY.train)
plot(out.cv.weak)
out.cv.weak

out.weak$th[,,11] # interactions
out.weak$bp[,11]
out.weak$bn[,11]


# re-run model

dat.comb <- data.frame(XX.test,YY.test)
colnames(dat.comb)[1:5] <- c("math","knowledge","income","bmi","gender")
colnames(dat.comb)[26] <- "science"

lm.int <- lm(science ~ math + knowledge + income + bmi + gender +
               I(math^2) + I(knowledge^2) + I(income^2) + I(bmi^2) +
               math*knowledge + knowledge*income,
             dat.comb)
summary(lm.int)
stargazer::stargazer(lm.int,column.sep.width = "1pt",single.row = TRUE)




# try stability selection

# stability
library(stabs)
stab.out11 <- stabsel(XX,YY,cutoff=0.75,PFER=1)
stab.out11
