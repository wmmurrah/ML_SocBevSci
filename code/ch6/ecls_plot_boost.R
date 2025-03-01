library(rpart.plot)
ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');
ecls.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');

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

head(ecls.1)

myvars = c('k_math_irt','g8_sci','k_gk_irt','weight','height','income','ses_c','g8_read')

ecls.2 = ecls.1[ ,myvars]
colnames(ecls.2) <- c("math","science","knowledge",'weight','height','income','ses','read')

plot(ecls.2[,c(1,3)])


library(dismo);library(gbm)

#gbm.1 <- gbm.fixed(ecls.2,c(1,3:8),2,learning.rate=0.001,family="gaussian")
#gbm.1 <- gbm.fixed(ecls.2,c(1,3:8),2,learning.rate=0.001,family="gaussian")



gbm.11 <- gbm(science ~ ., distribution="gaussian",data=ecls.2,n.trees=10000,shrinkage=0.1)
#gbm.perf(gbm.11)
plot(gbm.11$train.error,type="l",xlab="Number of Trees",ylab="Train Error",ylim=c(60,200))

gbm.22 <- gbm(science ~ ., distribution="gaussian",data=ecls.2,n.trees=10000,shrinkage=0.01)
#gbm.perf(gbm.11)
lines(gbm.22$train.error,lty=2)

gbm.33 <- gbm(science ~ ., distribution="gaussian",data=ecls.2,n.trees=10000,shrinkage=c(0.001))
#gbm.perf(gbm.33)
lines(gbm.33$train.error,type="l",lty=3)
legend(5000, 160, legend = c("0.1", "0.01", "0.001"),lty = c(1,2,3),title="Shrinkage",cex=1,y.intersp=1)




# with CV



gbm.44 <- gbm(science ~ ., distribution="gaussian",data=ecls.2,n.trees=10000,shrinkage=0.1,cv.folds=5)
#gbm.perf(gbm.11)
plot(gbm.44$cv.error,type="l",xlab="Number of Trees",ylab="CV Error",ylim=c(60,200))

gbm.55 <- gbm(science ~ ., distribution="gaussian",data=ecls.2,n.trees=10000,shrinkage=0.01,cv.folds=5)
#gbm.perf(gbm.11)
lines(gbm.55$train.error,lty=2)

gbm.66 <- gbm(science ~ ., distribution="gaussian",data=ecls.2,n.trees=10000,shrinkage=c(0.001),cv.folds=5)
#gbm.perf(gbm.33)
lines(gbm.66$train.error,type="l",lty=3)
legend(5000, 180, legend = c("0.1", "0.01", "0.001"), lty = c(1,2,3),title="Shrinkage",cex=1,y.intersp=1)



# gbm.55 best
# set interaction depth to 5
X11()
gbm.77 <- gbm(science ~ ., distribution="gaussian",interaction.depth=5,
              data=ecls.2,n.trees=10000,shrinkage=0.01,cv.folds=5)
imp = relative.influence(gbm.77)

inter <- gbm.interactions(gbm.77)

gbm.88 <- gbm.step(ecls.2,c(1,3:8),2,tree.complexity=5,family="gaussian")
summary(gbm.88)
gbm.plot(gbm.88)
inter <- gbm.interactions(gbm.88)
inter$interactions
inter$rank.list

gbm.perspec(gbm.88,7,1)
