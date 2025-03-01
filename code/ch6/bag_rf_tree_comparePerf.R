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


gbm.ecls <- gbm.step(ecls.2,c(1,3:8),2,tree.complexity=3,family="gaussian")
summary(gbm.ecls)
gbm.plot(gbm.ecls)
inter <- gbm.interactions(gbm.ecls)
inter$interactions
inter$rank.list

#gbm.1 <- gbm.fixed(ecls.2,c(1,3:8),2,learning.rate=0.001,family="gaussian")
#gbm.1 <- gbm.fixed(ecls.2,c(1,3:8),2,learning.rate=0.001,family="gaussian")

ids <- sample(1:nrow(ecls.2),1000)
ecls.train <- ecls.2[-ids,]
ecls.test <- ecls.2[ids,]


library(rpart);library(ipred);library(randomForest)
tree.out <- rpart(science ~ ., ecls.train)
rsq_tree_train <- cor(predict(tree.out,ecls.train),ecls.train$science)**2
rsq_tree_test <- cor(predict(tree.out,ecls.test),ecls.test$science)**2

ret.mat <- matrix(NA,1000,4)

for(i in 1:1000){
  bag.out1 = bagging(science ~ .,data=ecls.train,nbagg=i,method="standard")
  rsq.bag.train = 1- cor(predict(bag.out1,ecls.train),ecls.train$science)**2
  rsq.bag.test = 1- cor(predict(bag.out1,ecls.test),ecls.test$science)**2
  
  rf.out1 = randomForest(science ~ .,data=ecls.train,ntree=i)
  rsq.rf.train = 1- cor(predict(rf.out1,ecls.train),ecls.train$science)**2
  rsq.rf.test = 1- cor(predict(rf.out1,ecls.test),ecls.test$science)**2
  
  ret.mat[i,] = c(rsq.bag.train,rsq.bag.test,rsq.rf.train,rsq.rf.test)
}

#saveRDS(ret.mat,"bag_rf_fit.rds")

ret.mat = bag_rf_fit
plot(1, type="n", xlab="Number of Trees", ylab="1-RSquared", xlim=c(0,1000),ylim=c(0,.5))
abline(a=1-rsq_tree_train,b=0,lwd=2,lty=2)
abline(a=1-rsq_tree_test,b=0,lwd=2)
points(seq(1,1000,20),ret.mat[seq(1,1000,20),1],col="grey",cex=.5,pch=1)
points(seq(1,1000,20),ret.mat[seq(1,1000,20),2],col="black",cex=.5,pch=2)
#points(ret.mat[,3],col=3,cex=.5)
#points(ret.mat[,4],col=4,cex=.5)
legend(600, .21, legend=c("Bagging Train","Bagging Test"),#,"RF Train","RF Test"),
       col=c("grey","black"), cex=0.8,pch=c(1,2)) # col=c("black","red","blue",'green')
legend(600, .3, legend=c("RPart Train", "RPart Test"),
       col=c("black","black"), lty=c(1,2), cex=0.8)



ret.mat = bag_rf_fit
plot(1, type="n", xlab="Number of Trees", ylab="1-RSquared", xlim=c(0,1000),ylim=c(0,.5))
abline(a=1-rsq_tree_train,b=0,lwd=2,lty=2)
abline(a=1-rsq_tree_test,b=0,lwd=2)
points(seq(1,1000,20),ret.mat[seq(1,1000,20),1],col="grey",cex=.5,pch=1)
points(seq(1,1000,20),ret.mat[seq(1,1000,20),2],col="black",cex=.5,pch=2)
points(seq(1,1000,20),ret.mat[seq(1,1000,20),3],cex=.5,col="grey",pch=4)
points(seq(1,1000,20),ret.mat[seq(1,1000,20),4],cex=.5,col="black",pch=4)
legend(600, .21, legend=c("Bagging Train","Bagging Test","RF Train","RF Test"),
       col=c("grey","black","grey","black"),pch=c(2,2,4,4), cex=0.8) 
legend(600, .3, legend=c("RPart Train", "RPart Test"),
       col=c("black","black"), lty=c(1,2), cex=0.8)

