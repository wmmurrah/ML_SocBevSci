library(rpart.plot)
ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/ecls_DM.dat', na='.');
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

myvars = c('k_math_irt','g8_read','k_gk_irt')

ecls.2 = ecls.1[1:500,]

ids <- sample(1:nrow(ecls.2),0.5*nrow(ecls.2))
ecls.train = ecls.2[ids,myvars]
ecls.test = ecls.2[-ids,myvars]


big_tree <- rpart(g8_read ~ ., ecls.train,control=rpart.control(cp=0.00005))
printcp(big_tree)


cp1 = big_tree$cptable
#cp.rank <- rank(cp1[2:19,"CP"])
plot(1:nrow(cp1),cp1[1:nrow(cp1),"rel error"],ylim=c(0,1.1),
     type="p",xlab="Tree",ylab="Misfit",lwd=2,cex=.75)
lines(1:nrow(cp1),cp1[1:nrow(cp1),"xerror"],pch=2,lwd=2,type="p",cex=.75)

rsquareds <- rep(NA,nrow(cp1))
for(i in 1:nrow(cp1)){
  rsquareds[i] = 1-cor(predict(prune(big_tree,cp1[i,1]),ecls.test),ecls.test$g8_read)**2
}
rsquareds[1] = 1
lines(1:nrow(cp1),rsquareds,lwd=2,type=c("p"),pch=3,cex=.75)

arrows(1:nrow(cp1), cp1[1:nrow(cp1),"xerror"]-cp1[1:nrow(cp1),"xstd"],
       1:nrow(cp1), cp1[1:nrow(cp1),"xerror"]+cp1[1:nrow(cp1),"xstd"],
       length=0.05, angle=90, code=3)

legend(2,0.3,c("Train","CV","Test"),pch=c(1,2,3),cex=.5)
axis(3,seq(1,nrow(cp1),2),labels=cp1[1:nrow(cp1),"nsplit"][seq(1,nrow(cp1),2)])
mtext("Nsplit",side=3,line=2)

save.image("eclse_pruning.RData")
