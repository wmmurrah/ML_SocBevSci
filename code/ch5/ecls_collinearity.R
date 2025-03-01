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

myvars = c('k_math_irt','g8_sci','k_gk_irt')

ecls.2 = ecls.1[ ,myvars]
colnames(ecls.2) <- c("math","science","knowledge")

plot(ecls.2[,c(1,3)])

library(ggplot2)
attach(ecls.2)
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
qplot(math,knowledge,colour=science) +scale_color_gradient(low="black", high="grey")

#install.packages('tree')

library(rpart)

attach(ecls.2)

set.seed(1)
ecls.2$math.2 <- ecls.2$math + rnorm(nrow(ecls.2),1,3)


tree.sci = rpart(science ~ ., data = ecls.2)
printcp(tree.sci)
summary(tree.sci)
prp(tree.sci)
rpart.plot(tree.sci,box.palette="white")
