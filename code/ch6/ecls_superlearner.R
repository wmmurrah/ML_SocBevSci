library(rpart.plot)

ecls.1 = read.csv('/Users/rjacobuc/Documents/GitHub/edm_book/ch4_regression/analysis/ecls_syn.csv', na='.');

names(ecls.1) = c("X",'gender','kage',
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

ecls.2 = ecls.1[1:500,myvars]
colnames(ecls.2) <- c("math","science","knowledge",'weight','height','income','ses','read')

plot(ecls.2[,c(1,3)])

library(caret)


rf.out <- caret::train(science ~ ., ecls.2,method="rf",tuneLength=3,ntree=500,importance=T)

library(xtable)
vi.rf = varImp(rf.out)
#xtable(vi.rf$importance)



# super learner

library(SuperLearner)
# https://github.com/ecpolley/SuperLearner
#vignette(package = "SuperLearner")



set.seed(1)

sl_lib = c("SL.xgboost", "SL.randomForest", "SL.glmnet")

result = SuperLearner(Y = ecls.2$science, X = ecls.2[,-2], SL.library = sl_lib)
result

result$Z
result$cvRisk

# nested cross-validation
result2 = CV.SuperLearner(Y = ecls.2$science, X = ecls.2[,-2], SL.library = sl_lib)
result2
plot(result2) + theme_minimal()
