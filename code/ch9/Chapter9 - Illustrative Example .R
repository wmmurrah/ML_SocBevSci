#install.packages('devtools')
#install.packages('ggplot2')
#install.packages('nlme')
#install.packages('longRPart2')
#install.packages('REEMtree')
#install.packages('glmertree')
#install.packages('lmmlasso')
#install.packages('glmmLasso')
#install.packages('miscTools')

library(devtools)

#install_github("Rjacobucci/longRPart2")

# have to install lmmlasso from CRAN archive
# # https://cran.r-project.org/src/contrib/Archive/lmmlasso/
# download, then in rstudio, install package from tar.gz

library(ggplot2)
library(nlme)
library(longRPart2)
library(REEMtree)
library(glmertree)
library(lmmlasso) 
library(glmmLasso)



## Loading Data ##

ecls.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch9_longitudinal/scripts/ecls_20180130.dat', na.strings = '.')

names(ecls.1) = c('id','gender','k_age','app_ln','slf_cnt','social','sad_lon','impuls','health',
                'num_pk','time_pk','type_pk','lang','disabl',
                'momed','daded','ses','poor','f_mtr','g_mtr',
                'occ','age','time',
                'rd_scl','rd_th','mt_scl','mt_th','gk_scl','gk_th')

summary(ecls.1)

## Selecting variables to be used in the analysis ## 

vars = c('id','gender','k_age','app_ln','slf_cnt','social','sad_lon','impuls','health',
          'num_pk','time_pk','type_pk','lang','disabl',
          'momed','daded','ses','poor','time','rd_th')

ecls.2 = ecls.1[,vars]

names(ecls.2) = c('id','gender','k_age','app_ln','slf_cnt','social','sad_lon','impuls','health',
                  'num_pk','time_pk','type_pk','lang','disabl',
                  'momed','daded','ses','poor',
                  'time','read')

ecls.3 = na.omit(ecls.2)

## Plotting the Longitudinal Reading Data ##

# Plotting smaller group of participants #
ecls.3a = ecls.3[1001:1500,]

plot_obs <- ggplot(data=ecls.3a, aes(x=time, y=read, group=id)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(limits = c(0,2),  breaks = c(0,0.5,1,1.5,2), name = "Years in Since Beginning Kindergarten") + 
  scale_y_continuous(limits = c(-3,3), breaks = c(-3, -2, -1, 0, 1, 2, 3), name = "Reading Theta Score")

print(plot_obs)

## Linear Mixed-Effects Model for Reading to obtain baseline information ##

ecls.3$time_c25 = ecls.3$time - 0.25

# Using lme
mem.1 = lme(read~time_c25, random=~time_c25|id, data=ecls.3, method="REML")
summary(mem.1)

# Linear mixed-effects model fit by REML
# Data: ecls.3 
# AIC      BIC    logLik
# 33068.92 33119.94 -16528.46
# 
# Random effects:
#   Formula: ~time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 0.5197833 (Intr)
# time_c25    0.1601310 -0.557
# Residual    0.2373447       
# 
# Fixed effects: read ~ time_c25 
# Value   Std.Error    DF   t-value p-value
# (Intercept) -0.8946276 0.005078606 24513 -176.1561       0
# time_c25     1.0325546 0.002644455 24513  390.4603       0
# Correlation: 
#   (Intr)
# time_c25 -0.489
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -4.4136594 -0.5011218  0.0151415  0.5066032  4.1833499 
# 
# Number of Observations: 36466
# Number of Groups: 11952

### Using nlme

#mem.2 = nlme(read ~ b0i + b1i * (time - 0.25),
#             data   =    ecls.2,
#             fixed  =    b0i+b1i~1,
#             random =    b0i+b1i~1,
#             group  =~   id,
#             start  =    c(-1,.5))
#summary(mem.2)

#### Longitudinal Recursive Partitioning with Linear Model ####

# Linear growth model within each partition 
lp2.m1 = lrp(method        =  "lme",
             fixedFormula  =  read ~ time_c25,
             randomFormula =~ 1 + time_c25|id,
             rPartFormula  =~ gender+app_ln+slf_cnt+social+sad_lon+impuls+health+
                              num_pk+time_pk+lang+disabl+momed+daded+poor,
             data=ecls.3,
             control=rpart.control(minsplit=500, cp=.00224))

### Using nlme

#lp2.m1 = lrp(method="nlme",
#             nlme.model=read~b0i+b1i*(time-0.25),
#             fixedFormula=b0i+b1i~1,
#             randomFormula=b0i+b1i~1,
#             group=~id,
#             rPartFormula=~gender+app_ln+slf_cnt+social+sad_lon+impuls+health+
#                           num_pk+time_pk+lang+disabl+momed+daded+poor,
#             data=ecls.2,
#             start=c(-1,.5),
#             control=rpart.control(minsplit=500, cp=.00224))

summary(lp2.m1)

# $importance
#       momed       daded        poor      gender      app_ln      impuls 
# 22104.33441 20442.25756 10523.86510  5267.23552  5015.21363  1924.66431 
#      health      social     time_pk      disabl      num_pk     slf_cnt 
#  1331.75014  1288.93426  1062.17857   327.30308   249.53758   125.04245 
#     sad_lon 
#    48.05293 
# 
# $parameters
#   node (Intercept)  time_c25     resid
# 1    3  -1.3116642 1.0579939 0.2693100
# 2    5  -1.2622413 1.1043456 0.2666100
# 3    7  -1.0504915 1.0625080 0.2518628
# 4    8  -0.9279389 1.0539521 0.2410861
# 5   11  -0.9498188 1.0560332 0.2369876
# 6   12  -0.7811486 1.0236316 0.2255627
# 7   13  -0.5986568 0.9730953 0.2163832
# 
# attr(,"class")
# [1] "summary.lrp"

names(lp2.m1)
lp2.m1$rpart_out
# 1) root 36466 33056.920 1.0325550  
#    2) daded< 4.5 16855 15662.160 1.0613110  
#       4) poor< 1.5 3154  3114.981 1.0579940 *
#       5) poor>=1.5 13701 12125.790 1.0628380  
#          10) momed< 2.5 1561  1533.931 1.1043460 *
#          11) momed>=2.5 12140 10419.970 1.0580860  
#             22) gender< 1.5 6067  5603.362 1.0625080 *
#             23) gender>=1.5 6073  4702.098 1.0539520 *
#    3) daded>=4.5 19611 15833.840 1.0068780  
#       6) momed< 5.5 10160  8345.322 1.0378900  
#          12) app_ln< 3.125 4419  3675.407 1.0560330 *
#          13) app_ln>=3.125 5741  4579.168 1.0236320 *
#       7) momed>=5.5 9451  7113.559 0.9730953 *
  
lp2.m1$var.corr

# [[1]]
# NULL
# 
# [[2]]
# NULL
# 
# [[3]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 1.4798788 (Intr)
# time_c25    0.4976023 0.115 
# Residual    1.0000000       
# 
# 
# [[4]]
# NULL
# 
# [[5]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 1.5068386 (Intr)
# time_c25    0.4825886 0.008 
# Residual    1.0000000       
# 
# 
# [[6]]
# NULL
# 
# [[7]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 1.7859316 (Intr)
# time_c25    0.6326099 -0.378
# Residual    1.0000000       
# 
# 
# [[8]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 1.8170272 (Intr)
# time_c25    0.4526967 -0.624
# Residual    1.0000000       
# 
# 
# [[9]]
# NULL
# 
# [[10]]
# NULL
# 
# [[11]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 1.9550578 (Intr)
# time_c25    0.6518855 -0.534
# Residual    1.0000000       
# 
# 
# [[12]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 2.2122639 (Intr)
# time_c25    0.7311873 -0.656
# Residual    1.0000000       
# 
# 
# [[13]]
# Random effects:
#   Formula: ~1 + time_c25 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 2.3531548 (Intr)
# time_c25    0.8230367 -0.693
# Residual    1.0000000     
saveRDS(lp2.m1,'/Users/rjacobuc/Documents/GitHub/edm_book/ch9_longitudinal/scripts/lrp_mod.rds')
plot(lp2.m1,box.palette="grey")
rpart.plot::rpart.plot(lp2.m1$rpart_out,box.palette="grey")

############ glmertree ############ 

glmt.1 = lmertree(read ~ time_c25 | (1 + time_c25 | id) | gender  + app_ln + slf_cnt + social + 
                                                          sad_lon + impuls + health  + num_pk +
                                                          time_pk + lang   + disabl  + momed  +
                                                          daded   + poor,
                  data     = ecls.3,
                  REML     = FALSE,
                  alpha    = .00000000000000000000001,#1 - .9999999999999999, 
                  minsplit = 3500)

#qchisq(.9999999999999999, 2)
#73.4736

names(glmt.1)

glmt.1$tree

# Linear model tree
# 
# Model formula:
#   read ~ time_c25 | gender + app_ln + slf_cnt + social + sad_lon + 
#   impuls + health + num_pk + time_pk + lang + disabl + momed + 
#   daded + poor
# 
# Fitted party:
#   [1] root
# |   [2] daded <= 5
# |   |   [3] poor <= 1
# |   |   |   [4] momed <= 2: n = 1277
# |   |   |       (Intercept)    time_c25 
# |   |   |         -1.368174    1.036585 
# |   |   |   [5] momed > 2: n = 2347
# |   |   |       (Intercept)    time_c25 
# |   |   |         -1.233667    1.081478 
# |   |   [6] poor > 1
# |   |   |   [7] momed <= 3
# |   |   |   |   [8] momed <= 2: n = 1740
# |   |   |   |       (Intercept)    time_c25 
# |   |   |   |         -1.264621    1.109893 
# |   |   |   |   [9] momed > 2
# |   |   |   |   |   [10] app_ln <= 2.75: n = 2784
# |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |         -1.132527    1.083492 
# |   |   |   |   |   [11] app_ln > 2.75
# |   |   |   |   |   |   [12] impuls <= 1.5: n = 2194
# |   |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |   |        -0.8964299   1.0522099 
# |   |   |   |   |   |   [13] impuls > 1.5: n = 2854
# |   |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |   |         -1.018688    1.065415 
# |   |   |   [14] momed > 3
# |   |   |   |   [15] gender <= 1
# |   |   |   |   |   [16] impuls <= 2
# |   |   |   |   |   |   [17] app_ln <= 3: n = 1985
# |   |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |   |        -0.9798827   1.0527073 
# |   |   |   |   |   |   [18] app_ln > 3: n = 2342
# |   |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |   |        -0.8312439   1.0390609 
# |   |   |   |   |   [19] impuls > 2: n = 1530
# |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |         -1.073976    1.062005 
# |   |   |   |   [20] gender > 1
# |   |   |   |   |   [21] momed <= 5
# |   |   |   |   |   |   [22] app_ln <= 2.75: n = 1038
# |   |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |   |        -0.9673021   1.0470586 
# |   |   |   |   |   |   [23] app_ln > 2.75: n = 3225
# |   |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |   |        -0.8210593   1.0370466 
# |   |   |   |   |   [24] momed > 5: n = 1491
# |   |   |   |   |       (Intercept)    time_c25 
# |   |   |   |   |        -0.6723101   0.9792618 
# |   [25] daded > 5
# |   |   [26] momed <= 5
# |   |   |   [27] app_ln <= 3: n = 1658
# |   |   |       (Intercept)    time_c25 
# |   |   |        -0.8640312   1.0439647 
# |   |   |   [28] app_ln > 3: n = 2356
# |   |   |       (Intercept)    time_c25 
# |   |   |        -0.6882554   0.9981962 
# |   |   [29] momed > 5
# |   |   |   [30] app_ln <= 2.75: n = 1861
# |   |   |       (Intercept)    time_c25 
# |   |   |        -0.7002764   0.9900590 
# |   |   |   [31] app_ln > 2.75
# |   |   |   |   [32] momed <= 7: n = 3982
# |   |   |   |       (Intercept)    time_c25 
# |   |   |   |        -0.5737962   0.9761490 
# |   |   |   |   [33] momed > 7: n = 1802
# |   |   |   |       (Intercept)    time_c25 
# |   |   |   |        -0.4159418   0.9175546 
# 
# Number of inner nodes:    16
# Number of terminal nodes: 17
# Number of parameters per node: 2
# Objective function (residual sum of squares): 1276.426

plot(glmt.1, which = "tree",observed=T,fitted="none",drop_terminal=T) 

round(coef(glmt.1),2)
#    (Intercept)  time_c25
# 4   -1.3681737 1.0365853
# 5   -1.2336669 1.0814777
# 8   -1.2646215 1.1098928
# 10  -1.1325270 1.0834921
# 12  -0.8964299 1.0522099
# 13  -1.0186884 1.0654154
# 17  -0.9798827 1.0527073
# 18  -0.8312439 1.0390609
# 19  -1.0739764 1.0620045
# 22  -0.9673021 1.0470586
# 23  -0.8210593 1.0370466
# 24  -0.6723101 0.9792618
# 27  -0.8640312 1.0439647
# 28  -0.6882554 0.9981962
# 30  -0.7002764 0.9900590
# 32  -0.5737962 0.9761490
# 33  -0.4159418 0.9175546

VarCorr(glmt.1)

# Groups   Name        Std.Dev. Corr  
# id       (Intercept) 0.45880        
#          time_c25    0.15283  -0.520
# Residual             0.23744


############ REEMtree ############ 
ret.1 = REEMtree(read ~ time_c25 + 
                        gender  + app_ln + slf_cnt + social + 
                        sad_lon + impuls + health  + num_pk +
                        time_pk + lang   + disabl  + momed  +
                        daded   + poor,
                        data = ecls.3,
                        random = ~ 1 | id,
                        method = "ML")
ret.1


# [1] "*** RE-EM Tree ***"
# n= 36466 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 36466 16001.12000 -0.30273470  
#  2) time_c25< 0.875 25485  4735.33700 -0.65229520  
#   4) time_c25< 0.265 11545   984.38550 -1.02360900  
#     8) daded< 5.5 7821   454.50770 -1.13617000 *
#     9) daded>=5.5 3724   222.43020 -0.78725520 *
#   5) time_c25>=0.265 13940   840.91550 -0.34477590  
#     10) daded< 5.5 9518   442.55190 -0.43242280  
#       20) time_c25< 0.68 8010   322.90660 -0.46977620 *
#       21) time_c25>=0.68 1508    48.96003 -0.23421640 *
#     11) daded>=5.5 4422   167.86790 -0.15612290  
#       22) time_c25< 0.565 3730   119.60870 -0.19144810 *
#       23) time_c25>=0.565 692    18.74574  0.03340346 *
#  3) time_c25>=0.875 10981   924.47680  0.50853450  
#    6) time_c25< 1.135 861    37.87261 -0.04365325 *
#    7) time_c25>=1.135 10120   601.47300  0.55553620  
#      14) daded< 5.5 6924   333.50000  0.47984130 *
#      15) daded>=5.5 3196   141.99900  0.71962570 *
#   [1] "Estimated covariance matrix of random effects:"
# (Intercept)
# (Intercept)   0.1987158
# [1] "Estimated variance of errors: 0.065733377524669"
# [1] "Log likelihood:  -15863.47597525"

names(ret.1)

ret.1$ErrorVariance
#[1] 0.06573338

ret.1$EffectModel

# Linear mixed-effects model fit by maximum likelihood
# Data: newdata 
# Subset: SubsetVector 
# Log-likelihood: -15863.48
# Fixed: formula(paste(c(toString(TargetName), "as.factor(nodeInd)"),      collapse = "~")) 
# (Intercept)  as.factor(nodeInd)5  as.factor(nodeInd)8  as.factor(nodeInd)9 
# -1.1361704            0.3489153            0.6663942            0.9019541 
# as.factor(nodeInd)11 as.factor(nodeInd)12 as.factor(nodeInd)14 as.factor(nodeInd)16 
# 0.9447223            1.1695739            1.0925172            1.6160117 
# as.factor(nodeInd)17 
# 1.8557961 
# 
# Random effects:
#   Formula: ~1 | id
# (Intercept)  Residual
# StdDev:   0.4457756 0.2563852
# 
# Number of Observations: 36466
# Number of Groups: 11952

plot(ret.1,text=F)


#####################################################
#####################################################
#                 Regularization                    #
#####################################################
#####################################################

### glmmLasso Package

ecls.4 = ecls.3
ecls.4$gender  = scale(ecls.3$gender)
ecls.4$app_ln  = scale(ecls.3$app_ln)
ecls.4$slf_cnt = scale(ecls.3$slf_cnt)
ecls.4$social  = scale(ecls.3$social)
ecls.4$sad_lon = scale(ecls.3$sad_lon)
ecls.4$impuls  = scale(ecls.3$impuls)
ecls.4$health  = scale(ecls.3$health)
ecls.4$num_pk  = scale(ecls.3$num_pk)
ecls.4$time_pk = scale(ecls.3$time_pk)
ecls.4$lang    = scale(ecls.3$lang)
ecls.4$disabl  = scale(ecls.3$disabl)
ecls.4$momed   = scale(ecls.3$momed)
ecls.4$daded   = scale(ecls.3$daded)
ecls.4$poor    = scale(ecls.3$poor)

head(ecls.4)

#glmm.1 = glmmLasso(read ~ time_c25 + 
#                          gender   + app_ln + slf_cnt + social + 
#                          sad_lon  + impuls + health  + num_pk +
#                          time_pk  + lang   + disabl  + momed  +
#                          daded    + poor, 
#                   rnd     = list(id =~ 1 + time_c25), 
#                   lambda  = 1,
#                   data    = ecls.4, 
#                   control = list(index=c(NA, 1, 2, 3, 4, 5,
#                                              6, 7, 8, 9,10,
#                                             11,12,13,14),
#                   method  = "REML"))



############# Incorporating Predictors with the Lasso #############

###### Creating product terms to include effects on slope 
ecls.4$gender.t  = ecls.4$gender*ecls.4$time_c25
ecls.4$app_ln.t  = ecls.4$app_ln*ecls.4$time_c25
ecls.4$slf_cnt.t = ecls.4$slf_cnt*ecls.4$time_c25
ecls.4$social.t  = ecls.4$social*ecls.4$time_c25
ecls.4$sad_lon.t = ecls.4$sad_lon*ecls.4$time_c25
ecls.4$impuls.t  = ecls.4$impuls*ecls.4$time_c25
ecls.4$health.t  = ecls.4$health*ecls.4$time_c25
ecls.4$num_pk.t  = ecls.4$num_pk*ecls.4$time_c25
ecls.4$time_pk.t = ecls.4$time_pk*ecls.4$time_c25
ecls.4$lang.t    = ecls.4$lang*ecls.4$time_c25
ecls.4$disabl.t  = ecls.4$disabl*ecls.4$time_c25
ecls.4$momed.t   = ecls.4$momed*ecls.4$time_c25
ecls.4$daded.t   = ecls.4$daded*ecls.4$time_c25
ecls.4$poor.t    = ecls.4$poor*ecls.4$time_c25

######## lmmlasso

## Note that outcome, predictors, and random effects are all separate matrices
#vars = c('id','time_c25','read',
#         'gender'   ,'app_ln'  ,'slf_cnt'  ,'social' 
#         'sad_lon'  ,'impuls'  ,'health'   ,'num_pk',
#         'time_pk'  ,'lang'    ,'disabl'   ,'momed',
#         'daded'    ,'poor',
#         'gender.t' ,'app_ln.t','slf_cnt.t','social.t' 
#         'sad_lon.t','impuls.t','health.t' ,'num_pk.t',
#         'time_pk.t','lang.t'  ,'disabl.t' ,'momed.t',
#         'daded.t'  ,'poor.t')

#ecls.5 = ecls.4[,vars]

Y = as.matrix(ecls.4$read)
X = as.matrix(cbind(rep(1,nrow(ecls.4)), ecls.4$time_c25,
                    ecls.4$gender,       ecls.4$app_ln,   ecls.4$slf_cnt,   ecls.4$social, 
                    ecls.4$sad_lon,      ecls.4$impuls,   ecls.4$health,    ecls.4$num_pk,
                    ecls.4$time_pk,      ecls.4$lang,     ecls.4$disabl,    ecls.4$momed,
                    ecls.4$daded,        ecls.4$poor,
                    ecls.4$gender.t,     ecls.4$app_ln.t, ecls.4$slf_cnt.t, ecls.4$social.t, 
                    ecls.4$sad_lon.t,    ecls.4$impuls.t, ecls.4$health.t,  ecls.4$num_pk.t,
                    ecls.4$time_pk.t,    ecls.4$lang.t,   ecls.4$disabl.t,  ecls.4$momed.t,
                    ecls.4$daded.t,      ecls.4$poor.t))
Z = X[,1:2]

#### Beginning with Regularizing Effects on the Intercept

# trial with no penalty
lmm.1 = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                 weights=c(NA,NA,rep(1,14),rep(NA,14)), 
                 lambda = 0, 
                 pdMat='pdSym', 
                 method="ML", 
                 startValue = 0)

summary(lmm.1)

# trial with larger penalty
lmm.2 = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                 weights=c(NA,NA,rep(1,14),rep(NA,14)), 
                 lambda = 15000, 
                 pdMat='pdSym', 
                 method="ML", 
                 startValue = 0)
summary(lmm.2)

### Let's do a loop with different penalties ###

lx
lmm.lambda   = matrix(NA,31,1)
lmm.deviance = matrix(NA,31,1)
lmm.npar     = matrix(NA,31,1)

for(i in 1:31){
  lmm.lambda[i]   = (i-1)*500
  lmm.lasso       = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                             weights=c(NA,NA,rep(1,14),rep(NA,14)), 
                             lambda=lmm.lambda[i], 
                             pdMat='pdSym', 
                             method="ML", 
                             startValue = 0)
  lmm.deviance[i] = lmm.lasso$deviance
  lmm.npar[i]     = lmm.lasso$npar
}

# BIC
lmm.bic = lmm.deviance + log(nrow(ecls.4)) * lmm.npar
which(lmm.bic==min(lmm.bic))
#[1] - lambda = 0

# BIC 500
lmm.deviance.1 = lmm.deviance/(nrow(ecls.4))
lmm.deviance.500 = lmm.deviance.1*500
lmm.deviance.bic.500 = lmm.deviance.500 + log(500) * lmm.npar
lmm.deviance.bic.500
#           [,1]
#  [1,] 631.0560
#  [2,] 619.2067
#  [3,] 607.3671
#  [4,] 603.1540
#  [5,] 599.7086
#  [6,] 601.5970
#  [7,] 585.0763
#  [8,] 581.1023
#  [9,] 583.6057
# [10,] 586.5675
# [11,] 576.5423
# [12,] 578.4615
# [13,] 580.6355
# [14,] 583.0922
# [15,] 585.8627
# [16,] 589.0016
# [17,] 592.5640
# [18,] 596.6257
# [19,] 601.3083
# [20,] 590.8208
# [21,] 590.8208
# [22,] 590.8208
# [23,] 590.8208
# [24,] 590.8208
# [25,] 590.8208
# [26,] 590.8208
# [27,] 590.8208
# [28,] 590.8208
# [29,] 590.8208
# [30,] 590.8208
# [31,] 590.8208

which(lmm.deviance.bic.500==min(lmm.deviance.bic.500))
#[11] - lambda = 5000

plot(lmm.deviance.bic.500)

lmm.3 = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                 weights=c(NA,NA,rep(1,14),rep(NA,14)), 
                 lambda = 5000, 
                 pdMat='pdSym', 
                 method="ML", 
                 startValue = 0)
summary(lmm.3)

# Model fitted by ML for lambda = 5000 :
#   AIC       BIC    logLik  deviance objective 
# 32121.0   32308.1  -16038.5   32077.0   16709.4 
# 
# Random effects: pdSym 
# Variance   Std.Dev.
# X1       0.19137010 0.43745869
# X2       0.00827975 0.09099313
# Residual 0.06355501 0.25210118
# 
# Random effects: Correlations 
# (Intercept)       X2
# (Intercept)    1.000000 0.020465
# X2             0.020465 1.000000
# 
# Fixed effects: 
#   |active set|= 18 
# Estimate    
# (Intercept) -0.8917213733 (n)
# X2           1.0310931030 (n)
# X14          0.0644321967    
# X15          0.0697526114    
# X17          0.0002051823 (n)
# X18          0.0048644286 (n)
# X19         -0.0002621623 (n)
# X20          0.0034929752 (n)
# X21          0.0018282806 (n)
# X22         -0.0040659754 (n)
# X23         -0.0037752270 (n)
# X24          0.0015158319 (n)
# X25         -0.0102409220 (n)
# X26         -0.0070587547 (n)
# X27          0.0076268367 (n)
# X28         -0.0183058674 (n)
# X29         -0.0137156125 (n)
# X30          0.0128258155 (n)
# 
# Number of iterations: 17


# Examining where minimum is most likely and then refining search 
for(i in 1:31){
  lmm.lambda[i]   = 3500 + (i-1)*100
  lmm.lasso       = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                             weights=c(NA,NA,rep(1,14),rep(NA,14)), 
                             lambda=lmm.lambda[i], 
                             pdMat='pdSym', 
                             method="ML", 
                             startValue = 0)
  lmm.deviance[i] = lmm.lasso$deviance
  lmm.npar[i]     = lmm.lasso$npar
}

lmm.deviance.1 = lmm.deviance/(nrow(ecls.4))
lmm.deviance.500 = lmm.deviance.1*500
lmm.deviance.bic.500 = lmm.deviance.500 + log(500) * lmm.npar
lmm.deviance.bic.500
#          [,1]
#  [1,] 581.1023
#  [2,] 581.5686
#  [3,] 582.0529
#  [4,] 582.5528
#  [5,] 583.0699
#  [6,] 583.6057
#  [7,] 584.1594
#  [8,] 584.7321
#  [9,] 585.3245
# [10,] 585.9359
# [11,] 586.5675
# [12,] 581.7020
# [13,] 581.4916
# [14,] 582.0408
# [15,] 576.9556
# [16,] 576.5423
# [17,] 576.9069
# [18,] 577.2808
# [19,] 577.6646
# [20,] 578.0580
# [21,] 578.4615
# [22,] 578.8751
# [23,] 579.3004
# [24,] 580.4716
# [25,] 580.1790
# [26,] 580.6355
# [27,] 581.1031
# [28,] 581.5823
# [29,] 582.0728
# [30,] 582.5764
# [31,] 583.0922

which(lmm.deviance.bic.500==min(lmm.deviance.bic.500))
#[16] - lambda = 5000

plot(lmm.deviance.bic.500)

### Penalizing slope effects while retaining effects to the intercept

## Updated X matrix to retain effects on the intercept
X = as.matrix(cbind(rep(1,nrow(ecls.4)), ecls.4$time_c25,
                    ecls.4$daded,        ecls.4$poor,
                    ecls.4$gender.t,     ecls.4$app_ln.t, ecls.4$slf_cnt.t, ecls.4$social.t, 
                    ecls.4$sad_lon.t,    ecls.4$impuls.t, ecls.4$health.t,  ecls.4$num_pk.t,
                    ecls.4$time_pk.t,    ecls.4$lang.t,   ecls.4$disabl.t,  ecls.4$momed.t,
                    ecls.4$daded.t,      ecls.4$poor.t))

# trial with no penalty
lmm.1 = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                 weights=c(NA,NA,rep(NA,2),rep(1,14)), 
                 lambda = 0, 
                 pdMat='pdSym', 
                 method="ML", 
                 startValue = 0)

summary(lmm.1)

# trial with larger penalty
lmm.2 = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                 weights=c(NA,NA,rep(NA,2),rep(1,14)), 
                 lambda = 8000, 
                 pdMat='pdSym', 
                 method="ML", 
                 startValue = 0)
summary(lmm.2)

### Let's do a loop with different penalties ###

lmm.lambda   = matrix(NA,33,1)
lmm.deviance = matrix(NA,33,1)
lmm.npar     = matrix(NA,33,1)

for(i in 1:33){
  lmm.lambda[i]   = (i-1)*250
  lmm.lasso       = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                             weights=c(NA,NA,rep(NA,2),rep(1,14)), 
                             lambda=lmm.lambda[i], 
                             pdMat='pdSym', 
                             method="ML", 
                             startValue = 0)
  lmm.deviance[i] = lmm.lasso$deviance
  lmm.npar[i]     = lmm.lasso$npar
}

# BIC
lmm.bic = lmm.deviance + log(nrow(ecls.4)) * lmm.npar
which(lmm.bic==min(lmm.bic))
#[5] - lambda = 1000

# BIC 500
lmm.deviance.1 = lmm.deviance/(nrow(ecls.4))
lmm.deviance.500 = lmm.deviance.1*500
lmm.deviance.bic.500 = lmm.deviance.500 + log(500) * lmm.npar
lmm.deviance.bic.500

#           [,1]
#  [1,] 639.5035
#  [2,] 619.2882
#  [3,] 594.3439
#  [4,] 540.0738
#  [5,] 521.5714
#  [6,] 515.5043
#  [7,] 515.6444
#  [8,] 515.8104
#  [9,] 516.0022
# [10,] 503.7166
# [11,] 497.6056
# [12,] 497.7002
# [13,] 497.8039
# [14,] 497.9170
# [15,] 491.8215
# [16,] 491.9287
# [17,] 492.0436
# [18,] 492.1662
# [19,] 492.2965
# [20,] 492.4346
# [21,] 492.5807
# [22,] 492.7347
# [23,] 492.8967
# [24,] 493.0668
# [25,] 493.2450
# [26,] 493.4315
# [27,] 493.6264
# [28,] 493.8296
# [29,] 487.7661
# [30,] 487.7661
# [31,] 487.7661
# [32,] 487.7661
# [33,] 487.7661

plot(lmm.deviance.bic.500)


lmm.lambda   = matrix(NA,91,1)
lmm.deviance = matrix(NA,91,1)
lmm.npar     = matrix(NA,91,1)

for(i in 1:91){
  lmm.lambda[i]   = 2500 + (i-1)*50
  lmm.lasso       = lmmlasso(x = X, y = Y, z = Z, grp=ecls.4$id, 
                             weights=c(NA,NA,rep(NA,2),rep(1,14)), 
                             lambda=lmm.lambda[i], 
                             pdMat='pdSym', 
                             method="ML", 
                             startValue = 0)
  lmm.deviance[i] = lmm.lasso$deviance
  lmm.npar[i]     = lmm.lasso$npar
}

# BIC
lmm.bic = lmm.deviance + log(nrow(ecls.4)) * lmm.npar
which(lmm.bic==min(lmm.bic))
#[1] - lambda = 2500

# BIC 500
lmm.deviance.1 = lmm.deviance/(nrow(ecls.4))
lmm.deviance.500 = lmm.deviance.1*500
lmm.deviance.bic.500 = lmm.deviance.500 + log(500) * lmm.npar
lmm.deviance.bic.500
which(lmm.deviance.bic.500==min(lmm.deviance.bic.500))
plot(lmm.deviance.bic.500)


summary(ecls.3$poor)

ecls.3$poor  = ecls.3$poor - 1
ecls.3$daded = ecls.3$daded - 1

mem.2 = lme(read ~ time_c25 + daded + poor, 
            random =~ time_c25|id, 
            data=ecls.3, 
            method="REML")

summary(mem.2)

