#install.packages('nlme')
#install.packages('devtools')
#install.packages('ggplot2')

require(devtools)
#install_github("Rjacobucci/longRPart2")
require(ggplot2)
require(nlme)
require(longRPart2)
library(glmertree)

## Loading Data ##

ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/scripts/ecls_20180130.dat', na.strings = '.')

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
          'momed','daded','ses','poor',
          'time','rd_th')

ecls.2 = ecls.1[,vars]

names(ecls.2) = c('id','gender','k_age','app_ln','slf_cnt','social','sad_lon','impuls','health',
                  'num_pk','time_pk','type_pk','lang','disabl',
                  'momed','daded','ses','poor',
                  'time','read')

## Plotting the Longitudinal Reading Data ##

# Plotting smaller group of participants #
ecls.2a = ecls.2[1001:1500,]

plot_obs <- ggplot(data=ecls.2a, aes(x=time, y=read, group=id)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(limits = c(0,2),  breaks = c(0,0.5,1,1.5,2), name = "Years in Since Beginning Kindergarten") + 
  scale_y_continuous(limits = c(-3,3), breaks = c(-3, -2, -1, 0, 1, 2, 3), name = "Reading Theta Score")

print(plot_obs)



# linear growth model with lme4
library(mgcv);library(gamm4)
linear.out = lmer(read~time + (time|id),ecls.2a)
summary(linear.out)
plot(linear.out)

gamm.out2 <- gamm(read ~ s(time,k=4),gamma=1.4,
                  random=list(id=~time),
                  data=ecls.2a)
gamm.out2
plot(gamm.out2$gam)
plot(gamm.out2$lme)



