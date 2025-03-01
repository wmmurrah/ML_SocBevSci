
#ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/ecls_DM.dat', na='.');
ecls.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');
ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');

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

dat.comb <- data.frame(XX.std,YY)
colnames(dat.comb)[1:2] <- c("math","knowledge")
colnames(dat.comb)[26] <- "science"


dat.comb.sub <- dat.comb[sample(1:nrow(dat.comb),200),]

# https://www.kaggle.com/avehtari/bayesian-logistic-regression-with-rstanarm

library(rstanarm);library(rstan)
p = ncol(dat.comb.sub) - 1
p0 <- 4 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(nrow(dat.comb.sub))
hs_prior <- hs(df=1, global_df=1, global_scale=tau0)
stan.out <- stan_glm(science ~ ., data=dat.comb.sub,
                    family = gaussian(),
                    prior = hs_prior,
                    seed=1234)
stan.out

#traceplot(stan.out,"math")
summary(stan.out)

library(ggplot2);library(bayesplot)
color_scheme_set("gray")
plot(stan.out)
