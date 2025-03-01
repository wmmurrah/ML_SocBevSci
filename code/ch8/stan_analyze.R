diffuse = readRDS("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/stan/draws_grit_stan.rds")
nonlinear = readRDS("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/stan/draws_grit_stan_nonlinear.rds")
hyplasso = readRDS("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/stan/draws_grit_stan_nonlinear_hyplasso.rds")

diffuse = readRDS("/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts/draws_grit_stan.rds")
nonlinear = readRDS("/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts/draws_grit_stan_nonlinear.rds")
hyplasso = readRDS("/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts/draws_grit_stan_nonlinear_hyplasso.rds")



grit1 = read.csv("/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/data.csv",header=T,sep="")

#grit1 = read.csv("/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/data.csv",header=T,sep="")

grit1 = read.csv("data.csv",header=T,sep="")

# set of models
# factor score for grit, use regression
# factor score for grit, use boosting
# sem model with predictors, latent variable for grit
# sem model with latent variable for predictors, latent variable for grit
# sem trees with grit as latent variable
# sem forests with grit as latent variable

#levels(grit1$O10)

grit1$O10[grit1$O10 == "iOS" | grit1$O10 == "Linux"| grit1$O10 == "Macintosh" | grit1$O10 == "Windows"] = NA

grit2 <- grit1[complete.cases(grit1),c(3:14,31:92)]

grit2 = data.matrix(grit2)
grit2[,74] = grit2[,74] - 2

# take sample of 1000 for each
set.seed(1)
ids1 = sample(1:nrow(grit2),2000)
grit.train = grit2[ids1[1:1000],]
grit.test = grit2[ids1[1001:2000],]
grit.pop = grit2[-ids1,]


library(lavaan)


mimic.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
agree =~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10
consc =~ C1+C2+C3+C4+C5+C6+C7+C8+C9+C10
open =~ O1+O2+O3+O4+O5+O6+O7+O8+O9+O10
extra =~ E1+E2+E3+E4+E5+E6+E7+E8+E9+E10
neuro =~ N1+N2+N3+N4+N5+N6+N7+N8+N9+N10
grit1 + grit2 ~ agree+consc+open+extra+neuro+ education+urban+gender+engnat+age+hand+religion+orientation+race+voted+married+familysize
"
mimic.out2 = cfa(mimic.grit2,grit.train)
summary(mimic.out2,fit=T,rsquare=T,std=T)



# get stan results


names = parTable(mimic.out2)[66:99,c(2,4)]
pars.lavaan <- parTable(mimic.out2)[66:99,c("est","se")]
pars.mat <- data.frame(matrix(NA,34,4))

pars.diffuse = data.frame(summary(diffuse)$summary[183:216,c("mean","sd")])
pars.diffuse[1:12,1] = pars.diffuse[1:12,1]*-1 # reflect the representation of LV

pars.nonlinear = summary(nonlinear)$summary[183:244,c("mean","sd")]

pars.hyplasso = summary(hyplasso)$summary[183:244,c("mean","sd")]

names2 <- names[c(6:17,1:5,18:29,30:34),2]
names3 <- strtrim(names2[1:17],3)
names.grit1 <- paste0(names3,".1",sep="")
names.grit2 <- paste0(names3,".2",sep="")


library(ggplot2)



library(reshape)
means <- cbind(pars.lavaan[c(6:17,1:5,23:34,18:22),1], pars.diffuse[c(1:12,c(27,28,29,25,26),13:24,c(32,33,34,30,31)),1])
means.long = melt(means)
sd <- cbind(pars.lavaan[c(6:17,1:5,23:34,18:22),2], pars.diffuse[c(1:12,c(27,28,29,25,26),13:24,c(32,33,34,30,31)),2])
sd.long = melt(sd)
means.long$Method = c(rep("ML",34),rep("Bayes",34))
means.long$se = sd.long$value
library(ggplot2)

means.long$variable <- as.ordered(c(names.grit1,names.grit2,names.grit1,names.grit2))
#levels(means.long$variable) = c(names.grit1,names.grit2,names.grit1,names.grit2)

pl <- ggplot(means.long,aes(variable,value,col=Method)) + geom_point() + geom_errorbar(aes(ymin=value-(se*2),ymax=value+(se*2)))+ scale_color_grey(start=0.6, end=0.2)
pl +  theme_classic()+xlab("Predictor")+ylab("Estimate") + geom_abline(intercept=0,slope=0) +  theme(axis.text.x = element_text(angle = -45, hjust = 0))
#pl + theme(axis.text.x = element_text(angle = -45, hjust = 0)) +theme_minimal()+xlab("Predictor")+ylab("Estimate") + geom_abline(intercept=0,slope=0)
#pl2= pl+  facet_wrap(~Method)+geom_point(aes(x=variable,y=true),shape=8,size=2,colour='black')+theme_set(theme_grey(base_size = 22)) #increase text size
#pl2



# just latent coefficients



library(reshape)
means <- cbind(pars.nonlinear[25:62,1], pars.hyplasso[25:62,1])
means.long = melt(means)
sd <- cbind(pars.nonlinear[25:62,2], pars.hyplasso[25:62,2])
sd.long = melt(sd)
means.long$Method = c(rep("Normal(0,1)",38),rep("Hyperlasso",38))
means.long$se = sd.long$value
library(ggplot2)

#means.long$variable <- as.factor(means.long$X1)

nam = c("g1=E","g1=N","g1=A","g1=C","g1=O","g1=E*E","g1=N*N","g1=A*A","g1=C*C","g1=O*O","g1=E*N","g1=E*A",
"g1=E*C","g1=E*O","g1=N*A","g1=N*C","g1=N*O","g1=A*C","g1=C*O",
"g2=E","g2=N","g2=A","g2=C","g2=O","g2=E*E","g2=N*N","g2=A*A","g2=C*C","g2=O*O","g2=E*N","g2=E*A",
"g2=E*C","g2=E*O","g2=N*A","g2=N*C","g2=N*O","g2=A*C","g2=C*O")

means.long$variable = as.factor(nam)
#levels(means.long$variable) = nam


pl <- ggplot(means.long,aes(variable,value,col=Method)) + geom_point() + geom_errorbar(aes(ymin=value-(se*2),ymax=value+(se*2))) + scale_color_grey(start=0.6, end=0.2)
pl +  theme_classic()+xlab("Predictor")+ylab("Estimate") + geom_abline(intercept=0,slope=0) + theme(axis.text.x = element_text(angle = -45, hjust = 0))


# plot neuroticism quadratic effect
# didn't keep samples


nonlinear

nonlinear_full = readRDS("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/stan/draws_grit_stan_nonlinear_samples.rds")
nonlinear_full = readRDS("/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts/draws_grit_stan_nonlinear_samples.rds")

grit2 = row.names(summary(nonlinear_full)$summary)
conscientiousnous = summary(nonlinear_full)$summary[]

fs = summary(nonlinear_full)$summary[247:7246,"mean"]
fs.mat <- matrix(fs,1000,7,byrow=T)

alphas = summary(nonlinear_full)$summary[c(245,246),"mean"]
betas = summary(nonlinear_full)$summary[207:244,"mean"]

# conscientiousnous
xx = seq(-2,2,.01)
lines = alphas[2] + -.52*xx + .121*xx*xx
plot(fs.mat[,6],fs.mat[,2],xlab="Conscientiousness",ylab="Grit2")
lines(xx,lines)

# neuroticism
plot(fs.mat[,4],fs.mat[,1],xlab="Neuroticism",ylab="Grit1")
lines2 = alphas[1] + betas[2]*xx + betas[7]*xx*xx
lines(xx,lines2)
# openness

plot(fs.mat[,7],fs.mat[,2])
lines3 = alphas[2] + betas[5]*xx + betas[10]*xx*xx
lines(xx,lines3)
