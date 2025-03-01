grit.kev = read.csv("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_Groups/semtree/data_kev.csv",header=T,sep=",")
# set of models
# factor score for grit, use regression
# factor score for grit, use boosting
# sem model with predictors, latent variable for grit
# sem model with latent variable for predictors, latent variable for grit
# sem trees with grit as latent variable
# sem forests with grit as latent variable

colnames(grit.kev)

grit2 <- grit.kev[,c(3:14,31:92)]
grit2$familysize[grit2$familysize > 40] = NA


ind99 = grit2[,c(1:12,25:74)] == 0 & is.na(grit2[,c(1:12,25:74)]) ==F
grit2[,c(1:12,25:74)][ind99] <- NA

grit2$GS2  = 6 - grit2$GS2
grit2$GS3  = 6 - grit2$GS3
grit2$GS5  = 6 - grit2$GS5
grit2$GS7  = 6 - grit2$GS7
grit2$GS8  = 6 - grit2$GS8
grit2$GS11 = 6 - grit2$GS11


grit2 = data.matrix(grit2)
grit2[,74] = grit2[,74] - 2


grit3 = grit2[complete.cases(grit2),]

# take sample of 1000 for each
set.seed(1)
ids1 = sample(1:nrow(grit3),2000)
grit.train = grit3[ids1[1:1000],]
grit.test = grit3[ids1[1001:2000],]
grit.pop = grit3[-ids1,]

library(lavaan)



mimic.grit1 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS10+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
grit1 + grit2 ~ education+urban+gender+engnat+age+hand+religion+orientation+race+voted+married+familysize+
A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+
C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+
O1+O2+O3+O4+O5+O6+O7+O8+O9+O10+
E1+E2+E3+E4+E5+E6+E7+E8+E9+E10+
N1+N2+N3+N4+N5+N6+N7+N8+N9+N10
"
mimic.out1 = cfa(mimic.grit1,grit.train)
summary(mimic.out1,fit=T,rsquare=T,std=T)



library(bootnet)


net <- estimateNetwork(grit.train,default="EBICglasso")
plot(net)

net2 <- estimateNetwork(grit.train,default="ggmModSelect",stepwise=T)
plot(net2)

saveRDS(net2,"C:/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts/stepwise_network.rds")
