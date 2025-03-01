library(grpreg)


# real data


dat11 <- read.csv("C:/Users/rjacobuc/Dropbox/NSSI Frequency - Methods - SEM Tree/NSSI Frequency _ Methods - SEM Tree/FrequencyMethod_2.18.16.csv",
                  na.strings="")
dat11 <- read.csv("/Users/rjacobuc/Dropbox/NSSI Frequency - Methods - SEM Tree/NSSI Frequency _ Methods - SEM Tree/FrequencyMethod_2.18.16.csv",
                  na.strings="")

obsVars <- c("SBQR1","SBQR2","SBQR3","SBQR4","QIDS1_4","QIDS5","QIDS6_9","QIDS10","QIDS11","QIDS13","QIDS14","QIDS15_16",
             "MSIBPD1","MSIBPD3","MSIBPD4","MSIBPD5","MSIBPD6","MSIBPD7","MSIBPD8","MSIBPD9","MSIBPD10",
             "CTQ1","CTQ2r","CTQ3","CTQ4","CTQ5r","CTQ6","CTQ7r","CTQ8","CTQ9",
             "CTQ10","CTQ11","CTQ12r","CTQ13","CTQ14","CTQ15","CTQ16","CTQ17r","CTQ18",
             "CTQ19","CTQ20","CTQ21","CTQ22","CTQ23r","CTQ24r","CTQ25")

sub <- c(c("FREQNSSIacts","TotNSSImethods"),obsVars)
dat22 <- dat11[,sub]

dat22$methods <- ifelse(dat22$TotNSSImethods > 0,1,0)



dat33.4 <- dat22[,c("methods","SBQR1","SBQR2","SBQR3","SBQR4",
                    "QIDS1_4","QIDS5","QIDS6_9","QIDS10","QIDS11","QIDS13","QIDS14","QIDS15_16",
                    "MSIBPD1","MSIBPD3","MSIBPD4","MSIBPD5","MSIBPD6","MSIBPD7","MSIBPD8","MSIBPD9","MSIBPD10",
                    "CTQ1","CTQ2r","CTQ3","CTQ4","CTQ5r","CTQ6","CTQ7r","CTQ8","CTQ9",
                    "CTQ10","CTQ11","CTQ12r","CTQ13","CTQ14","CTQ15","CTQ16","CTQ17r","CTQ18",
                    "CTQ19","CTQ20","CTQ21","CTQ22","CTQ23r","CTQ24r","CTQ25")]
dat44.4 <- dat33.4[complete.cases(dat33.4),]

glm.out10.4 <- glm(methods ~ ., dat44.4,family=binomial)
summary(glm.out10.4)



# group lasso
# single level -- selection is by group
x2 <- dat44.4[,2:47]
y2 <- dat44.4[,1]
group2 <- c(rep("sb",4),rep("qids",8),rep("msi",9),rep("ctq",25))

fit2 <- grpreg(x2, y2, group2, penalty="grLasso",family="binomial")
select(fit2,"BIC")

# two level -- select within group as wells

fit3 <- grpreg(x2, y2, group2, penalty="gel",family="binomial")
select(fit3,"BIC")


# -----------------------------------------
# compare to hierarchical multiple regression
# -----------------------------------------

# ------------- first set


dat33.4 <- dat22[,c("methods","SBQR1","SBQR2","SBQR3","SBQR4",
                    "QIDS1_4","QIDS5","QIDS6_9","QIDS10","QIDS11","QIDS13","QIDS14","QIDS15_16",
                    "MSIBPD1","MSIBPD3","MSIBPD4","MSIBPD5","MSIBPD6","MSIBPD7","MSIBPD8","MSIBPD9","MSIBPD10",
                    "CTQ1","CTQ2r","CTQ3","CTQ4","CTQ5r","CTQ6","CTQ7r","CTQ8","CTQ9",
                    "CTQ10","CTQ11","CTQ12r","CTQ13","CTQ14","CTQ15","CTQ16","CTQ17r","CTQ18",
                    "CTQ19","CTQ20","CTQ21","CTQ22","CTQ23r","CTQ24r","CTQ25")]
dat1 <- dat33.4[complete.cases(dat33.4),]

glm.step1 <- glm(methods ~ SBQR1 + SBQR2 + SBQR3 + SBQR4, dat1,family=binomial)



#--------------- second set


glm.step2 <- glm(methods ~ SBQR1 + SBQR2 + SBQR3 + SBQR4 +
                 QIDS1_4+QIDS5+QIDS6_9+QIDS10+QIDS11+QIDS13+QIDS14+QIDS15_16,
                 dat1,family=binomial)


# -------------- third set


glm.step3 <- glm(methods ~ SBQR1 + SBQR2 + SBQR3 + SBQR4 +
                   QIDS1_4+QIDS5+QIDS6_9+QIDS10+QIDS11+QIDS13+QIDS14+QIDS15_16+
                   MSIBPD1+MSIBPD3+MSIBPD4+MSIBPD5+MSIBPD6+MSIBPD7+MSIBPD8+MSIBPD9+MSIBPD10,
                 dat1,family=binomial)


# ---------------- fourth set
# doesn't converge


glm.step4 <- glm(methods ~ SBQR1 + SBQR2 + SBQR3 + SBQR4 +
                   QIDS1_4+QIDS5+QIDS6_9+QIDS10+QIDS11+QIDS13+QIDS14+QIDS15_16+
                   MSIBPD1+MSIBPD3+MSIBPD4+MSIBPD5+MSIBPD6+MSIBPD7+MSIBPD8+MSIBPD9+MSIBPD10+
                   CTQ1+CTQ2r+CTQ3+CTQ4+CTQ5r+CTQ6+CTQ7r+CTQ8+CTQ9+
                 CTQ10+CTQ11+CTQ12r+CTQ13+CTQ14+CTQ15+CTQ16+CTQ17r+CTQ18+
                 CTQ19+CTQ20+CTQ21+CTQ22+CTQ23r+CTQ24r+CTQ25,
                 dat1,family=binomial)


# ---------------- compare models
# https://rcompanion.org/rcompanion/e_06.html
anova(glm.step1,glm.step2,glm.step3,glm.step4,test="Chisq")
summary(glm.step4)
c(glm.step1$aic,glm.step2$aic,glm.step3$aic,glm.step4$aic)

library(lmtest)
lrtest(glm.step1,glm.step2,glm.step3,glm.step4)

library(rcompanion)


c(nagelkerke(glm.step1)$Pseudo.R.squared[3],
  nagelkerke(glm.step2)$Pseudo.R.squared[3],
  nagelkerke(glm.step3)$Pseudo.R.squared[3],
  nagelkerke(glm.step4)$Pseudo.R.squared[3])
