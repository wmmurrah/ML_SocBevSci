
wisc <- read.table("/Users/rjacobuc/Documents/GitHub/EDM_Labs/2015/wisc4vpe.dat")

#wisc <- read.table(file.choose()) # use directory to find wisc4vpe.dat
names(wisc)<- c("V1","V2","V4","V6","P1","P2","P4", "P6", "Moeducat")

# install longRPart2 from source



wisc.verb <- wisc[,c(1:4,9)]

# create subset for plotting
ntot <- nrow(wisc.verb)    # total number of observations
wisc.verb.sel <- wisc.verb[sample(ntot, 30), ]

wisc.long <- reshape(wisc.verb, varying = c("V1", "V2", "V4", "V6"), v.names = "verbal",
                     times = c(1, 2, 4, 6), direction = "long")

wisc.long.sel <- reshape(wisc.verb.sel, varying = c("V1", "V2", "V4", "V6"),
                         v.names = "verbal", times = c(1, 2, 4, 6),
                         direction = "long")
head(wisc.long,3)
names(wisc.long)[2] <- "grade"
names(wisc.long.sel)[2] <- "grade"

write.csv(wisc.long,"/Users/rjacobuc/Desktop/wisc_long.csv")

wisc.long[,2] =  wisc.long[,2]-1

#wisc.long[,3] = wisc.long[,3] + c(rep(0,204),rep(10,204),rep(40,204),rep(100,204))

#wisc.long$grade <- as.factor(wisc.long$grade)

library(lattice)
xyplot(verbal ~ grade, groups = id, data = wisc.long.sel, type = "o", col = "black", 
       xlab = "Grade of Testing", ylab = "Verbal[t]")




library(lme4)
library(nlme)

# ---------------------------------
# generalized additive mixed models
# ---------------------------------


# ----- WAIS-R

# Get Data
WAISR <- read.csv("/Users/rjacobuc/Documents/GitHub/edm_book/ch9_longitudinal/scripts/waisr.csv", header = T)
head(WAISR)

waisr.long <- reshape(WAISR, timevar="yearsage",direction = "long",varying="pc_bd")

# Load Package
require(earth)

# add smoother for grade

gamm.out2 <- gamm(verbal ~ s(grade,k=4),gamma=1.4,
                  random=list(id=~grade),
                  data=wisc.long)



# -------------------------- other example



# don't think LRT is right, since DF is assuming the smooth function is actually linear
# Simon Wood book on p. 305 says open research topic, and bottom p.311

# regular mixed model

mix1 <- lme(fixed = verbal ~ grade, random = ~ grade | id, 
            data = wisc.long, method="ML" )
mix1

# specified to get same results
library(mgcv);library(gamm4)
gamm.out <- gamm(verbal ~ grade,#+ s(Moeducat),
                 # group = ~ id,
                  random=list(id=~grade),
                  data=wisc.long)
gamm.out
plot(gamm.out$lme)

# add smoother for grade

gamm.out2 <- gamm(verbal ~ s(grade,k=4),gamma=1.4,
                  random=list(id=~grade),
                  data=wisc.long)
gamm.out2
plot(gamm.out2$gam)
plot(gamm.out2$lme)

# compare to linear model
anova(gamm.out$lme,gamm.out2$lme)

# add in mother's education
# linear effect for grade, but smoother for mother's education
gamm.out3 <- gamm(verbal ~ grade + s(Moeducat,k=3),
                 # group = ~ id,
                 random=list(id=~grade),#gamma=1.4,
                 data=wisc.long)
gamm.out3

#ranef(gamm.out3$lme)

anova(gamm.out$lme,gamm.out2$lme,gamm.out3$lme)
plot(gamm.out3$gam)

vis.gam(gamm.out3$gam)

# dual gamm

gamm.out4 <- gamm(verbal ~ s(grade,k=4) + s(Moeducat,k=2),
                  gamma=1.4,
                  random=list(id=~grade),
                  data=wisc.long)
gamm.out4

anova(gamm.out$lme,gamm.out2$lme,gamm.out3$lme,gamm.out4$lme)
plot(gamm.out3$gam)


# finally, add an interaction

gamm.out5 <- gamm(verbal ~ s(grade,k=4) + s(Moeducat,k=2) + te(grade,Moeducat,k=3),
                  gamma=1.4,
                  random=list(id=~grade),
                  data=wisc.long)
gamm.out5
plot(gamm.out5$gam)


anova(gamm.out$lme,gamm.out2$lme,gamm.out3$lme,gamm.out4$lme,gamm.out5$lme)

gamm.out$gam
gamm.out4$gam # have to actually use this for df

# compare to longRPart2 results
# see if fits better than multiple group model
library(longRPart2)
lcart.mod1 <- lrp(method="lme",
                         fixedFormula=verbal ~ grade,
                         rPartFormula = ~ Moeducat,
                         group = ~ id,
                         randomFormula=~grade|id,
                         data=wisc.long)
 str(lcart.mod1)
 
plot(lcart.mod1)

# 6 df in base model, so longRPart2 model has 12 df
# this fits a lot worse than gamm.out5, similar gamm.out4
# use a lot of DF for multiple group model
# however, I don't think DF is calculated correctly for the GAMM
# might need to factor in EDF from the nonlinear function