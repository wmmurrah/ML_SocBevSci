library(lavaan)

dat <- read.table("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/scripts/nlsy_math_wide_R.dat",na.strings=".")
dat <- read.table("/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/scripts/nlsy_math_wide_R.dat",na.strings=".")



colnames(dat) <-  c("c_id", "female", "lb_wght", "anti_k1",
                    "math2","math3","math4","math5","math6","math7","math8",
                    "age2","age3","age4","age5","age6","age7","age8",
                    "men2","men3","men4","men5","men6","men7","men8",
                    "spring2","spring3","spring4","spring5","spring6","spring7","spring8",
                    "anti2","anti3","anti4","anti5","anti6","anti7","anti8")

sum(is.na(dat[,c("math2","math3","math4","math5","math6","math7","math8")]) ==F)

table(rowSums(is.na(dat[,c("math2","math3","math4","math5","math6","math7","math8")])==F))

ntot <- nrow(dat)    # total number of observations
dat.sel <- dat[sample(ntot, 400), ]

dat.long.sel <- reshape(dat.sel, varying = c("math2","math3","math4","math5","math6","math7","math8"), v.names = "math",
                     times = 2:8, direction = "long")

dat.long.sel2 = dat.long.sel[,c("time","math","id")]
colnames(dat.long.sel2) <- c("Grade","Math","id")

library(ggplot2)
p <- ggplot(data = dat.long.sel2, aes(x = Grade, y = Math, group = id))
p + geom_line()


# do by age

ntot <- nrow(dat)    # total number of observations
dat.sel <- dat[sample(ntot, 100), ]

dat.long.sel11 <- reshape(dat.sel, varying = c("math2","math3","math4","math5","math6","math7","math8"), v.names = "math",
                        times = 2:8, direction = "long")
math.long = dat.long.sel11[,"math"]

dat.long.sel22 <- reshape(dat.sel, varying = c("age2","age3","age4","age5","age6","age7","age8"), v.names = "age",
                        times = 2:8, direction = "long")
age.long = round(dat.long.sel22[,"age"]/12)

dat.long.sel33 = data.frame(math.long,age.long,dat.long.sel11$id)
colnames(dat.long.sel33) = c("Math","Age","id")


library(ggplot2)
p <- ggplot(data = dat.long.sel33, aes(x = Age, y = Math, group = id))
p + geom_line()



# reshape to wide



dat.long.sel11 <- reshape(dat, varying = c("math2","math3","math4","math5","math6","math7","math8"), v.names = "math",
                          times = 2:8, direction = "long")
math.long = dat.long.sel11[,"math"]

dat.long.sel22 <- reshape(dat, varying = c("age2","age3","age4","age5","age6","age7","age8"), v.names = "age",
                          times = 2:8, direction = "long")
age.long = round(dat.long.sel22[,"age"]/12)

dat.long.sel33 = data.frame(math.long,age.long,dat.long.sel11$id)
colnames(dat.long.sel33) = c("Math","Age","id")


dat.wide <- reshape(dat.long.sel33, v.names = c("Math"), idvar="id",timevar="Age",
                           direction = "wide")



dat.wide2 = data.frame(dat.wide,dat[,c("female", "lb_wght", "anti_k1")])
dat.wide3 = subset(dat.wide2, select = -c(Math.NA,Math.15,Math.7) )
# linear growth curve model


# lcs model


# run w/ openmx -- lavaan has error with equality constraints



colnames(dat.wide3)[2:8] = c("Math_10","Math_8","Math_9","Math_11","Math_12","Math_13","Math_14")
baseModel <- mxModel(type="RAM", name = "baseModel",
                     mxData(observed=dat.wide3, type="raw" ),
                     manifestVars=c("Math_8","Math_9","Math_10",
                                    "Math_11","Math_12","Math_13","Math_14"),
                     latentVars=c("lm8","lm9","lm10","lm11","lm12","lm13","lm14",
                                  'd1',"d2","d3","d4","d5","d6",
                                  "slope"),
                     
                     # Defining true scores
                     mxPath(from=c("lm8","lm9","lm10","lm11","lm12","lm13","lm14"),
                            to=c("Math_8","Math_9","Math_10",
                                 "Math_11","Math_12","Math_13","Math_14"),
                            arrows=1, free=FALSE, values=1),
                     
                     # Autoregressive Effects 
                     mxPath(from=c("lm8","lm9","lm10","lm11","lm12","lm13"),
                            to=c("lm9","lm10","lm11","lm12","lm13","lm14"),
                            arrows=1, free=FALSE, values=1),
                     
                     # Defining change scores 
                     mxPath(from=c('d1',"d2","d3","d4","d5","d6"),
                            to=c("lm9","lm10","lm11","lm12","lm13","lm14"),
                            arrows=1, free=FALSE, values=1),
                     
                     # Constant change component 
                     mxPath(from='slope', to=c('d1',"d2","d3","d4","d5","d6"),
                            arrows=1, free=FALSE, values=1),
                     
                     # Variance paths
                     mxPath(from=c("Math_8","Math_9","Math_10",
                                   "Math_11","Math_12","Math_13","Math_14"),
                            arrows=2, free=TRUE, values=20, labels='sigma2_u'),
                     
                     # Latent variable covariance matrix path 
                     mxPath(from=c('lm8','slope'), 
                            arrows=2, connect='unique.pairs', free=TRUE, values=c(70, 0, 20),
                            labels=c('sigma2_1','sigma_21','sigma2_2')),
                     
                     # Proportional Effects 
                     mxPath(from=c("lm8","lm9","lm10","lm11","lm12","lm13"),
                            to=c('d1',"d2","d3","d4","d5","d6"),
                            arrows=1, free=TRUE, values=c(0),
                            labels=c('beta')),
                     
                     # means 
                     mxPath(from='one', to=c('lm8','slope'),
                            arrows=1, free=TRUE, values=c(30, 15), labels=c('mean_int','mean_slope'))
                     
) # close model

base.out <- mxRun(baseModel)
summary(base.out)

dat.wide4 = subset(dat.wide3, select = -c(id) )
tree.out.open <- semtree(base.out,dat.wide4)
plot(tree.out.open)


# plot trajectories

# group1
traj1 = rep(NA,7)
traj1[1] = 33.4
for(j in 2:7){
  traj1[j]= traj1[j-1] + 14.74 + -.233*traj1[j-1]
}

traj1 = data.frame(8:14,traj1,rep("Low",7),rep(888,7))
colnames(traj1) = c("Age","Math","node","id")


traj2 = rep(NA,7)
traj2[1] = 29.4
for(j in 2:7){
  traj2[j]= traj2[j-1] + 12.6 + -.196*traj2[j-1]
}

traj2 = data.frame(8:14,traj2,rep("High",7),rep(999,7))
colnames(traj2) = c("Age","Math","node","id")


trajs = rbind(traj1,traj2)
trajs$node = as.factor(trajs$node)

p <- ggplot(data = dat.long.sel33, aes(x = Age, y = Math, group = id))
p2 = p + geom_line()
p3 = p2 + geom_line(data=trajs,aes(x=Age,y=Math,color=node),size=3)
p3

ggsave("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/lcs_tree_traj.pdf",p3)


# -------------------------------------------------------------

# latent basis growth model

# ------------------------------------------------------------

model <- ' i =~ 1*Math.8 + 1*Math.9 + 1*Math.10 + 1*Math.11 + 1*Math.12 + 1*Math.13 + 1*Math.14
           s =~ 0*Math.8 + l2*Math.9 + l3*Math.10 + l4*Math.11 + l5*Math.12 + l6*Math.13 + 1*Math.14'
fit <- growth(model, data=dat.wide3,missing="fiml")
summary(fit)

semPlot::semPaths(fit)


library(semtree)
dat.wide4 = subset(dat.wide3, select = -c(id) )
tree.out.lgm <- semtree(fit,dat.wide4)
plot(tree.out.lgm)
