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
                  'science','science_tht')

head(ecls.1)

myvars = c('k_math_irt','science','k_gk_irt')

ecls.2 = ecls.1[ ,myvars]
colnames(ecls.2) <- c("math","science","knowledge")

plot(ecls.2[,c(1,3)])

library(ggplot2)
attach(ecls.2)
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
qplot(math,knowledge,colour=science) +scale_color_gradient(low="black", high="grey")

#install.packages('tree')

library(rpart);library(tree)

attach(ecls.2)

tree.sci = rpart(science ~ ., data = ecls.2)
printcp(tree.sci)
tree.prune = prune(tree.sci,0.035)
rpart.plot::rpart.plot(tree.prune,box.palette="white")
summary(tree.sci)

library(partykit)
ecls.sub = ecls.2[1:200,]
ctree.out <- ctree(science ~ ., data=ecls.sub,control=ctree_control(mincriterion=0.99))
plot(ctree.out)

node.id = predict(ctree.out)  < 80
summary(ecls.sub[node.id,])

group = as.numeric(Hmisc::cut2(ecls.sub[node.id,]$science, g=4))
  
plot(ecls.sub[node.id,]$math,ecls.sub[node.id,]$knowledge,pch=group)


cor(predict(ctree.out),ecls.sub$science)**2


library(ggplot2)
plot.1 <- qplot(math,knowledge,colour=science) +scale_color_gradient(low="black", high="grey")
print(plot.1)
plot.2 = plot.1 + geom_segment(aes(x = 24.96, y = 5, xend = 24.96, yend = 50), color='black', size=2)
print(plot.2)
plot.3 = plot.2 + geom_segment(aes(x = 5, y = 16.41, xend = 24.96, yend = 16.41), color='black', size=2)
print(plot.3)
plot.4 = plot.3 + geom_segment(aes(x = 24.96, y = 28.5695, xend = 93.2, yend = 28.5695), color='black', size=2)
print(plot.4)

plot.5 = plot.4 + annotate("text", x = 7, y = 11, label = "71",size=11,color="black")
plot.6 = plot.5 + annotate("text", x = 13, y = 35, label = "83",size=11,color="black")
plot.7 = plot.6 + annotate("text", x = 60, y = 20, label = "90",size=11,color="black")
plot.8 = plot.7 + annotate("text", x = 60, y = 40, label = "97",size=11,color="black")
plot.8




# ------------------------------
# look at residuals
# ------------------------------

plot(scale(ecls.2$science - predict(tree.prune)))
qqnorm(scale(ecls.2$science -predict(tree.prune)))
qqline(scale(ecls.2$science - predict(tree.prune)))

lm.out <- lm(science ~ ., data = ecls.2)
plot(scale(ecls.2$science - predict(lm.out)))
qqnorm(scale(ecls.2$science -predict(lm.out)))
qqline(scale(ecls.2$science - predict(lm.out)))
plot(lm.out)


# compare R2

cor(predict(tree.prune),ecls.2$science)**2
summary(lm.out)




# ----------------------
# understand nonlinearity
# ----------------------

tree.sci2 = rpart(science ~ math, data = ecls.2)
plot(tree.sci2);text(tree.sci2)

plot(ecls.2$math,ecls.2$science,ylab="science",xlab="knowledge",col="grey")
lines(sort(ecls.2$math),sort(predict(tree.sci2)),lwd=5,col="black")


tree.sci3 = rpart(science ~ knowledge, data = ecls.2)
plot(tree.sci3);text(tree.sci3)
rpart.plot(tree.sci3,box.palette="white")

plot(ecls.2$knowledge,ecls.2$science,ylab="science",xlab="knowledge",col="grey")
lines(sort(ecls.2$knowledge),sort(predict(tree.sci3)),lwd=5,col="black")



# 3D
library(plotly)



mat = matrix(NA,45,93)

mat[1:16,1:24] <- 71
mat[17:45,1:24] <- 83
mat[1:29,25:93] <- 90
mat[30:45,25:93] <- 97

p2 = plot_ly(z=~mat,ylab="math",showscale=F,colors = colorRamp(c("grey","black"))) %>% add_surface() %>% layout(
  title = "Decision Surface",
  scene = list(
    xaxis = list(title = "Knowledge"),
    yaxis = list(title = "Math"),
    zaxis = list(title = "Predicted Science")
  )) 
p2



# can't currently install rgl
library(rgl)
knowledge = ecls.2[,3]
math = ecls.2[,1]
predicted.science = predict(tree.prune)
plot3d(knowledge,math,predicted.science)
planes3d(knowledge,math,predicted.science,type = "s", col = "red", size = 1)
#persp3d(ecls.2[,3],ecls.2[,1],predict(tree.prune))


# stability
# doesn't currently work
devtools::install_github("rjacobucci/dtree")
library(dtree)


stab.out = stable(science ~ ., data = ecls.2,methods=c("lm","rpart","ctree"),
                  samp.method="cv",tuneLength=c(1,15,3))
stab.out

library(xtable)
xtable(stab.out$means[,1:5])


## Dividing data into test set and training set ##
dim(ecls.2)
set.seed(10607)
train = sample(1:nrow(ecls.2), 2278) #50% of the sample size

ecls.train = ecls.2[train,]
ecls.test  = ecls.2[-train,]

# Build new tree based on training dataset
detach(ecls.2)

# Default Tree Control Parameters for tree package
# tree.control(nobs, mincut = 5, minsize = 10, mindev = 0.01)

cntltree= tree.control(nobs = 2278, minsize=1, mindev=0.002)

tree.sci.train = tree(science ~ ., data = ecls.train, control=cntltree)
plot(tree.sci.train)
text(tree.sci.train, pretty=0)

# Calculate MSE for training data and test data
mse.train = mean((ecls.train$science - predict(tree.sci.train))^2)
mse.train

r.square.train = cor(ecls.train$science, predict(tree.sci.train))^2
r.square.train

fitted.test = predict(tree.sci.train, newdata = ecls.test)

mse.test = mean((ecls.test$science - fitted.test)^2)
mse.test

r.square.test = cor(ecls.test$science, fitted.test)^2
r.square.test


## Cost Complexity Pruning using 10-fold Cross-Validation ##
cv.tree.train = cv.tree(tree.sci.train, K = 10)
cv.tree.train

plot(cv.tree.train)
plot(cv.tree.train$size, cv.tree.train$dev, type='b')

## Pruning is necessary - tree with 10 terminal nodes ##
pruned.sci.train = prune.tree(tree.sci.train, best = 10)

plot(pruned.sci.train)
text(pruned.sci.train, pretty=0)

## Examine MSE & R Square for Pruned Tree ##

mse.train.prune = mean((ecls.train$science - predict(pruned.sci.train))^2)
mse.train.prune

r.square.train.prune = cor(ecls.train$science, predict(pruned.sci.train))^2
r.square.train.prune

fitted.test.prune = predict(pruned.sci.train, newdata = ecls.test)

mse.test.prune = mean((ecls.test$science - fitted.test.prune)^2)
mse.test.prune

r.square.test.prune = cor(ecls.test$science, fitted.test.prune)^2
r.square.test.prune

###### Party ########

#install.packages('party')

library(party)

party.1 = ctree(science ~ ., data = ecls.2)
plot(party.1)
cor(predict(party.1), ecls.2$science)^2

cntl.parms = ctree_control(mincriterion = 0.9999, minsplit = 50, minbucket = 10)

party.2 = ctree(science ~ ., data = ecls.2, control=cntl.parms)
plot(party.2)

## Examing Cross Validation ##
party.3 = ctree(science ~ ., data=ecls.train)
plot(party.3)

r.square.train = cor(predict(party.3), ecls.train$science)^2
r.square.train

#0.422

test.party.3 = predict(party.3, newdata=ecls.test)
r.square.test = cor(test.party.3,ecls.test$science)^2
r.square.test

#0.360


## Inducing Control Parameters 
cntl.parms = ctree_control(mincriterion = 0.9999, minsplit = 50, minbucket = 10)

party.4 = ctree(science ~ ., data=ecls.train, control=cntl.parms)
plot(party.4)

r.square.train = cor(predict(party.4), ecls.train$science)^2
r.square.train

#0.368

test.party.4 = predict(party.4, newdata=ecls.test)
r.square.test = cor(test.party.4,ecls.test$science)^2
r.square.test

#0.358

######### rpart #########

#install.packages('rpart')

library(rpart)

rpart.1 = rpart(science~., data=ecls.2)
plot(rpart.1)
text(rpart.1)

rpart.1

#n= 4556 
#
#node), split, n, deviance, yval
#      * denotes terminal node
#
# 1) root 4556 987994.60 87.02949  
#   2) k_math_irt< 24.96 1874 458798.00 78.57069  
#     4) k_gk_irt< 16.41 655 184047.30 70.88995  
#       8) k_gk_irt< 10.7585 121  33618.41 61.09992 *
#       9) k_gk_irt>=10.7585 534 136203.90 73.10830 *
#     5) k_gk_irt>=16.41 1219 215347.10 82.69775  
#      10) k_gk_irt< 26.2045 948 162658.30 80.65232 *
#      11) k_gk_irt>=26.2045 271  34848.03 89.85299 *
#   3) k_math_irt>=24.96 2682 301418.40 92.93993  
#     6) k_gk_irt< 28.5695 1538 189754.80 89.82529  
#      12) k_math_irt< 32.515 1058 141793.20 87.93900 *
#      13) k_math_irt>=32.515 480  35899.75 93.98298 *
#     7) k_gk_irt>=28.5695 1144  76684.73 97.12727 *

cor(predict(rpart.1),ecls.2$science)^2

#0.37

#install.packages('rpart.plot')
library(rpart.plot)

prp(rpart.1) 

library(rattle)

fancyRpartPlot(rpart.1)

plotcp(rpart.1)

prune.1 = prune(rpart.1,0.016) 
fancyRpartPlot(prune.1)




# boosting


library(dismo)

ecls.3 <- data.frame(ecls.2,noise1 = rnorm(nrow(ecls.2)),
                     noise2 = rnorm(nrow(ecls.2)))

gbm.plan <- gbm.step(data=ecls.3,gbm.x=c(1,3,4,5),gbm.y=2,
                     family = "gaussian", tree.complexity = 5,
                     learning.rate = 0.01, bag.fraction = 0.5)
interactions = gbm.interactions(gbm.plan)

head(interactions$rank.list)