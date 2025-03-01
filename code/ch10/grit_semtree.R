#grit1 = read.csv("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/data.csv",header=T,sep="")
#grit.kev = read.csv("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_Groups/semtree/data_kev.csv",header=T,sep=",")
grit.kev = read.csv("/Users/rjacobuc/Documents/GitHub/edm_book/ch10_Groups/semtree/data_kev.csv",header=T,sep=",")
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

library(lavaan);library(semtree)

mod.grit <- "
grit =~ GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12
"
cfa.out = cfa(mod.grit,grit.train)
summary(cfa.out,fit=T)


# for demonstration

demo.control =semtree.control(min.N=100,max.depth=2)
demo.tree = semtree(cfa.out,data.frame(grit.train),control=demo.control)


setwd('C:/Users/rjacobuc/Documents/GitHub/edm_book/Ch9_Groups/semtree')
#saveRDS(demo.tree,"demo_tree.rds")
demo.tree = readRDS("demo_tree.rds")
plot(demo.tree)


library(psych)

fa.out = fa(grit.train[,1:12],2)
summary(fa.out)
fa.out$loadings


mod.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
"
cfa.out2 = cfa(mod.grit2,grit.train)
summary(cfa.out2,fit=T)

semPlot::semPaths(cfa.out2,whatLabels="par")

library(semtree)

# use lavaan first for plotting

my.control=semtree.control(min.N=100,max.depth=4)

tree.out.lav <- semtree(cfa.out2,data.frame(grit.train),control=my.control)



#setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
#saveRDS(tree.out,"tree1.rds")

tree.out = readRDS("tree1.rds")
plot(prune(tree.out,1))

observed=colnames(grit.train[,1:12])
latents=c("f1","f2")
cfa2<-mxModel("Model", type="RAM",
               manifestVars=c(observed),
               latentVars=c("f1","f2"),
               mxPath(from="f1", to=c("GS2","GS3","GS5","GS7","GS8","GS9","GS11"), 
                      free=c(F,T,T,T,T,T,T),values=c(1,1,1,1,1,1,1),labels=c("l1","l2","l3","l4","l5","l6","l7")),
               mxPath(from="f2", to=c("GS1","GS4","GS6","GS8","GS9","GS10","GS11","GS12"), 
                      free=c(F,T,T,T,T,T,T,T),values=c(1,1,1,1,1,1,1,1),
                      labels=c("l11","l22","l33","l44","l55","l66","l77","l88")),
               mxPath(from = 'one', to = observed,values=0,labels=paste(paste("m"), 1:12,sep="")),
               mxPath(from=observed, arrows=2,labels=paste(paste("resid"), 1:12,sep="")),
               mxPath(from=latents, arrows=2,free=T,values=1,labels=c("v1","v2")),
              mxPath(from="f1",to="f2", arrows=2,free=T,values=1,labels=c("cov1")),
               mxData(grit.train[,1:12],type="raw")
)
out.cfa2 = mxRun(cfa2)
summary(out.cfa2)



my.control=semtree.control(min.N=200)

tree.out <- semtree(out.cfa2,data.frame(grit.train),control=my.control)

#setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
#saveRDS(tree.out,"tree1.rds")

tree.out = readRDS("tree1.rds")

tree.out

plot(tree.out)
plot(prune(tree.out,1))


# just variances
out.cfa2$output$estimate[c("v1","v2")]
parameters(tree.out)[c("v1","v2"),]

# rsquare
(rsq1 = 1 - parameters(tree.out)["INITIALIZED MODEL.S[13,13]",]/ .506)
(rsq2 = 1- parameters(tree.out)["INITIALIZED MODEL.S[14,14]",]/ .309)

sapply(semtree:::getNodeList(tree.out),function(x) {c(x$node_id,nrow(x$model$data$observed))})

mean(rsq1)
mean(rsq2)

semtree:::modelEstimates(tree.out)

my.control3=semtree.control(min.N=100,method="fair")

tree.out3 <- semtree(out.cfa2,data.frame(grit.train),control=my.control3)

parameters(tree.out3)

#setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
#saveRDS(tree.out3,"tree3.rds")

tree.out3 = readRDS("tree3.rds")
plot(tree.out3)

# only allow factor variance to vary
# to use focus.parameters, have to name all parameters

tree.out.focus <- semtree(out.cfa2,data.frame(grit.train),control=my.control3,
                          constraints=semtree.constraints(focus.parameters = c("v1","v2","cov1")))

setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
saveRDS(tree.out.focus,"tree_focus.rds")

tree.out.focus = readRDS("tree_focus.rds")

parameters(tree.out.focus)


# invariance


tree.out.inv <- semtree(out.cfa2,data.frame(grit.train),control=my.control3,
                          constraints=semtree.constraints(global.invariance=c("l1","l2","l3","l4","l5","l6","l7",
                                                                              "l11","l22","l33","l44","l55","l66","l77","l88",
                                                                              paste(paste("resid"), 1:12,sep=""),
                                                                              paste(paste("m"), 1:12,sep=""))))
tree.out.inv

setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
saveRDS(tree.out.inv,"tree_invariance.rds")

tree.out.inv = readRDS("tree_invariance.rds")

rsq1.inv = 1 - parameters(tree.out.inv)[1,]/ .494
rsq2.inv = 1 - parameters(tree.out.inv)[3,]/ .364


weighted.mean(rsq1.inv,c(228,370,402))
weighted.mean(rsq2.inv,c(228,370,402))





# try just factor means

grit.train.scale <- grit.train
grit.train.scale[,1:12] <- scale(grit.train[,1:12])


cfa3<-mxModel("Model", type="RAM",
              manifestVars=c(observed),
              latentVars=c("f1","f2"),
              mxPath(from="f1", to=c("GS2","GS3","GS5","GS7","GS8","GS9","GS11"), 
                     free=c(F,T,T,T,T,T,T),values=c(1,1,1,1,1,1,1),labels=c("l1","l2","l3","l4","l5","l6","l7")),
              mxPath(from="f2", to=c("GS1","GS4","GS6","GS8","GS9","GS10","GS11","GS12"), 
                     free=c(F,T,T,T,T,T,T,T),values=c(1,1,1,1,1,1,1,1),
                     labels=c("l11","l22","l33","l44","l55","l66","l77","l88")),
              mxPath(from = 'one', to = observed,values=0,
                     free=F,labels=paste(paste("m"), 1:12,sep="")),
              mxPath(from = 'one', to = latents,values=0,labels=c("lm1","lm2")),
              mxPath(from=observed, arrows=2,labels=paste(paste("resid"), 1:12,sep="")),
              mxPath(from=latents, arrows=2,free=T,values=1,labels=c("v1","v2")),
              mxPath(from="f1",to="f2", arrows=2,free=T,values=1,labels=c("cov1")),
              mxData(grit.train.scale[,1:12],type="raw")
)
out.cfa3 = mxRun(cfa3)
summary(out.cfa3)


tree.out.focus.means <- semtree(out.cfa3,data.frame(grit.train.scale),control=my.control3,
                                constraints=semtree.constraints(focus.parameters = c("lm1","lm2")))

tree.out.focus.means
plot(tree.out.focus.means)




tree.out.inv.means <- semtree(out.cfa3,data.frame(grit.train.scale),control=my.control3,
                        constraints=semtree.constraints(global.invariance=c("l1","l2","l3","l4","l5","l6","l7",
                                                                            "l11","l22","l33","l44","l55","l66","l77","l88",
                                                                            paste(paste("resid"), 1:12,sep="")
                                                                            ))) # paste(paste("m"), 1:12,sep="")

tree.out.inv.means
plot(tree.out.inv.means)

















# -------------------------------------------------

# sem forests

# ---------------------------------------------











control <- semforest.control()
control$semtree.control <- my.control3

control$num.trees <- 100 # number of trees to fit
control$sampling <- "subsample" # sampling process
control$mtry <- 20 # number of variables compared per node


semforest.out <- semforest(out.cfa2,data.frame(grit.train),control)

set.seed(436)
vim.forest1 <- varimp(semforest.out)

plot(vim.forest1)

setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
saveRDS(semforest.out,"forest.rds")


#control2 <- semforest.control()
#control2$semtree.control <- my.control3

#control2$num.trees <- 200 # number of trees to fit
#control2$sampling <- "subsample" # sampling process
#control2$mtry <- 2 # number of variables compared per node


#semforest.out2 <- semforest(out.cfa2,data.frame(grit.train),control2)

#set.seed(436)
#vim.subsample <- varimp(semforest.out2)

#plot(vim.subsample)


control3 <- semforest.control()
control3$semtree.control <- my.control3

control3$num.trees <- 500 # number of trees to fit
control3$sampling <- "subsample" # sampling process
control3$mtry <- 2 # number of variables compared per node


semforest.out3 <- semforest(out.cfa2,data.frame(grit.train),control3)

set.seed(436)
vim.subsample2 <- varimp(semforest.out3)

plot(vim.subsample2)

setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
saveRDS(semforest.out3,"forest2.rds")
