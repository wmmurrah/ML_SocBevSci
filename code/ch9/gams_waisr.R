# "Day3_MARS.r":
# Multivariate adaptive regression splines (MARS) with WAIS data 
# Kevin J. Grimm 2012/5/8

# Get Data
WAISR <- read.csv("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/scripts/waisr.csv", header = T)
head(WAISR)

# Load Package
require(mgcv)


out <- gam(pc_bd ~ s(yearsage,bs="ts",k=4), data=WAISR,gamma=1.4) # k sets max df
summary(out)
plot(out)
# produce a better plot


pred.data = data.frame(preds=out$fitted.values,x=WAISR$yearsage)


library(ggplot2)

pp = ggplot(data=WAISR, aes(x=yearsage, y=pc_bd)) +
  geom_point(color="grey") + theme_minimal() + labs(y="PC_BD",x="Age") + 
  geom_line(color='black',data = pred.data, aes(x=x, y=preds))

ggsave("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/gam.pdf",pp)


# can also run and tune using method="earth" in caret's train()
# see example http://uc-r.github.io/mars

# Multivariate 

MARS.BD.2 <- earth(pc_bd ~ yearsage + yearsed, degree=1,data=WAISR) #
summary(MARS.BD.2)
plotmo(MARS.BD.2)

MARS.BD.3 <- earth(pc_bd ~ yearsage + yearsed, degree=2) #
summary(MARS.BD.3)
plotmo(MARS.BD.3)

MARS.BD.4 <- earth(pc_bd ~ yearsage + yearsed, degree=3) #
summary(MARS.BD.4)
plotmo(MARS.BD.4)

# Penalty
# ---------------------------------------------------------------
# Generalized Cross Validation (GCV) penalty per knot. Default is if(degree>1)
#	3 else 2. A value of 0 penalizes only terms, not knots. 
#	The value -1 is treated specially to mean no penalty, so GCV=RSS/n. 
#	Theory suggests values in the range of about 2 to 4. 
#	In practice, for big data sets larger values can be useful to
#	force a smaller model. 
# ---------------------------------------------------------------

# Update the previous model, no penalty at all 
MARS.BD.5<-update(MARS.BD.4, penalty=-1)
summary(MARS.BD.5)

# More penalty for model complexity
MARS.BD.6<-update(MARS.BD.4, penalty=6)
summary(MARS.BD.6)
