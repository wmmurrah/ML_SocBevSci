# "Day3_MARS.r":
# Multivariate adaptive regression splines (MARS) with WAIS data 
# Kevin J. Grimm 2012/5/8

# Get Data
WAISR <- read.csv("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/scripts/waisr.csv", header = T)
head(WAISR)

# Load Package
require(earth)

# Specify the model using function "earth"
MARS.BD.1<-earth(pc_bd ~ yearsage, data = WAISR)
summary(MARS.BD.1)

# Write Out Model
# pc_bd = 60.77 - .57*(MAX(0,yearsage - 29.5))

# Two ways to get the fitted values
MARS.BD.1$fitted.values
# Or,
predict(MARS.BD.1)

# Two ways to look up the design matrix
model.matrix(MARS.BD.1)
# Or,
MARS.BD.1$bx

# Produce a few plots 
plot(MARS.BD.1)

plot(WAISR$yearsage, MARS.BD.1$fitted.values)

pred.data = data.frame(preds=MARS.BD.1$fitted.values,x=WAISR$yearsage)


# produce a better plot

library(ggplot2)

pp = ggplot(data=WAISR, aes(x=yearsage, y=pc_bd)) +
  geom_point(color="grey") + theme_minimal() + labs(y="PC_BD",x="Age") + 
  geom_line(color='black',data = pred.data, aes(x=x, y=pc_bd))

ggsave("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_longitudinal/spline.pdf",pp)


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
