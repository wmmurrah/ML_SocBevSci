gender <- rbinom(1000,1,0.5)
depression <- rep(NA,1000)

depression[gender==1]   <- rnorm(sum(gender==1),5,1)

depression[gender==0] <- rnorm(sum(gender==0),5,1)

anxiety <- rep(NA,1000)

#anxiety[depression<4.8] <- 4
#anxiety[depression>=4.8 & depression < 6.6] <- 8
#anxiety[depression>=6.6 & depression < 9.7] <- 12
#anxiety[depression >= 9.7] <- 16


anxiety[gender == 1 & depression < 5] <- 4
anxiety[gender == 1 & depression >= 5] <- 8
anxiety[gender == 0 & depression < 5] <- 12
anxiety[gender == 0 & depression >= 5] <- 16

#anxiety[depression>=4.8 & depression < 6.6] <- 8
#anxiety[depression>=6.6 & depression < 9.7] <- 12
#anxiety[depression >= 9.7] <- 16

dat <- data.frame(cbind(gender,depression,anxiety))

library(rpart)

main <- rpart(anxiety ~ ., dat)
rpart.plot::rpart.plot(main)



# 3 D



mat = matrix(NA,2,12)

mat[2,1:4] <- 4
mat[2,5:12] <- 8
mat[1,1:9] <- 12
mat[1,10:12] <- 16

mat = matrix(NA,2,8)

mat[2,1:4] <- 4
mat[2,5:8] <- 8
mat[1,1:4] <- 12
mat[1,5:8] <- 16

p2 = plot_ly(z=~mat,ylab="Gender",showscale=F,colors = colorRamp(c("grey","black"))) %>% add_surface() %>% layout(
  title = "Decision Surface",
  scene = list(
    xaxis = list(title = "Depression"),
    yaxis = list(title = "Gender"),
    zaxis = list(title = "Predicted Anxiety")
  )) 
p2



library(rgl)
preds = predict(main)
plot3d(gender,depression,preds)
planes3d(gender,depression,preds,type = "s", col = "red", size = 1)
