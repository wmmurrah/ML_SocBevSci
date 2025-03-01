library(ggplot2)

x <- rnorm(100000,0,2)
dist1 = dnorm(x,0,1)*.9
dist2 = dnorm(x,-2.3,.5)*.1
mix = dist1 + dist2
data = data.frame(x, dist1, dist2, mix)

plot = ggplot(data, aes(x=x, y=dist1)) +
  geom_line(colour='black', size=3) +
  geom_line(aes(x=x, y=dist2), colour='black', size=3) +
  geom_line(aes(x=x, y=mix), colour='grey', size=3) +
  theme(legend.position="none") +
  scale_y_continuous("Density", limits=c(0,.5)) +
  scale_x_continuous("x", limits=c(-4, 4)) + theme_minimal()
  
plot
