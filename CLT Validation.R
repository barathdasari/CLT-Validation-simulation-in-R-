################################################################################
# 1

simulations<-5000
n<-1   # no of sample

mean.value<- rep(NA,simulations)

for (i in 1:simulations){
  x<- runif(n)
  mean.value[i]<- mean(x)
}

h <- hist(mean.value, breaks = 10, density = 10,
          col = "gray", xlab = "Mean values", main = "Overall") 

##################################################################################
# 2

simulations<-5000
n<-5   # no of sample

mean.value<- rep(NA,simulations)

for (i in 1:simulations){
  x<- runif(n)
  mean.value[i]<- mean(x)
}

h <- hist(mean.value, breaks = 10, density = 10,
          col = "gray", xlab = "Mean values", main = "Overall") 

#################################################################################
# large values of n 

simulations<-5000
n<-300   # no of samples
  
mean.value<- rep(NA,simulations)

for (i in 1:simulations){
  x<- runif(n)
  mean.value[i]<- mean(x)
}

h <- hist(mean.value, breaks = 10, density = 10,
          col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(mean.value), max(mean.value), length = 40) 
yfit <- dnorm(xfit, mean = mean(mean.value), sd = sd(mean.value)) 
yfit <- yfit * diff(h$mids[1:2]) * length(mean.value) 

lines(xfit, yfit, col = "red", lwd = 2)


