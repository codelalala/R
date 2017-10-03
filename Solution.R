
#this is the loadData function
loadData <- function(name,name1,name2){
  data=read.csv(name)
  cnt=data$cnt
  temp=data$temp
  return(list(cnt,temp))
}
#this is the train function
train <- function(y,x){
  y_mean=mean(y)
  x_mean=mean(x)
  beta_1=sum((x-x_mean)*(y-y_mean))/sum((x-x_mean)^2)
  beta_0=y_mean-beta_1*x_mean
  return(list(beta0=beta_0,beta1=beta_1))
}
#this is the test function
test <-function(cnt,temp,beta1,beta0){
  x=temp
  y=cnt
  RSS=sum((y-beta0-beta1*x)^2)
  return(RSS)
}
#this is the plotDataModel function
plotDataModel <-function(cnt,temp,beta1,beta0,plotname){
  pdf(plotname,width=5,height=5)
  plot( temp , cnt , type="p" , xlab="temp" , ylab="cnt" , main="cnt vs temperature" )
  abline(a=beta0,b=beta1,col="red")
  dev.off()
}

