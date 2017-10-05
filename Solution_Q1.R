

generateData<- function(testRusultsNum, PosTestResultsProb, testResultsFile){
  temp = sample(x=c(0,1), prob=c(1-PosTestResultsProb,PosTestResultsProb), size=testRusultsNum, replace=TRUE )
  testresult=data.frame(Test_number=c(1:testRusultsNum),Test_results=temp[1:testRusultsNum])
  write.csv(testresult,file=testResultsFile,row.names=F)
  
}
  
loadData<-function(testResultsFile){
  testResults=read.csv(testResultsFile) 
  return(testResults)
  
}
  

bayesianInference<-function(testResults, prior, fpr, hr){

  posteriors=data.frame(Test_number=testResults$Test_number,posteriors=c(1:length(testResults$Test_results)))
  
  if(testResults$Test_results[1]==1) {
    posteriors$posteriors[1]=(hr*prior)/(hr*prior+(1-prior)*fpr)
  }
  if(testResults$Test_results[1]==0) {
    posteriors$posteriors[1]=((1-hr)*prior)/((1-hr)*prior+(1-prior)*(1-fpr))
  }
  for(i in 2:length(testResults$Test_results)) {
    if(testResults$Test_results[i]==1) {
      posteriors$posteriors[i]=(hr*posteriors$posteriors[i-1])/(hr*posteriors$posteriors[i-1]+(1-posteriors$posteriors[i-1])*fpr)
    }
    if(testResults$Test_results[i]==0) {
      posteriors$posteriors[i]=((1-hr)*posteriors$posteriors[i-1])/((1-hr)*posteriors$posteriors[i-1]+(1-posteriors$posteriors[i-1])*(1-fpr))
    }
  }
  return(posteriors)
    
}


plotPosteriors<-function(posteriors, posteriorsFig){
  pdf(posteriorsFig,width=5,height=5)
  plot(posteriors$Test_number,posteriors$posteriors,type="l",lwd=5,log="x",xlab="Test number",ylab="Posterior probability",col="red") 
  dev.off()
 
}