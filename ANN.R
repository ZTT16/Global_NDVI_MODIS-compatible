
library(nnet)
nnet.fit1<- nnet(medv ~. ,data=BostonHousing, size=2)
nnet.predict1<-predict(nnet.fit1)

library(caret)
mygrid<- expand.grid(.decay = c(0.5,0.1), .size= c(4,5,6))
nnetfit<- train(medv~., data=BostonHousing, method= "nnet", maxit = 1000, tuneGrid=mygrid, trace=F)
print(nnetfit)
