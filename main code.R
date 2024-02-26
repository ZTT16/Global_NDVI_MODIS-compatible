library(terra)
library(zoo)
library(randomForest)
library(foreach)
library(iterators)
library(doParallel)
setwd("D:/yuceshujuji/new")
r1 <- list.files("H:/Data/MODIS/MOD13C1_VI_5600m/",pattern = '.tif$',full.names=TRUE)
r2 <- list.files("H:/Data/GlobalData/AVHRR/MVC/16days",pattern = '.tif$',full.names=TRUE)
r3 <- list.files("H:/Data/GlobalData/AVHRR/MVC/16days",pattern = '.tif$',full.names=TRUE)
modis <- rast(r1)
avhrr1 <- rast(r2)
avhrr2 <- rast(r3)

for (ii in c(13:16) )
{
  
  for (jj in c(1:34))
  {
    
    tryCatch({
      
      md1<-modis[c((200*ii-1):(200*ii+202)),c((200*jj-1):(200*jj+202))]
      ah1<-avhrr1[c((200*ii-1):(200*ii+202)),c((200*jj-1):(200*jj+202))]
      ah2<-avhrr2[c((200*ii-1):(200*ii+202)),c((200*jj-1):(200*jj+202))]
      print(paste0("行：",200*ii-1,":",200*ii+202,",","列：",200*jj-1,":",200*jj+202))
      
      
      #}}  
      #md1<-modis[c(699:902),c(5999:6202)]
      #ah1<-avhrr1[c(699:902),c(5999:6202)]
      #ah2<-avhrr2[c(699:902),c(5999:6202)]
      md1<-(md1/10000)
      md1<-t(md1)
      ah1<-t(ah1)
      ah2<-t(ah2)
      colnames(md1)<-colnames(md1,do.NULL=FALSE,prefix="mo")
      colnames(ah1)<-colnames(ah1,do.NULL=FALSE,prefix="av")
      colnames(ah2)<-colnames(ah2,do.NULL=FALSE,prefix="av")
      m2<-cbind(md1,ah1)
      m1<-ah2
      m2<-na.approx(m2)
      m1<-na.approx(m1)
      m1[is.na(m1)] <- 0
      m2[is.na(m2)] <- 0
      #setwd("D:/yuceshujuji/test")
      #save(m2, m1, file="t4.RData")
      
      
      #load("t4a.RData")
      result_df <- data.frame(no=c(1:460))
      y_df <- m2[,1:41616]
      x_df <- m2[,41617:83232]
      fun <- function(y_df, i, x_df, m2, m1, result_df) {
        y <- y_df[,i]
        #x <- x_df[,c((i-1),i,(i+1),(i-102),(i+102))]
        #x <- x_df[,c((i-1),(i-101),(i-103),i,(i+1),(i-102),(i+101),(i+102),(i+103))]
        x<-x_df[,c(i,(i-1),(i+1),(i-2),(i+2),(i-104),(i+104),(i-105),(i+105),(i-106),(i+106),(i-103),(i+103),(i-102),(i+102),(i+208),(i-208),(i+209),(i-209),(i+210),(i-210),(i+206),(i-206),(i+207),(i-207))]
        #x<-x_df[,c(i,(i+1),(i+2),(i+3),(i+103),(i+104),(i+105),(i+106),(i+107),(i+108),(i+109),(i+209),(i+210),(i+211),(i+212),(i+213),(i+214),(i+215),(i+315),(i+316),(i+317),(i+318),(i+319),(i+320),(i+321),(i-1),(i-2),(i-3),(i-103),(i-104),(i-105),(i-106),(i-107),(i-108),(i-109),(i-209),(i-210),(i-211),(i-212),(i-213),(i-214),(i-215),(i-315),(i-316),(i-317),(i-318),(i-319),(i-320),(i-321))]
        set.seed(123)
        fit.rf<-randomForest(x=x,y=y,data=m2,ntree=100)
        prediction.rf<-predict(fit.rf,m1)
        col<-prediction.rf
        result_df <- cbind(result_df,col)
        return(result_df)
      }
      cl <- makeCluster(20)     
      registerDoParallel(cl)
      print("---------Task Start!----------")
      system.time({AllResult <- foreach(i=c(411:41206), .packages = "randomForest") %dopar% fun(y_df, i, x_df, m2, m1, result_df)})
      # c(103:10302)
      # c(104:10301)
      stopCluster(cl) 
      closeAllConnections()
      print("---------Task Finish!----------")
      
      df_AllResult<- as.data.frame(AllResult)
      a<-df_AllResult[,seq(0,ncol(df_AllResult),2)]
      a<-round(a,4)
      a[a==0]<-NA
      i<-c(1:200)
      
      s7<-a[ , -c((204*i),(204*i-1),(204*i-2),(204*i-3))]
      
      #s10<-a[ , -c(101,(102*i),(102*i+101),10302)]
      
      
      name<- paste0("m_",ii,"_",jj,".RData")
      save(s7, file=name)
      
    },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
    Sys.sleep(2) 
  }
}
