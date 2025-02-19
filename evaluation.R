
substrRight <- function(x, n) { substr(x, nchar(x)-n+1, nchar(x)) }

EF<- function(observed,predicted){
  mean <- mean(observed)
#term1 (observed-mean)^2
term1<-  (observed-mean)^2
#term2 (observed-predicted)^2
term2<-(observed-predicted)^2  
sterm1<-sum(term1)
sterm2<-sum(term2)
EF=(sterm1-sterm2)/sterm1
EF=round(EF,digits = 3)
   return(EF)
}

EF_corr<- function(observed,predicted,mean){
  #term1 (observed-mean)^2
  term1<-  (observed-mean)^2
  #term2 (observed-predicted)^2
  term2<-(observed-predicted)^2  
  sterm1<-sum(term1)
  sterm2<-sum(term2)
  EF=(sterm1-sterm2)/sterm1
  EF=round(EF,digits = 3)
  return(EF)
}

RMSE<- function(observed,predicted){
  
  #term1 (observed-predicted)^2
  term1<-  (observed-predicted)^2
  sterm1<-sum(term1)
MSE<-sterm1/length(observed)
RMSE<-MSE^0.5
  RMSE=round(RMSE,digits = 3)
  return(RMSE)
}

#created AC
RRMSE<- function(observed,predicted){
  
  #term1 (observed-predicted)^2
  term1<-  (observed-predicted)^2
  sterm1<-sum(term1)
  MSE<-sterm1/length(observed)
  RMSE<-sqrt(MSE)
  RRMSE<-100*RMSE/mean(observed)
  RRMSE=round(RRMSE,digits = 2)
  return(RRMSE)
}

#created AC
ME<- function(observed,predicted){
  
  #term1 (observed-predicted)^2
  term1<-  (observed-predicted)
  sterm1<-sum(term1)
  ME<-sterm1/length(observed)
  ME=round(ME,digits = 3)
  return(ME)
}

#created AC 27/11/2023
MAE<- function(observed,predicted){
  #term1 (observed-predicted)^2
  term1<-  abs(observed-predicted)
  sterm1<-sum(term1)
  ME<-sterm1/length(observed)
  ME=round(ME,digits = 3)
  return(ME)
}

MSD<- function(observed,predicted){
  
  #term1 (observed-predicted)^2
  term1<-  (observed-predicted)^2
  sterm1<-sum(term1)
  MSD<-sterm1/length(observed)
  MSD=round(MSD,digits = 3)
  return(MSD)
}

SB<- function(observed,predicted){
  mean_obs<-mean(observed)
  mean_pred<-mean(predicted)
  SB<-(mean_obs-mean_pred)^2
  SB=round(SB,digits = 3)
  return(SB)
}

NU<- function(observed,predicted){
  #from Filippos suggestion: 
  #Gauch 2003: Model Evaluation by Comparison of Model-Based Predictions and Measured Values
  mean_pred<-mean(predicted)
  fit = lm(observed~predicted)
  intercept<-coef(fit)[1]
  slope<-coef(fit)[2]
  term1<-(sum((predicted-mean_pred)^2)/length(predicted))
  NU<-(1-slope)^2*term1
  NU=round(NU,digits = 3)
  return(NU)
}

LC<- function(observed,predicted){
  #from Filippos suggestion: 
  #Gauch 2003: Model Evaluation by Comparison of Model-Based Predictions and Measured Values
  mean_obs<-mean(observed)
  fit = lm(observed~predicted)
  intercept<-coef(fit)[1]
  slope<-coef(fit)[2]
  r2<-(summary(fit)$r.squared)
  term1<-(sum((observed-mean_obs)^2)/length(observed))
  LC<-(1-r2)*term1
  LC=round(LC,digits = 3)
  return(LC)
}


SSE1<- function(observed,predicted){
  #from M?ller 1998 paper (normal SSE calculation as for regression I think)
  mean <- mean(observed)
  term1<-(observed-predicted)
  term2<-  (mean-predicted)
  term3<-(term1-term2)^2
  SSE<-sum(term3)
  return(SSE)
}

SSE2<- function(observed,predicted){
  #from Mueller 1998 paper (I think this equation can be simplified)
  mean <- mean(observed)
  term1<-(observed-mean)^2
  SSE<-sum(term1)
  return(SSE)
}


R_square<-function(observed,predicted){
  mean_obs<-mean(observed)
  mean_pred<-mean(predicted)
  term1 <-(observed-mean_obs)*(predicted-mean_pred)
  sterm1<-sum(term1)
  term2<-((sum((observed-mean_obs)^2))^0.5)
  term3<-((sum((predicted-mean_pred)^2))^0.5)
  R_square<-(sterm1/(term2*term3))^2
  R_square=round(R_square,digits = 3)
    return(R_square)
}


MBE<-function(observed,predicted){
  MBE<-mean(predicted-observed)
  MBE=round(MBE,digits = 3)
  return(MBE)
}








multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
    # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
    numPlots = length(plots)
    # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
    if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
