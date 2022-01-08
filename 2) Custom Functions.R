

##Step 2: Loading Functions to be Used

#Mode function for Imputation of categorical variables
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1)
    xmode <- xmode[1]
  return(xmode)
}

#Confusion Statistics and ROC (code from Jean-Francois Plante)
confusion=function(truth,pred,conversion=c(1,1)){
  a=conversion*table(truth,pred,dnn=c("Truth","Prediction"))
  if(ncol(a)<2){ return(  list(
    Confusion=NA,
    Misclassification=NA,
    Precision=NA,
    Sensitivity=NA,
    Specificity=NA
  )
  )
  }
  list(
    Confusion=addmargins(a, FUN = list(Total = sum), quiet = TRUE),
    Misclassification=1-sum(diag(a))/sum(a),
    Precision=a[2,2]/sum(a[,2]),
    Sensitivity=a[2,2]/sum(a[2,]),
    Specificity=a[1,1]/sum(a[1,])
  )
  
}

roc=function(truth,p,k=100,plot=TRUE,lines=FALSE,...){
  Curve=rbind(c(0,0),
              t(sapply(quantile(p,(k:1)/(k+1)),function(th){
                pred=as.numeric(p>th)
                if(length(unique(pred))==2){
                  a=confusion(truth,pred)
                  return(c(1-a$Specificity,a$Sensitivity))
                } else {
                  return(c(NA,NA))
                }
              }
              )),
              c(1,1)
  )
  Curve=Curve[complete.cases(Curve),]
  if(plot&!lines) plot(Curve,xlab="1-Specificity",ylab="Sensitivity",main="ROC curve",xlim=0:1,ylim=0:1,type="l",...)
  if(plot&lines) lines(Curve,...)
  invisible(list(ROC=Curve,AUC=sum(diff(Curve[,1])*(Curve[-1,2]+Curve[-nrow(Curve),2])/2)))
}


#Wrapper function for wrapping long titles
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Return common legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#Setting working directory using shortcut
set_wd_function <- function(directory) setwd( file.path(getwd(), directory))

#----------------------------------END OF CODE------------------------------------------------------
