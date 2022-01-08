

##Step 4b.Simulation function using RF Imputation

missingness_performance_rf <- function(simulations,missing_values) {
  #Creating lists to store outputs from function
  #For Logistic Regression
  glm = list()
  predict_glm = list()
  classification_glm = list()
  roc_glm = list()
  confusion_glm = list()
  
  #For Random Forest
  rf = list()
  predict_rf = list()
  roc_rf = list()
  confusion_rf = list()
  
  nsim = simulations #number of simulations
  for(i in 1:nsim){
    data_nonpred = data[c(20,21,22)]
    data_predictors = data[c(-20,-21,-22)]
    data_missing = prodNA(data_predictors, noNA = missing_values) #Set percentage of missing values (exclusing ID variable and Target)
    data_clean =cbind(data_nonpred,data_missing)
    
    #Splitting into training and validation set
    set.seed(1234)  
    trainind=sample(1:nrow(data_clean),nrow(data_clean)*.8)
    train=data_clean[trainind,]
    validate=data_clean[-trainind,]
    
    #Random Forest Imputation for Categorical Variables
    train <- missForest(train, maxiter = 1, parallelize = 'variables')$ximp 
   
    #Adding labels to training and validation set to distinguish between them
    set = rep('Train',nrow(train))
    train = cbind(train,set)
    set = rep('Validate',nrow(validate))
    validate = cbind(validate,set)
    
    #Imputing on validation set using training set
    train_test = rbind(train,validate)
    train_test$set = as.factor(train_test$set)
    test <- missForest(train_test, maxiter = 1, parallelize = 'variables')$ximp #option 
    validate = test %>% 
      filter(set == "Validate") %>%
      select(-set)
    #Dropping set variable 
    train = train %>% select(-set)
    
    #Performing logistic regression models
    glm[[i]]=glm(Churn ~ . -ID -Churn2,family="binomial",data=train)
    predict_glm[[i]] = predict(glm[[i]],newdata=validate, type="response")
    myroc <-pROC::roc(validate$Churn, predict_glm[[i]], transpose = TRUE)
    threshold = coords(myroc, x="best", input="threshold", best.method="youden",transpose = TRUE)
    threshold = threshold[1]
    classification_glm[[i]]= rep(0,nrow(validate))
    classification_glm[[i]][predict_glm[[i]] > threshold]= '1'
    roc_glm[[i]]= roc(validate$Churn,predict_glm[[i]],plot = FALSE)$AUC
    confusion_glm[[i]] = confusion(validate$Churn,classification_glm[[i]])
    glm[[i]] = NULL #To Conserve Memory
    
    #Performing Random forest models
    rf[[i]]=randomForest(Churn2~.-ID -Churn,data=train)
    predict_rf[[i]] = predict(rf[[i]],newdata=validate,type="prob")[,2]
    roc_rf[[i]]= roc(validate$Churn2,predict_rf[[i]],plot=FALSE)$AUC
    confusion_rf[[i]] = confusion(validate$Churn2,predict(rf[[i]],validate))
    rf[[i]] = NULL #To Conserve Memory
  }
  
  #Performance Measurements
  
  #Unlisting Confusion Statistics to Create Data Table
  #For Logistic Regression Models
  glm_models <- data.frame(matrix(unlist(confusion_glm), nrow=length(confusion_glm), byrow=T))
  glm_models <- glm_models[-c(1:9)]
  setnames(glm_models, old = c('X10','X11','X12','X13'), new = c('Misclassification','Precision','Sensitivity','Specificity'))
  
  #For Random Forest Models
  rf_models <- data.frame(matrix(unlist(confusion_rf), nrow=length(confusion_rf), byrow=T))
  rf_models <- rf_models[-c(1:9)]
  setnames(rf_models, old = c('X10','X11','X12','X13'), new = c('Misclassification','Precision','Sensitivity','Specificity'))
  
  #Unlisting ROC data
  #For Logistic Regression Models
  glm_roc <- data.frame(matrix(unlist(roc_glm), nrow=length(roc_glm), byrow=T))
  setnames(glm_roc, old = c('matrix.unlist.roc_glm...nrow...length.roc_glm...byrow...T.'), new = c('AUC'))
  
  #For Random Forest Models
  rf_roc <- data.frame(matrix(unlist(roc_rf), nrow=length(roc_rf), byrow=T))
  setnames(rf_roc, old = c('matrix.unlist.roc_rf...nrow...length.roc_rf...byrow...T.'), new = c('AUC'))
  
  #Merging all Performance Data into One Table and Adding Missing Values Column (=0)
  Model= rep('Logistic Regression',nrow(glm_models))
  Missing_Values = rep(missing_values,nrow(glm_models))
  performance_glm = cbind(glm_models,glm_roc,Model,Missing_Values)
  
  Model= rep('Random Forest',nrow(rf_models))
  Missing_Values = rep(missing_values,nrow(glm_models))
  performance_rf = cbind(rf_models,rf_roc,Model,Missing_Values)
  
  performance = rbind(performance_glm,performance_rf)
  
  #Plotting Boxplots of Performance Measures
  #Setting Font Size
  theme_set(
    theme_classic(base_size = 11)
  )
  
  #Plotting Misclasification rate
  plot1 = ggplot(performance, aes(x=model, y=`Misclassification`, fill=Model)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")+ 
    ggtitle("Misclassification Rate of Models") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  #Plotting AUC
  plot2 = ggplot(performance, aes(x=model, y=`AUC`, fill=Model)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")+ 
    ggtitle("Comparison of AUC of Models") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  #Plotting Sensitivity
  plot3 = ggplot(performance, aes(x=model, y=`Sensitivity`, fill=Model)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")+ 
    ggtitle("Comparison of Sensitivity of Models") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  #Plotting Specificity
  plot4 = ggplot(performance, aes(x=model, y=`Specificity`, fill=Model)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")+ 
    ggtitle("Comparison of Specificity of Models") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  #Arranging Plots side by side
  grid.arrange(plot1, plot2,plot3, plot4, ncol=2,nrow=2)
  
  #Summary Statistics
  Logistic_Regression = sapply(performance_glm,mean)
  Random_Forest = sapply(performance_rf,mean)
  
  #Table with Performance
  summary_stats = rbind(Logistic_Regression,Random_Forest)
  summary_stats = data.frame(summary_stats)
  summary_stats = summary_stats[-c(6)]
  summary_stats$Model = c("Logistic Regression","Random Forest")
  
  #Saving datasets with results into their respective folder
  write.csv(summary_stats,file= paste("Output/RF Imputation Summary/Summary_Performance_rf_", missing_values, ".csv", sep=""))
  write.csv(performance,file= paste("Output/RF Imputation/Full_Performance_rf_", missing_values, ".csv", sep=""))
}

#----------------------------------END OF CODE------------------------------------------------------