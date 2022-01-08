

##Step 3: Base Model Development

#Splitting into training and validation set using 80:20 ratio proportion
set.seed(1234)
trainind=sample(1:nrow(data),nrow(data)*.8)
train=data[trainind,]
validate=data[-trainind,]

#Performing logistic regression models and evaluating using ROC and Confusion table
glm_base=glm(Churn ~ . -ID -Churn2,family="binomial",data=train)
predict_glm = predict(glm_base,newdata=validate, type="response")
myroc <-pROC::roc(validate$Churn, predict_glm,transpose = TRUE)
threshold = coords(myroc, x="best", input="threshold", best.method="youden",transpose = TRUE)
threshold = threshold[1]
classification_glm= rep(0,nrow(validate))
classification_glm[predict_glm > threshold]= '1'
AUC_glm= roc(validate$Churn,predict_glm)$AUC
confusion_glm = confusion(validate$Churn,classification_glm)

#Performing Random forest models and evaluating using ROC and Confusion table
rf_base=randomForest(Churn2~.-ID -Churn,data=train)
predict_rf = predict(rf_base,newdata=validate,type="prob")[,2]
AUC_rf= roc(validate$Churn2,predict_rf)$AUC
confusion_rf = confusion(validate$Churn2,predict(rf_base,validate))

#------------------------------------------------------------------------------------------------

#Comparing performance of logistic regression model and random forest using ROC curve
roc(validate$Churn,predict_glm)$AUC
roc(validate$Churn2,predict_rf,lines=TRUE,col="red")$AUC
#Similar performance

#Comparing performance using performance measures obtained in confusion function
#Performance of Logistic Regression Models
glm_perf = as.data.frame(confusion_glm)  
glm_perf = glm_perf[-c(1:3)]  %>%
  slice(1)

#Performance of Random Forest Models
rf_perf = as.data.frame(confusion_rf)  
rf_perf = rf_perf[-c(1:3)]  %>%
  slice(1)

#Adding ROC to performance table and renaming columns
glm_perf = cbind(glm_perf,AUC= AUC_glm)
rf_perf = cbind(rf_perf,AUC = AUC_rf)

#-------------------------------------------------------------------------------------------------

#Merging all Performance Data from both Models into One Table and Adding Missing values column (=0)
Model= rep('Logistic Regression',nrow(glm_perf))
Missing_Values = rep(0.00,nrow(glm_perf))
performance_glm = cbind(Model,Missing_Values,glm_perf)

Model= rep('Random Forest',nrow(rf_perf))
Missing_Values = rep(0.00,nrow(rf_perf))
performance_rf = cbind(Model,Missing_Values,rf_perf)
performance_rf$Model = as.factor(Model)

#Output of Performance (Performance_Base) to both Mode Imputation Folder and RF Impuatation Folder, and Output Folder
Performance_Base = rbind(performance_glm,performance_rf)
write.csv(Performance_Base,"Output/RF Imputation Summary/Performance_Base.csv")
write.csv(Performance_Base,"Output/Mode Imputation_Summary/Performance_Base.csv")
write.csv(Performance_Base,"Output/Performance_Base.csv")

#----------------------------------END OF CODE------------------------------------------------------