

##Step 5.Evaluating Performance of Simulated Models (Functions used to produce plots)

#Loading summary data files for summary performance of mode imputed and Random Forest imputed models

#Mode Imputed model
#Setting working directory
here = here()
setwd(here)
setwd("Output/Mode Imputation_Summary")

#Importing Performance Data
temp = list.files(pattern="*.csv")
performance_raw = lapply(temp, read.csv)

#Renaming and Dropping Variables to Make Merge between Base and Mode Imputed Performance Data
performance_raw[[1]]$X = NULL
for(i in 2:(length(performance_raw))){
  performance_raw[[i]]$X = NULL
}

#Creating dataframe and turning Model into factor while adding imputation column
performance_mode = bind_rows(performance_raw)
performance_mode$Model = as.factor(performance_mode$Model)
Imputation= rep('Mode',nrow(performance_mode))
performance_mode = cbind(performance_mode,Imputation)

#-----------------------------------------------------------------------------------------------

#RF Imputed model
here = here()
setwd(here)
setwd("Output/RF Imputation Summary")

#Importing Performance Data
temp = list.files(pattern="*.csv")
performance_raw = lapply(temp, read.csv)

#Renaming and Dropping Variables to Merge Base and Mode Imputed Performance Data
performance_raw[[1]]$X = NULL
for(i in 2:(length(performance_raw))){
  performance_raw[[i]]$X = NULL
}

#Creating dataframe and turning Model into factor
performance_rf = bind_rows(performance_raw)
performance_rf$Model = as.factor(performance_mode$Model)
Imputation= rep('Random Forest',nrow(performance_rf))
performance_rf = cbind(performance_rf,Imputation)

#-----------------------------------------------------------------------------------------------

#Creating new datasets: 1) All Logistic Regression Models 2) All Random Forest Models
#Also removing base model as these datasets will be used to compare imputation methods
performance_summary = rbind(performance_mode,performance_rf)
performance_summary = performance_summary %>% filter(Missing_Values != 0.00)
performance_glm = performance_summary %>% filter(Model == "Logistic Regression")
performance_randomforest = performance_summary %>% filter(Model == "Random Forest")


#Creating function to plot misclassification rate of both models
misclassification_plot <- function(dataset,imputation) {
  #Setting deafult font size of plots
  theme_set(
    theme_classic(base_size = 8)
  )
  #Plotting Line Chart for Misclassification Rate
  my_title = paste("Performance of ",imputation," Imputation Models with Different Levels of Missing Values",sep="") #title
  ggplot(data=dataset, aes(x=Missing_Values, y=Misclassification, group=Model)) +
    geom_line(size=1, aes(color=Model))+
    geom_point(size = 1.5)+
    labs(x = "Percentage of Missing Values (%)", y = "Misclassification (%)") +
    #ggtitle(wrapper(my_title, width = 30))+
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="Model"))+
    theme(
      axis.text=element_text(size=7.5),
      axis.title=element_text(size=8.5),
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.text=element_text(size=8),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
}

#-----------------------------------------------------------------------------------------------

#Creating function to plot (2x2) other performance measures (AUC, Precision, Specificity, and Sensitivity) both models
other_measures_plot <- function(dataset,imputation) {
  #Setting font size
  theme_set(
    theme_classic(base_size = 8)
  )
  
  #Plotting line Chart for AUC
  my_title = "AUC" #title
  AUC = ggplot(data=dataset, aes(x=Missing_Values, y=AUC, group=Model)) +
    geom_line(size=1, aes(color=Model))+
    geom_point(size = 1.5)+
    ggtitle(wrapper(my_title, width = 30))+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      legend.text=element_text(size=9),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text=element_text(size=7.5),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  #Plotting line Chart for Precision
  my_title = "Precision" #title
  Precision = ggplot(data=dataset, aes(x=Missing_Values, y=Precision, group=Model)) +
    geom_line(size=1, aes(color=Model))+
    geom_point(size = 1.5)+
    ggtitle(wrapper(my_title, width = 30))+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none")+ 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text=element_text(size=7.5),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  #Plotting line Chart for Sensitivity
  my_title = "Sensitivity" #title
  Sensitivity = ggplot(data=dataset, aes(x=Missing_Values, y=Sensitivity, group=Model)) +
    geom_line(size=1, aes(color=Model))+
    geom_point(size = 1.5)+
    ggtitle(wrapper(my_title, width = 30))+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none")+ 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text=element_text(size=7.5),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  #Plotting line Chart for Specificity
  my_title = "Specificity" #title
  Specificity = ggplot(data=dataset, aes(x=Missing_Values, y=Specificity, group=Model)) +
    geom_line(size=1, aes(color=Model))+
    geom_point(size = 1.5)+
    ggtitle(wrapper(my_title, width = 30))+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none")+ 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text=element_text(size=7.5),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  mylegend=g_legend(AUC)
  
  #Arranging Plots side by side (Optional output)
  Grid_Perf_Mode = grid.arrange(arrangeGrob(AUC+theme(legend.position = "none"),Precision, Sensitivity,Specificity, ncol = 2, nrow = 2), 
                                mylegend, heights=c(10, 1))
}


#-----------------------------------------------------------------------------------------------

#Creating Function to Plot line charts (1x2) Comparing Performance of Imputation Methods
imputation_comparison <- function(fun.data1,fun.data2, fun.y) {
  fun.data1$fun.y <- fun.data1[, fun.y]
  fun.data2$fun.y <- fun.data2[, fun.y]
  
  #Setting font size
  theme_set(
    theme_classic(base_size = 8)
  )
  #Plotting line Chart for Logistic Regression Models
  my_title = "Logistic Regression" #title
  LR = ggplot(data=fun.data1, aes(x=Missing_Values, y=fun.y, group=Imputation)) +
    geom_line(size=1, aes(color=Imputation))+
    geom_point(size = 1)+
    labs(y = fun.y)+
    ggtitle(wrapper(my_title, width = 40))+
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_jco() +
    guides(fill=guide_legend(title="Model"))+
    theme(
      axis.title.x = element_blank(),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      legend.position = "bottom",
      legend.title=element_text(size=8),
      legend.text=element_text(size=8),
      axis.title=element_text(size=8.5),
      axis.text=element_text(size=7.5),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  
  #Plotting line Chart for Random Forest Models
  my_title = "Random Forest" #title
  RF = ggplot(data=fun.data2, aes(x=Missing_Values, y=fun.y, group=Imputation)) +
    geom_line(size=1, aes(color=Imputation))+
    geom_point(size = 1)+
    ggtitle(wrapper(my_title, width = 40))+
    scale_color_jco()+
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="Model"))+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title=element_text(size=8.5),
      axis.text=element_text(size=7.5),
      panel.border = element_rect(fill=NA, colour = "black", 
                                  size=0.75),
      panel.background = element_rect(fill = "white",
                                      colour = "black",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
    )
  mylegend = g_legend(LR)
  
  #Arranging Plots side by side
  Grid_Perf_Mode = grid.arrange(arrangeGrob(LR+theme(legend.position = "none"),RF+theme(legend.position = "none"), 
                                            ncol = 2, nrow = 1), mylegend, heights=c(7, 1)) 
}

#----------------------------------END OF CODE------------------------------------------------------
