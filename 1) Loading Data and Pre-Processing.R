
##Step 1: Data Pre-Processing

#Reading Churn Dataset
data_raw = read.csv("churn.csv")

#Printing Summary Statistics
summary(data_raw)

#Checking for Missing Values in Dataset
sapply(data_raw, function(data_raw) sum(is.na(data_raw)))
#Note: There are 11 missing values in totalcharges column

#Removing Missing Values from Dataset
data_raw <- data_raw[complete.cases(data_raw), ]
glimpse(data_raw)

#Converting categorical variables and binary variables from characters to factors
data = data_raw %>% 
  mutate(gender = as.factor(gender))  %>% 
  mutate(Partner = as.factor(Partner))  %>% 
  mutate(Dependents = as.factor(Dependents))  %>%
  mutate(PhoneService = as.factor(PhoneService))  %>% 
  mutate(MultipleLines = as.factor(MultipleLines))  %>% 
  mutate(InternetService = as.factor(InternetService))  %>%
  mutate(OnlineSecurity = as.factor(OnlineSecurity))  %>% 
  mutate(OnlineBackup = as.factor(OnlineBackup))  %>% 
  mutate(DeviceProtection = as.factor(DeviceProtection))  %>%
  mutate(TechSupport = as.factor(TechSupport))  %>% 
  mutate(StreamingTV = as.factor(StreamingTV))  %>% 
  mutate(StreamingMovies = as.factor(StreamingMovies))  %>%       
  mutate(Contract = as.factor(Contract))  %>% 
  mutate(PaperlessBilling = as.factor(PaperlessBilling))  %>% 
  mutate(PaymentMethod = as.factor(PaymentMethod))  %>%     
  mutate(SeniorCitizen = as.factor(SeniorCitizen)) %>% 
  mutate(Churn = as.factor(Churn)) %>%  
  mutate(customerID = as.factor(customerID))

#Plotting histograms of continuous variables to determine appropriate bin sizes for continuous variables
histogram(data$tenure)
histogram(data$MonthlyCharges)
histogram(data$TotalCharges)

#Creating categorical variables using bin sizes determined above
data[, "tenure"] <- bin_data(data$tenure, bins=c(0, 15, 30, 45, 60, 72), binType = "explicit")
data[, "MonthlyCharges"] <- bin_data(data$MonthlyCharges, bins=c(0, 30, 60, 90, 120), binType = "explicit")
data[, "TotalCharges"] <- bin_data(data$TotalCharges, bins=c(0, 1500,3000,4500,6000,7500,9000), binType = "explicit")

#Adding ID variable and dropping customer ID for simulation analysis 
data$ID <- seq.int(nrow(data))
data = data[,-1]

#Making churn into a dummy variable
data$Churn <- ifelse(data$Churn == 'Yes', 1, 0)
summary(data)

#Making second target variable 'Churn2' which is a factor variable for use with random forest models
data$Churn2 = as.factor(data$Churn) 

#----------------------------------END OF CODE------------------------------------------------------

