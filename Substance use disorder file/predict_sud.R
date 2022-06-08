library('dplyr')
library('tidyverse')
library('ggplot2') 
library('cowplot')
library('caTools')
library('ElemStatLearn')
library('class')
library('e1071')
library('rpart')
library('rpart.plot')
library('randomForest')
library('caret')
library('knitr')
library('PerformanceAnalytics')
library('ROSE')
library('metan')
library('gmodels')
library('CGPfunctions')
library('lsr')
library('ggpubr')
library('pROC')
library('plotly')
library('reshape2')


# Data pre-processing

Dataset <- read.csv('Data.csv', header = TRUE, stringsAsFactors = FALSE)
str(Dataset)
summary(Dataset)
fix(Dataset)

patientData <- Dataset %>% select(CASEID, AGE, ALCDRUG, DSMCRIT, EDUC, EMPLOY, ETHNIC, FREQ1, FREQ2, FREQ3, FRSTUSE1,
                                  FRSTUSE2, FRSTUSE3, GENDER, LIVARAG, LOS, MARSTAT, METHUSE, NOPRIOR,
                                  PSOURCE, RACE, ROUTE1, ROUTE2, ROUTE3, SERVICES, SUB1, SUB2, SUB3, 
                                  VET, REASON)

#patientData %>% slice(1:1000)

# patientData_2 <- Dataset %>% select(AGE, ALCDRUG, DSMCRIT, EDUC, EMPLOY, ETHNIC, FREQ1, FREQ2, FREQ3, FRSTUSE1,
#                                   FRSTUSE2, FRSTUSE3, GENDER, LIVARAG, LOS, MARSTAT, METHUSE, NOPRIOR, PREG
#                                   PSOURCE, RACE, ROUTE1, ROUTE2, ROUTE3, SERVICES, SUB1, SUB2, SUB3,
#                                   VET, REASON)

#REASON - Target variable

str(patientData)
summary(patientData)

#Consider patients with REASON as Treatment Completed. Replace everything else with 0
patientData$REASON <- replace(patientData$REASON, patientData$REASON > 1, 0)
#Dataset$REASON <- replace(Dataset$REASON, Dataset$REASON > 1, 0)


#Set the dependent variable - REASON as factor 
patientData$REASON <- as.factor(patientData$REASON)
Dataset$REASON <- as.factor(Dataset$REASON)

str(patientData)
summary(patientData)

# temp <- replace(patientData$GENDER, patientData$GENDER == -9, NA)
# temp
# which(is.na(temp))
# 
# patientData$GENDER <- replace(patientData$GENDER, patientData$GENDER == -9, NA)
# patientData$PREG <- replace(patientData$PREG, patientData$PREG == -9, NA)

#Remove rows with null values encoded as -9
patientData <- patientData[apply(patientData!= -9, 1, all),]
str(patientData)
dim(patientData)
head(patientData)


#Feature Selection using Boruta Algorithm
library(Boruta)
set.seed(111)
boruta <- Boruta(REASON~. , data = patientData, doTrace = 2, maxRuns = 20)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.6)

#Using Random Forest feature importance
rf_classifier <- randomForest(REASON ~. , data = train_set)

var_importance <- tibble(variable=setdiff(colnames(patientData), "REASON"),
                             importance=as.vector(importance(rf_classifier)))

var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar(alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Variable Importance from Random Forest Algorithm")
p <- p + xlab("Patient Attributes") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + scale_fill_discrete(name="Patient Attributes Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))



#The main thing is, we reject the null hypothesis if the p-value that comes out in 
# the result is less than a predetermined significance level, which is 0.05 usually, 
# then we reject the null hypothesis.
# 
# H0: The two variables are independent.
# H1: The two variables relate to each other.
# 
# In the case of a null hypothesis, a chi-square test is to test the two variables that are independent.

#Target vs Age
#Bar chart
ggplot(data = patientData, 
       aes(AGE, fill = REASON)) +
         geom_bar(position = "dodge", 
                  alpha = 0.5) + 
         theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
                              labs(title = "Age vs Treatment Completed",
                                   x = "Age Group", y= "Treatment Status")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
                            "45-49","50-54","55-64","65 >"))+
  scale_fill_discrete(name="Treatment\nStatus")
                    

#Target vs Alcohol and other drugs
ggplot(data = patientData, 
       aes(ALCDRUG, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Alcohol and other drugs vs Treatment Completed",
       x = "Alcohol and other drugs", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs DSM diagnosis - Type of SUD


ggplot(data = patientData, 
       aes(DSMCRIT, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
                            "45-49","50-54","55-64","65 >"))
  labs(title = "DSM diagnosis vs Treatment Completed",
       x = "DSM diagnosis", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs Education
ggplot(data = patientData, 
       aes(EDUC, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5"),
                   labels=c("No school/\n kg – grade 8","Grades 9 – 11","Grade 12/(GED)", 
                            "1-3 years of\nuniversity/college", "years of college\n/BA/BS/university"))+
  labs(title = "Education vs Treatment Completed",
       x = "Education", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs Employ
ggplot(data = patientData, 
       aes(EMPLOY, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Employment vs Treatment Completed",
       x = "Employment", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                       breaks = c("0","1"),
                       labels = c("Treatment not completed", "Treatment completed"))

#Target vs Ethnicity
ggplot(data = patientData, 
       aes(ETHNIC, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Ethnicity vs Treatment Completed",
       x = "Ethnicity", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs FREQ1
freq1p <- ggplot(data = patientData, 
       aes(FREQ1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of use at admission (primary) vs \nTreatment Completed",
       x = "Frequency", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs FREQ2
freq2p <-ggplot(data = patientData, 
       aes(FREQ2, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of use at admission (secondary) vs \nTreatment Completed",
       x = "Frequency", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs FREQ3
freq3p <-ggplot(data = patientData, 
       aes(FREQ3, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of use at admission (tertiary) vs \nTreatment Completed",
       x = "Frequency", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

freqp <- ggarrange(freq1p,freq2p,freq3p, ncol = 2, nrow = 2)
freqp


#Target vs FRSTUSE1
frst1 <-ggplot(data = patientData, 
                aes(FRSTUSE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first use (primary) vs \nTreatment Completed",
       x = "Age at first use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs FRSTUSE2
frst2 <-ggplot(data = patientData, 
               aes(FRSTUSE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first use (secondary) vs \nTreatment Completed",
       x = "Age at first use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs FRSTUSE3
frst3 <-ggplot(data = patientData, 
               aes(FRSTUSE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first use (tertiary) vs \nTreatment Completed",
       x = "Age at first use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

frst <- ggarrange(frst1, frst2, frst3, ncol = 2, nrow = 2)
frst


#Target vs GENDER
ggplot(data = patientData, 
               aes(GENDER, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Gender vs Treatment Completed",
       x = "Gender", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs LIVARAG
ggplot(data = patientData, 
       aes(LIVARAG, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Living arrangements vs Treatment Completed",
       x = "Living arrangements", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs LOS
ggplot(data = patientData, 
       aes(LOS, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Length of stay at the facility vs \n Treatment Completed",
       x = "Length of stay", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs MARSTAT
ggplot(data = patientData, 
       aes(MARSTAT, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Marital status vs Treatment Completed",
       x = "Marital status", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs METHUSE
ggplot(data = patientData, 
       aes(METHUSE, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Medication-assisted opioid therapy vs \nTreatment Completed",
       x = "Medication-assisted opioid therapy", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs NOPRIOR
ggplot(data = patientData, 
       aes(NOPRIOR, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Previous substance use treatment episodes vs \nTreatment Completed",
       x = "Previous substance use treatment episodes", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))


#Target vs PSOURCE
ggplot(data = patientData, 
       aes(PSOURCE, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Referral source vs Treatment Completed",
       x = "Referral source", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs RACE
ggplot(data = patientData, 
       aes(RACE, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Race vs Treatment Completed",
       x = "Race", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs ROUTE1
rout1<- ggplot(data = patientData, 
       aes(ROUTE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Route of administration (primary) vs \nTreatment Completed",
       x = "Route of administration", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs ROUTE2
rout2<- ggplot(data = patientData, 
               aes(ROUTE1, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Route of administration (secondary) vs\n Treatment Completed",
       x = "Route of administration", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

#Target vs ROUTE3
rout3<- ggplot(data = patientData, 
               aes(ROUTE3, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Route of administration (tertiary) vs \nTreatment Completed",
       x = "Route of administration", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

rout <- ggarrange(rout1,rout2,rout3, ncol = 2, nrow = 2)
rout

#Target vs Services
ggplot(data = patientData, 
       aes(SERVICES, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8'),
                   labels = c("Detox, 24-hour\nhospital inpatient","Detox, 24-hour", "Rehab/residential\n(non-detox)",
                              "Rehab/residential,\n(< 30 days)", "Rehab/residential\n(> 30 days)", "intensive\noutpatient",
                              "non-intensive\noutpatient","detoxification"))+
  labs(title = "Type of treatment/service setting at admission\n vs Treatment Completed",
       x = "Type of treatment/service", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))



#Target vs SUB1
subp1 <- patientData %>% filter(SUB1 != 1 & SUB1 != 6 & SUB1 != 8 & SUB1 != 9 & SUB1 <= 10) %>% ggplot(
               aes(x = SUB1, y = as.factor(REASON), fill = as.factor(REASON))) +
  geom_bar(stat = 'identity', 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/\nCrack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  labs(title = "Substance use at admission (primary) vs \nTreatment Completed",
       x = "Substance use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed")) 


#Target vs SUB2
subp2 <- patientData %>% filter(SUB2 != 1 & SUB2 != 6 & SUB2 != 8 & SUB2 != 9 & SUB2 <= 10) %>% ggplot(
  aes(x = SUB2, y = as.factor(REASON), fill = as.factor(REASON))) +
  geom_bar(stat = 'identity', 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/\nCrack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  labs(title = "Substance use at admission (secondary) vs \nTreatment Completed",
       x = "Substance use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c(" Treatment not completed", "Treatment completed"))

#Target vs SUB3
subp3 <- patientData %>% filter(SUB3 != 1 & SUB3 != 6 & SUB3 != 8 & SUB3 != 9 & SUB3 <= 10) %>%
  ggplot( aes(x = SUB3, y = as.factor(REASON), fill = as.factor(REASON))) +
  geom_bar(stat = 'identity', 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/\nCrack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  labs(title = "Substance use at admission (tertiary) vs \nTreatment Completed",
       x = "Substance use", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))

subp <- ggarrange(subp1,subp2, subp3, ncol = 2, nrow = 2)
subp

#Target vs VET
ggplot(data = patientData, 
       aes(VET, fill = REASON)) +
  geom_bar(position = "dodge", 
           alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Veteran Status vs Treatment Completed",
       x = "Veteran Status", y= "Treatment Status")+
  scale_fill_discrete(name="Treatment Status",
                      breaks = c("0","1"),
                      labels = c("Treatment not completed", "Treatment completed"))



#Splitting data into train and test set
set.seed(189028) #1745
##

##
split <- sample.split(patientData$REASON, SplitRatio = 0.8)
train_set <- subset(patientData, split == TRUE)
test_set <- subset(patientData, split == FALSE)
prop.table(table(train_set$REASON))
summary(train_set)

dim(train_set)
dim(test_set)

#Feature Scaling
train_set[,1:29] <- scale(train_set[,1:29])
test_set[,1:29] <- scale(test_set[,1:29])

#Check for imbalanced dataset
barplot(prop.table(table(patientData$REASON)), 
        col = rainbow(2),
        ylim = c(0, 0.7), 
        main = "Class Distribution")
#The dataset is imbalanced

#Oversampling
table(train_set$REASON)
over <- ovun.sample(REASON ~., data = train_set, method = "over", N = 225440)$data
table(over$REASON)

barplot(prop.table(table(over$REASON)), 
        col = rainbow(2),
        ylim = c(0, 0.7), 
        main = "Class Distribution after Oversampling")

#Underfitting
table(train_set$REASON)
under <- ovun.sample(REASON ~., data = train_set, method = "under", N = 83788)$data
table(under$REASON)

#SMOTE
#Both
both <- ovun.sample(REASON~., data = train_set, method = "both", p=0.5, seed = 222, N= 154614)$data
table(both$REASON)

barplot(prop.table(table(both$REASON)), 
        col = rainbow(2),
        ylim = c(0, 0.7), 
        main = "Class Distribution after SMOTE")


#Logistic Regression
#Original dataset
logistic_classifier0 <- glm(formula = REASON ~ ., family = binomial, data = train_set)
prob_predict0 <- predict(logistic_classifier0, type = 'response', newdata = test_set[-30])
y_pred0 <- ifelse(prob_predict0 > 0.5, 1, 0)
summary(logistic_classifier0)

#Confusion Matrix for Logistic Regression
print(cm <- table(test_set[, 30], y_pred0))
print("Logistic Regression")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%")) #18503+8207/38653
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm)) * 100, "%")) #2266+9677

# library(Metrics)
# mae_m = mae(as.numeric(as.character(test_set[[30]])), as.numeric(as.character(y_pred0)))
# rmse_m = rmse(test_set[[30]], y_pred0)
# mae_m
# rmse_m

#Overfitting
logistic_classifier <- glm(formula = REASON ~ ., family = binomial, data = over)
prob_predict <- predict(logistic_classifier, type = 'response', newdata = test_set[-30])
prob_temp <- predict(logistic_classifier, type = 'link', newdata = test_set[-30])
y_pred <- ifelse(prob_predict > 0.5, 1, 0)
summary(logistic_classifier)

#Confusion Matrix for Logistic Regression
print(cm <- table(test_set[, 30], y_pred))
print("Logistic Regression")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%")) #18503+8207/38653
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm)) * 100, "%")) #2266+9677

#SMOTE
lr_both  <- glm(formula = REASON ~ ., family = binomial, data = both)
prob_predict2 <- predict(lr_both, type = 'response', newdata = test_set[-30])
y_pred2 <- ifelse(prob_predict2 > 0.5, 1, 0)
print(cm <- table(test_set[, 30], y_pred2))
print("Logistic Regression")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm)) * 100, "%")) 

summary(lr_both)

sensitivity(cm)
specificity(cm)


#CM plot
library(cvms)
library(tibble)
cfm <- as_tibble(cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "y_pred2",
                      counts_col = "n")

#ROC Curve
library(pROC)
roc <- plot(roc(test_set$REASON, prob_predict2, direction="<"),
     col="red", lwd=3)
legend(0.4, 0.3, round(auc(roc),3), title = "   AUC    ")


plot(lr_both)


plot(REASON ~ LOS, data=both, col="steelblue")
lines(REASON ~ LOS, both, lwd=2)


#K-Nearest Neighbors (KNN)
#Original dataset
knn_classifier0 <- knn(train = train_set[, -30], test = test_set[, -30], cl = train_set[, 30], k = 393, prob = TRUE) #k = 5 = 57%
#Confusion Matrix for KNN
print(knn_cm <- table(test_set[, 30], knn_classifier0))
print("K Nearest Neighbor")
print(paste("Accuracy of the test set: ", (sum(diag(knn_cm))/sum(knn_cm)) * 100, "%")) #16259+6373
print(paste("Error rate of the test set: ", (1-sum(diag(knn_cm))/sum(knn_cm)) *100, "%"))

confusionMatrix(table(test_set[, 30], knn_classifier0))

#Overfitting
knn_classifier <- knn(train = over[, -30], test = test_set[, -30], cl = over[, 30], k = 10, prob = TRUE) #k = 5 = 57%
#Confusion Matrix for KNN
print(knn_cm <- table(test_set[, 30], knn_classifier))
print("K Nearest Neighbor")
print(paste("Accuracy of the test set: ", (sum(diag(knn_cm))/sum(knn_cm)) * 100, "%")) #16259+6373
print(paste("Error rate of the test set: ", (1-sum(diag(knn_cm))/sum(knn_cm)) *100, "%"))


#SMOTE
knn_both <- knn(train = both[, -30], test = test_set[, -30], cl = both[, 30], k = 393, prob = TRUE) #k = 5 = 57%
#Confusion Matrix for KNN
print(knn_cm <- table(test_set[, 30], knn_both))
print("K Nearest Neighbor")
print(paste("Accuracy of the test set: ", (sum(diag(knn_cm))/sum(knn_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(knn_cm))/sum(knn_cm)) *100, "%"))

confusionMatrix(table(test_set[, 30], knn_both))

#CM plot
cfm <- as_tibble(knn_cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "knn_both",
                      counts_col = "n")

#ROC Curve

roc <- plot(roc(test_set$REASON, as.numeric(knn_both), direction="<"),
            col="green", lwd=3)
legend(0.4, 0.3, round(roc$auc,3), title = "        AUC     ")






# #Support Vector Machine (SVM)
# svm_classifier <- svm(formula = REASON ~ ., data = over, type = 'C-classification', kernel = 'linear')
# summary(svm_classifier)
# #Confusion Matrix for SVM
# print(svm_cm <- table(test_set[, 30], svm_classifier))
# print("Support Vector Machine (SVM)")
# #print(paste("Accuracy of the test set: ", (24819+ 4450)/38653 * 100, "%"))
# #print(paste("Error rate of the test set: ", (6023+3361)/38653 * 100, "%"))


#Naive Bayes
#Original dataset
naive_classifier0 <- naiveBayes(x = train_set[-30], y = train_set$REASON)
summary(naive_classifier0)
naive_pred0 <- predict(naive_classifier0, newdata = test_set[-30])
#Confusion Matrix for Navive Bayes
print(naive_cm <- table(test_set[, 30], naive_pred0))
print("Naive Bayes")
print(paste("Accuracy of the test set: ",(sum(diag(naive_cm))/sum(naive_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(naive_cm))/sum(naive_cm)) * 100, "%"))
confusionMatrix(table(test_set[, 30], naive_pred0))


#Overfitting
naive_classifier <- naiveBayes(x = over[-30], y = over$REASON)
summary(naive_classifier)
naive_pred <- predict(naive_classifier, newdata = test_set[-30])
#Confusion Matrix for Navive Bayes
print(naive_cm <- table(test_set[, 30], naive_pred))
print("Naive Bayes")
print(paste("Accuracy of the test set: ",(sum(diag(naive_cm))/sum(naive_cm)) * 100, "%")) #20007+6191
print(paste("Error rate of the test set: ", (1-sum(diag(naive_cm))/sum(naive_cm)) * 100, "%"))

#SMOTE
naive_both <- naiveBayes(x = both[-30], y = both$REASON)
naive_pred1 <- predict(naive_both, newdata = test_set[-30])
#Confusion Matrix for Navive Bayes
print(naive_cm <- table(test_set[, 30], naive_pred1))
print("Naive Bayes")
print(paste("Accuracy of the test set: ",(sum(diag(naive_cm))/sum(naive_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(naive_cm))/sum(naive_cm)) * 100, "%"))

confusionMatrix(table(test_set[, 30], naive_pred1))

naive_both

#CM plot
cfm <- as_tibble(naive_cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "naive_pred1",
                      counts_col = "n")

#ROC
p <- predict(naive_both, test_set[,-30], type = 'raw')
roc <- roc(test_set$REASON, p[,2]) #Treatment completed
auc(roc)
plot(roc, col = c(6))
legend(0.4, 0.3, round(roc$auc,3), title = "        AUC     ")


#Decision Trees
#Original dataset
decision_tree_classifier0 = rpart(REASON~., data = train_set, method = "class")
rpart.plot(decision_tree_classifier0)
predictions0 = predict(decision_tree_classifier0, test_set[,-30], type="class")
print("Decision Tree")
print(dt_cm <- table(predictions0, test_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_cm))/sum(dt_cm)) * 100, "%")) #22444+6525
print(paste("Error rate of the test set: ", (1-sum(diag(dt_cm))/sum(dt_cm)) * 100, "%"))

confusionMatrix(table(test_set[, 30], predictions0))

#Overfitting
decision_tree_classifier = rpart(REASON~., data = over, method = "class")
rpart.plot(decision_tree_classifier)
predictions = predict(decision_tree_classifier, test_set[,-30], type="class")
print("Decision Tree")
print(dt_cm <- table(predictions, test_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_cm))/sum(dt_cm)) * 100, "%")) #22444+6525
print(paste("Error rate of the test set: ", (1-sum(diag(dt_cm))/sum(dt_cm)) * 100, "%"))

#SMOTE
decision_tree_both = rpart(REASON~., data = both, method = "class")
rpart.plot(decision_tree_both)
predictions1 = predict(decision_tree_both, test_set[,-30], type="class")
print("Decision Tree")
print(dt_cm <- table(predictions1, test_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_cm))/sum(dt_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(dt_cm))/sum(dt_cm)) * 100, "%"))

confusionMatrix(table(test_set[, 30], predictions1))

printcp(decision_tree_both)
plotcp(decision_tree_both)

#Pruning decision trees
dt_prune <- prune(decision_tree_both, cp = 0.014)
rpart.plot(dt_prune)
dt_pred = predict(dt_prune, test_set[,-30], type="class")
print("Pruned Decision Tree")
print(dt_prune <- table(dt_pred, test_set$REASON))
print(paste("Accuracy of the test set: ", (sum(diag(dt_prune))/sum(dt_prune)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(dt_prune))/sum(dt_prune)) * 100, "%"))

confusionMatrix(table(test_set[, 30], dt_pred))

#CM plot
cfm <- as_tibble(dt_prune, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "dt_pred", 
                      prediction_col = "...2",
                      counts_col = "n")

#ROC
p <- predict(decision_tree_both, test_set[,-30], type = 'prob')
roc <- roc(test_set$REASON, p[,2]) #Treatment completed
auc(roc)
plot(roc, col = "blue")
legend(0.4, 0.3, round(roc$auc,3), title = "        AUC     ")



#Bagging
library("ipred")
bag_model <- bagging(REASON ~., data = over)
bag_predict <- predict(bag_model, test_set)
print(bag_cm <- with(test_set, table(bag_predict, REASON)))
print(paste("Accuracy of the test set: ", (sum(diag(bag_cm))/sum(bag_cm)) * 100, "%"))
print(paste("Error rate of the test set: ", (1-sum(diag(bag_cm))/sum(bag_cm)) * 100, "%"))


#C5.0
library('C50')
learn_c50 <- C5.0(over[,-30],over$REASON)
c50_pred <- predict(learn_c50, test_set[,-30])
print(cm_c50 <- confusionMatrix(c50_pred, test_set$REASON))
print("C5.0")
print(paste("Accuracy of the test set: ", sum(diag(cm))/sum(cm), "%"))
print(paste("Error rate of the test set: ", 1-sum(diag(cm))/sum(cm), "%"))
plot(learn_c50)

#SMOTE
learn_both <- C5.0(both[,-30],both$REASON)
c50_pred1 <- predict(learn_both, test_set[,-30])
print(cm_c50 <- confusionMatrix(c50_pred1, test_set$REASON))
print("C5.0")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm))* 100, "%"))
print(paste("Error rate of the test set: ", (1-sum(diag(cm))/sum(cm))*100, "%"))
plot(learn_both)


#ROC
p <- predict(learn_both, test_set[,-30], type = 'prob')
roc <- roc(test_set$REASON, p[,2]) #Treatment completed
auc(roc)
plot(roc, col = c(3))
legend(0.4, 0.3, round(auc(roc),4), title = "        AUC     ")


#Random Forest 
#Original dataset
rf_classifier <- randomForest(REASON ~. , data = train_set, ntree = 10)
confusionMatrix(predict(rf_classifier, test_set), test_set$REASON, positive = '1')
rf_pred0 = predict(rf_classifier, test_set[,-30])
rf_cm <- table(test_set[, 30], rf_pred0)
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", 1-sum(diag(rf_cm))/sum(rf_cm), "%"))

print(rf_classifier)


#Overfitting
rf_over <- randomForest(REASON ~. , data = over, ntree = 10)
confusionMatrix(predict(rf_over, test_set), test_set$REASON, positive = '1')
rf_pred = predict(rf_over, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred))
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", 1-sum(diag(rf_cm))/sum(rf_cm) * 100, "%"))

print(rf_over)
#Improved in the number of predicting 1 from 5503 to 5867

#Underfitting
rf_under <- randomForest(REASON ~. , data = under, ntree = 10)
confusionMatrix(predict(rf_under, test_set), test_set$REASON, positive = '1')
rf_pred2 = predict(rf_under, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred2))
print("Random Forest")
print(paste("Accuracy of the test set: ", (sum(diag(cm))/sum(cm)) * 100, "%"))
print(paste("Error rate of the test set: ",(1-sum(diag(cm))/sum(cm)) * 100, "%"))
print(rf_under)
#Loss of data and loss of accuracy 74.77, predicted 1 from 5503 to 8076

#SMOTE
#Both

rf_both <- randomForest(REASON ~. , data = both, ntree = 500)
confusionMatrix(predict(rf_both, test_set), test_set$REASON, positive = '1')
rf_pred_both = predict(rf_both, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred_both))
print("Random Forest")
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(rf_cm))/sum(rf_cm)), "%"))
print(rf_both)

#both <- train_set
rf_pred_both = predict(rf, test_set[,-30], type = 'prob')
roc <- roc(test_set$REASON, rf_pred_both[,2]) #Treatment completed
auc(roc)
plot(roc, col = "dark blue")
legend(0.4, 0.3, round(roc$auc,4), title = "        AUC     ")

#CM plot
cfm <- as_tibble(rf_cm, .name_repair = "unique")
cfm

plot_confusion_matrix(cfm, 
                      target_col = "...1", 
                      prediction_col = "rf_pred_both",
                      counts_col = "n")


model_tuned <- tuneRF(
  x=both[,-30], 
  y=both$REASON, 
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE 
) 

bestmodel <- model_tuned[model_tuned[, 2] == min(model_tuned[, 2]), 1]
print(model_tuned)
print(bestmodel)

rf <-randomForest(REASON~.,data=both, mtry=13, importance=TRUE,ntree=500) 
print(rf)

plot(Random_Forest_Model)



confusionMatrix(predict(rf, test_set), test_set$REASON, positive = '1')

rf_pred_both = predict(rf, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred_both))
print("Random Forest")

print(paste("Accuracy of the test set: ",round((sum(diag(rf_cm))/sum(rf_cm)),2) * 100, "%")) 
print(paste("Error rate of the test set: ", (1-sum(diag(rf_cm))/sum(rf_cm))*100, "%"))


control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(both[,-30]))))
tunegrid
metric <- "Accuracy"
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(18238)
  fit <- train(REASON~., data=both, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


#Evaluate variable importance
Random_Forest_Model <- rf
importance(Random_Forest_Model)
varImpPlot(Random_Forest_Model)





new <- data.frame(AGE = 9, SUB1 = 10, LOS = 5 , SERVICES = 1, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)
predict(rf, newdata=new)

new <- data.frame(AGE = 9, SUB1 = 10, LOS = 7 , SERVICES = 2, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 9, SUB1 = 10, LOS = 5 , SERVICES = 3, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 9, SUB1 = 10, LOS = 19 , SERVICES = 4, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 10, LOS =19, SERVICES = 5, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 4, LOS =32 , SERVICES = 6, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 4, LOS =33 , SERVICES = 7, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 4, LOS =33 , SERVICES = 8, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

###freq3

new <- data.frame(AGE = 4, SUB1 = 10, LOS = 5 , SERVICES = 1, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 10, LOS = 7 , SERVICES = 2, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 10, LOS =15 , SERVICES = 3, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 10, LOS = 19 , SERVICES = 4, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 10, LOS =22, SERVICES = 5, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 1, FREQ3 = 1, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 10, LOS =32 , SERVICES = 6, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 4, LOS =32 , SERVICES = 7, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)

new <- data.frame(AGE = 4, SUB1 = 4, LOS =32 , SERVICES = 8, DSMCRIT = 4, CASEID = 888, ALCDRUG = 3,
                  EDUC = 4, EMPLOY = 3, ETHNIC = 2, FREQ1 = 3 , FREQ2 = 3, FREQ3 = 3, FRSTUSE1 = 3,
                  FRSTUSE2 = 4, FRSTUSE3 = 4, GENDER = 1, LIVARAG = 2, MARSTAT = 1, METHUSE = 2, NOPRIOR = 1,
                  PSOURCE = 1, RACE = 5, ROUTE1 = 1, ROUTE2 = 2, ROUTE3 = 5, SUB2 = 5, SUB3 = 2, VET = 2)

predict(rf, newdata=new)








#Visualize the result
random_forest_model <- rf
varImpPlot(random_forest_model)
varImp(rf_both)
plot(random_forest_model)
print(rf_both)

##
rf_final <- randomForest(REASON ~ LOS + CASEID + SERVICES + AGE + DSMCRIT + FRSTUSE3 + FRSTUSE2+
                         SUB3 + FRSTUSE1 + SUB2 + PSOURCE + EDUC + EMPLOY 
                         + VET + ROUTE2 + ROUTE3 + RACE + FREQ1 + METHUSE + NOPRIOR+
                          MARSTAT+ LIVARAG  , data = both, ntree = 500, mtry=13, importance=TRUE)
confusionMatrix(predict(rf_final, test_set), test_set$REASON, positive = '1')
rf_pred_both = predict(rf_final, test_set[,-30])
print(rf_cm <- table(test_set[, 30], rf_pred_both))
print("Random Forest")
print(paste("Accuracy of the test set: ",(sum(diag(rf_cm))/sum(rf_cm)) * 100, "%")) 
print(paste("Error rate of the test set: ", 1-sum(diag(rf_cm))/sum(rf_cm), "%"))

varImpPlot(rf_final)

#########

#Evaluating model performance using K-Fold cross validation
folds <- createFolds(both$REASON, k = 10)
cv <- lapply(folds, function(x){
  train_fold <- both[-x, ]
  test_fold <- test_set[x, ]
  rf_both <- randomForest(REASON ~. , data = train_fold, ntree = 500)
  rf_pred_both = predict(rf_both, test_fold[,-30])
  rf_cm <- table(test_fold[, 30], rf_pred_both)
  accuracy <- (rf_cm[1,1]+rf_cm[2,2])/(rf_cm[1,1]+rf_cm[2,2]+ rf_cm[1,2]+rf_cm[2,1])
  return(accuracy)
})

cv1 <-mean(as.numeric(cv))
cv1

accuracy < - mean(as.numeric(cv))

library(plyr)
classifier_rf <- train(form = REASON~., data = both, method = "rfRules")
classifier_rf
classifier$bestTune
#add mtry and maxdepth values in the main algorithm

#########
library(xgboost)
xg_model <- xgboost(data = as.matrix(both[-30]), label = both$REASON, nrounds = 1000)

folds <- createFolds(both$REASON, k = 5)
cv <- lapply(folds, function(x){
  train_fold <- both[-x, ]
  test_fold <- test_set[x, ]
  rf_both <- xgboost(data = as.matrix(both[-30]), label = both$REASON, nrounds = 10, 
                     max.depth = 3)
  rf_pred_both <- predict(rf_both, newdata = as.matrix(test_fold[,-30]))
  rf_pred_both <- (rf_pred_both >= 0.5)
  rf_cm <- table(test_fold[, 30], rf_pred_both)
  accuracy <- (sum(diag(rf_cm))/sum(rf_cm))
  return(accuracy)
})

cv1 <-mean(as.numeric(cv))
cv1



#Length of stay vs other factors
#1 LOS vs Service and reason for discharge

Dataset %>%
  ggplot(aes(x = SERVICES, y = LOS, fill = as.factor(REASON))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Length of stay w.r.t reason of discharge\n and treament type",
       x = "Treatment type", y= "Length of stay (LOS) in days")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8"),
                   labels = c("Hospital\nInpatient", "Free-standing\nresidential",
                              "Hospital\nnon detox", "short-term\n< 30", "long-term\n> 30",
                              "Intensive\noutpatient", "Non-Intensive\noutpatient","Detoxification"))+
  scale_fill_discrete(name="Reason for discharge",
                      breaks = c("1","2","3","4","5","6","7"),
                      labels = c("Treatment Completed", "Dropped out", "Terminated by facility",
                                 "Transferred", "Incarcerated", "Death", "Other"))

#2 Pie chart to view type of treatments administered at max based on treatment completed
ser = c("Hospital\nInpatient", "Free-standing\nresidential",
           "Hospital\nnon detox", "short-term\n< 30", "long-term\n> 30",
           "Intensive\noutpatient", "Non-Intensive\noutpatient","Detoxification")
pie(table(Dataset$SERVICES), labels= ser, col=rainbow(length(ser)), main="Type of treatment service at discharge")

#3 LOS and alcohol dependence/ other SUD and completing treatment
#LOS VS SUB1

Dataset %>%
  filter((SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) & REASON == 1) %>%
  ggplot(aes(x = SUB1, y = LOS, fill = as.factor(SUB1))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Length of stay w.r.t alcohol and other substance use ",
       x = "Type of Substance used", y= "Length of stay (LOS) in days")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  scale_fill_discrete(name="Type of SUD",
                      breaks = c("1","2","3","4","5","6","7",'8','9',"10"),
                      labels = c("","Alcohol", "Cocaine/Crack",
                                 "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                                 "Methamphetamine/speed"))


Dataset %>%
  filter(SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) %>%
  ggplot(aes(x = SUB1, y = REASON, fill = as.factor(REASON))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Substance use at primary ",
       x = "Type of Substance used", y= "Length of stay (LOS) in days")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  scale_fill_discrete(name="Type of SUD",
                      breaks = c("1","2","3","4","5","6","7",'8','9',"10"),
                      labels = c("","Alcohol", "Cocaine/Crack",
                                 "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                                 "Methamphetamine/speed"))


#5 Race vs Daywait to enter facility 



#11 freq1 vs firstuse1
Dataset %>%
  filter(SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) %>%
  ggplot(aes(x = SUB1, y = AGE, fill = as.factor(GENDER))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Age at first substance abuse with gender",
       x = "Substance type", y = "Age")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  # scale_y_continuous(limits = c("1","2","3","4","5","6","7",'8','9',"10", "11","12"),
  #                  labels = c("12-14", "15-17","18-20","21-24","25-29","30-34","35-39","40-44",
  #                             "45-49","50-54","55-64","65 >"))+
  scale_fill_discrete(name="Reason for discharge",
                      breaks = c("1","2"),
                      labels = c("Male","Female"))

#12 freq los
Dataset%>%
  filter((SUB1 == 2 | SUB1 == 3 |SUB1 == 4 |SUB1 == 5 |SUB1 == 7 |SUB1 == 10) & FREQ1 != -9) %>%
  ggplot( aes(x= as.factor(SUB1), y= LOS, fill=as.factor(FREQ1))) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "Frequency of Substance intake before entering the facility with time spent",
       x = "Substance type", y = "Length of stay at facility")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7",'8','9',"10"),
                   labels = c("","Alcohol", "Cocaine/Crack",
                              "Marijuana", "Heroin", "","Other opiates\n/synthetics","","",
                              "Methamphetamine/speed"))+
  scale_fill_discrete(name="Prior usage",
                      breaks = c("1","2","3"),
                      labels = c("No use in the past month","Some use","Daily Use"))

##
Dataset %>% filter(ETHNIC != -9 & DAYWAIT != -9) %>%
ggplot( aes(x = DAYWAIT,
           fill = as.factor(ETHNIC))) +
  geom_density(alpha = 0.4) +
  labs(title = "Daywait for Race and ethnicity ")

##


patientData$AGE <- as.factor(ifelse(patientData$AGE == 1, '12-14', 
                                    ifelse(patientData$AGE == 2, '15-17',
                                           ifelse(patientData$AGE == 3, '18-20',
                                                  ifelse(patientData$AGE == 4,'21-24',
                                                         ifelse(patientData$AGE == 5,'25-29',
                                                                ifelse(patientData$AGE == 6,'30-34',
                                                                       ifelse(patientData$AGE == 7,'35-39',
                                                                              ifelse(patientData$AGE == 8,'40-44',
                                                                                     ifelse(patientData$AGE == 9,'45-49',
                                                                                            ifelse(patientData$AGE == 10,'50-54',
                                                                                                   ifelse(patientData$AGE == 11, '55-64',
                                                                                                          ifelse(patientData$AGE == 12,'65 >','20')))))))))))))



patientData$ALCDRUG <- as.factor(ifelse(patientData$ALCDRUG == 0,'None',
                                        ifelse(patientData$ALCDRUG ==  1, 'Alcohol only',
                                        ifelse(patientData$ALCDRUG == 2, 'Other drugs only',
                                        ifelse(patientData$ALCDRUG  == 3,'Both', 'Both')))))


patientData$FRSTUSE1 <- as.factor(ifelse(patientData$FRSTUSE1 == 1, '11 years and under',
                                         ifelse(patientData$FRSTUSE1 == 2,'12–14 years',
                                                ifelse(patientData$FRSTUSE1 == 3,'15–17 years',
                                                       ifelse(patientData$FRSTUSE1 == 4,'18–20 years',
                                                              ifelse(patientData$FRSTUSE1 == 5,'21–24 years', 
                                                                     ifelse(patientData$FRSTUSE1 == 6,'25–29 years', 
                                                                            ifelse(patientData$FRSTUSE1 == 7,'30–95 years', '15–17 years'))))))))


patientData$FRSTUSE2 <- as.factor(ifelse(patientData$FRSTUSE2 == 1, '11 years and under',
                                         ifelse(patientData$FRSTUSE2 == 2,'12–14 years',
                                                ifelse(patientData$FRSTUSE2 == 3,'15–17 years',
                                                       ifelse(patientData$FRSTUSE2 == 4,'18–20 years',
                                                              ifelse(patientData$FRSTUSE2 == 5,'21–24 years', 
                                                                     ifelse(patientData$FRSTUSE2 == 6,'25–29 years', 
                                                                            ifelse(patientData$FRSTUSE2 == 7,'30–95 years', '15–17 years'))))))))
patientData$FRSTUSE3 <- as.factor(ifelse(patientData$FRSTUSE3 == 1, '11 years and under',
                                         ifelse(patientData$FRSTUSE3 == 2,'12–14 years',
                                                ifelse(patientData$FRSTUSE3 == 3,'15–17 years',
                                                       ifelse(patientData$FRSTUSE3 == 4,'18–20 years',
                                                              ifelse(patientData$FRSTUSE3 == 5,'21–24 years', 
                                                                     ifelse(patientData$FRSTUSE3 == 6,'25–29 years', 
                                                                            ifelse(patientData$FRSTUSE3 == 7,'30–95 years', '15–17 years'))))))))
patientData$FREQ1<- as.factor(ifelse(patientData$FREQ1 == 1 ,'No use',
                                     ifelse(patientData$FREQ1 == 2,'Some use',
                                            ifelse(patientData$FREQ1 == 3,'Daily use' ,'Daily use'))))

patientData$FREQ2<- as.factor(ifelse(patientData$FREQ2 == 1 ,'No use',
                                     ifelse(patientData$FREQ2 == 2,'Some use',
                                            ifelse(patientData$FREQ2 == 3,'Daily use' ,'Daily use'))))
patientData$FREQ3<- as.factor(ifelse(patientData$FREQ3 == 1 ,'No use',
                                     ifelse(patientData$FREQ3 == 2,'Some use',
                                            ifelse(patientData$FREQ3 == 3,'Daily use' ,'Daily use'))))

patientData$METHUSE <- as.factor(ifelse(patientData$METHUSE == 1,'Yes', 
                                        ifelse(patientData$METHUSE == 2,'No' ,1)))
patientData$ROUTE1 <- as.factor(ifelse(
  patientData$ROUTE1 == 1 , 'Oral', 
  ifelse(
    patientData$ROUTE1 == 2 ,'Smoking',
    ifelse(
      patientData$ROUTE1 == 3 ,'Inhalation',
      ifelse(
        patientData$ROUTE1 == 4 ,'Injection',
        ifelse(
          patientData$ROUTE1 == 5 , 'Other', 'Oral' ))))))

patientData$ROUTE2 <- as.factor(ifelse(
  patientData$ROUTE2 == 1 , 'Oral', 
  ifelse(
    patientData$ROUTE2 == 2 ,'Smoking',
    ifelse(
      patientData$ROUTE2 == 3 ,'Inhalation',
      ifelse(
        patientData$ROUTE2 == 4 ,'Injection',
        ifelse(
          patientData$ROUTE2 == 5 , 'Other', 'Oral' ))))))

patientData$ROUTE3 <- as.factor(ifelse(
  patientData$ROUTE3 == 1 , 'Oral', 
  ifelse(
    patientData$ROUTE3 == 2 ,'Smoking',
    ifelse(
      patientData$ROUTE3 == 3 ,'Inhalation',
      ifelse(
        patientData$ROUTE3 == 4 ,'Injection',
        ifelse(
          patientData$ROUTE3 == 5 , 'Other', 'Oral' ))))))


patientData$NOPRIOR <- as.factor(ifelse(patientData$NOPRIOR == 0, 'No prior treatment',
                                        ifelse(patientData$NOPRIOR == 1, '1 or more treatments','1 or more treatments')))



patientData$SUB1 <- as.factor(ifelse(patientData$SUB1 == 1, 'None',
                                     ifelse(patientData$SUB1 == 2 , 'Alcohol',
                                            ifelse(patientData$SUB1 == 3 , 'Cocaine/Crack',
                                                   ifelse(patientData$SUB1 == 4, 'Marijuana',
                                                          ifelse(patientData$SUB1 == 5 ,'Heroin',
                                                                 ifelse(patientData$SUB1 == 7 ,'Opiates/synthetics',
                                                                        ifelse(patientData$SUB1 == 10 ,'Methamphetamine','Alcohol' ))))))))

patientData$SUB2 <- as.factor(ifelse(patientData$SUB2 == 1, 'None',
                                     ifelse(patientData$SUB2 == 2 , 'Alcohol',
                                            ifelse(patientData$SUB2 == 3 , 'Cocaine/Crack',
                                                   ifelse(patientData$SUB2 == 4, 'Marijuana',
                                                          ifelse(patientData$SUB2 == 5 ,'Heroin',
                                                                 ifelse(patientData$SUB2 == 7 ,'Opiates/synthetics',
                                                                        ifelse(patientData$SUB2 == 10 ,'Methamphetamine','Alcohol' ))))))))
patientData$SUB3 <- as.factor(ifelse(patientData$SUB3 == 1, 'None',
                                     ifelse(patientData$SUB3 == 2 , 'Alcohol',
                                            ifelse(patientData$SUB3 == 3 , 'Cocaine/Crack',
                                                   ifelse(patientData$SUB3 == 4, 'Marijuana',
                                                          ifelse(patientData$SUB3 == 5 ,'Heroin',
                                                                 ifelse(patientData$SUB3 == 7 ,'Opiates/synthetics',
                                                                        ifelse(patientData$SUB3 == 10 ,'Methamphetamine','Alcohol' ))))))))


patientData$DSMCRIT <- as.factor(ifelse(patientData$DSMCRIT == 1, 'Alcohol-induced disorder',
                                        ifelse(patientData$DSMCRIT == 2,'Substance-induced disorder',
                                               ifelse(patientData$DSMCRIT == 3,'Alcohol intoxication',
                                                      ifelse(patientData$DSMCRIT == 4,'Alcohol dependence',
                                                             ifelse(patientData$DSMCRIT == 5,'Opioid dependence',
                                                                    ifelse(patientData$DSMCRIT == 6,'Cocaine dependence',
                                                                           ifelse(patientData$DSMCRIT == 7, 'Cannabis dependence',
                                                                                  ifelse(patientData$DSMCRIT == 8, 'Other substance dependence',
                                                                                         ifelse(patientData$DSMCRIT == 9, 'Alcohol abuse',
                                                                                                ifelse(patientData$DSMCRIT == 10, 'Cannabis abuse','Alcohol-induced disorder')))))))))))





patientData$PSOURCE <- as.factor(ifelse(patientData$PSOURCE == 1,'Individual (includes self- referral)',
                                        ifelse(patientData$PSOURCE == 2,'Alcohol/drug use care provider',
                                               ifelse(patientData$PSOURCE == 3,'Other health care provider',
                                                      ifelse(patientData$PSOURCE == 4,'School',
                                                             ifelse(patientData$PSOURCE == 5,'Employee',
                                                                    ifelse(patientData$PSOURCE == 6,'Community referral',
                                                                           ifelse(patientData$PSOURCE == 7, 'Court/criminal/DWI/DUI', 'Individual (includes self- referral)'))))))))




patientData$SERVICES <- as.factor(ifelse(patientData$SERVICES == 1,'Detox 24 hours Inpatient', 
                                         ifelse(patientData$SERVICES == 2,'Detox 24-hours free-standing',
                                                ifelse(patientData$SERVICES == 3,'Rehab non detox',
                                                       ifelse(patientData$SERVICES == 4,'short-term < 30', 
                                                              ifelse(patientData$SERVICES == 5, 'long-term > 30',
                                                                     ifelse(patientData$SERVICES == 6,'Intensive outpatient', 
                                                                            ifelse(patientData$SERVICES == 7,'Non-Intensive outpatient',
                                                                                   ifelse(patientData$SERVICES == 8, 'Detoxification','Non-Intensive outpatient'
                                                                                                                                                                                   )))))))))
patientData$VET <- as.factor(ifelse(patientData$VET == 1, 'Yes',
                                 ifelse(patientData$VET == 2,'No' ,'No')))

patientData$GENDER <- as.factor(ifelse(patientData$GENDER == 1,'Male',
                                       ifelse(patientData$GENDER == 2,  'Female', 'Male')))


patientData$LIVARAG <- as.factor(ifelse(patientData$LIVARAG == 1, 'Homeless',
                                        ifelse(patientData$LIVARAG == 2,'Dependent Living',
                                               ifelse(patientData$LIVARAG == 3,'Independent Living','Independent Living'))))
                                     

patientData$EMPLOY <- as.factor(ifelse(patientData$EMPLOY == 1,'Full time',
                                       ifelse(patientData$EMPLOY == 2, 'Part time', 
                                              ifelse(patientData$EMPLOY == 3,'Unemployed', 
                                                     ifelse(patientData$EMPLOY == 4,'Not in labor','Not in labor')))))
                                  

  
patientData$ETHNIC <- as.factor(ifelse(patientData$ETHNIC == 1,'Puerto Rican', 
                                       ifelse(patientData$ETHNIC == 2,'Mexican', 
                                              ifelse(patientData$ETHNIC == 3,'Cuban', 
                                                     ifelse(patientData$ETHNIC == 4,'Not Hispanic/Latino',
                                                            ifelse(patientData$ETHNIC == 5, 'Hispanic/Latino', 'Puerto Rican'))))))



patientData$RACE <- as.factor(ifelse(patientData$RACE == 1,'Alaska Native',
                                     ifelse(patientData$RACE == 2,'American Indian/other',
                                            ifelse(patientData$RACE == 3,'Asian or Pacific',
                                                   ifelse(patientData$RACE == 4,'Black or African American',
                                                          ifelse(patientData$RACE == 5,'White',
                                                                 ifelse(patientData$RACE == 6,'Asian', 
                                                                        ifelse(patientData$RACE == 7, 'Other single races',
                                                                               ifelse(patientData$RACE == 8,'Two or more races', 
                                                                                      ifelse(patientData$RACE == 9, 'Native Hawaiian' ,'Alaska Native'

                                                                                     ))))))))))
                                                                                            
                                     
patientData$MARSTAT <-  as.factor(ifelse(patientData$MARSTAT == 1,'Never married',
                                         ifelse(patientData$MARSTAT == 2, 'Now married', 
                                                ifelse(patientData$MARSTAT == 3,'Separated',
                                                       ifelse(patientData$MARSTAT == 4, 'Widowed/Divorced','Never married')))))



patientData$EDUC <- as.factor(ifelse(patientData$EDUC == 1,'No school/ kg – grade 8',
                                     ifelse(patientData$EDUC == 2,'Grades 9 – 11',
                                             ifelse(patientData$EDUC == 3,'Grade 12/(GED)',
                                                     ifelse(patientData$EDUC == 4,'1-3 years of college',
                                                             ifelse(patientData$EDUC == 5, '4+ years of college','Grade 12/(GED)'))))))

patientData$REASON <- as.factor(ifelse(patientData$REASON == 1, 'Treatment Succesful',
                                       ifelse(patientData$REASON == 0 ,'Treatment Failure','Treatment Failure')))

str(test_set$REASON)
str(both$REASON)

