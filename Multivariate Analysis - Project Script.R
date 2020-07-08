#Import HR.txt dataset as data
data <- read.table('HR.txt', header = TRUE)
data <- data.frame(data)
summary(data)
head(data)
str(data)

######Data Cleaning#######

#Excluding individuals with missing variables
data<-data[!(data$satisfaction_level=="XXXX"),]
data<-data[!(data$average_montly_hours=="XXXX"),]
data<-data[!(data$time_spend_company=="XXXX"),]

#Converting employee characteristics into continuous variable
data$satisfaction_level <- as.numeric(as.character(data$satisfaction_level))
data$average_montly_hours<- as.numeric(as.character(data$average_montly_hours))
data$time_spend_company<- as.numeric(as.character(data$time_spend_company))
data$left<- as.factor(data$left) #converting 'left' variable as a factor

#checking outlier in each continuous variable
boxplot(data$satisfaction_level,data=data, main="Boxplot",xlab="satisfaction_level", ylab="Years")
boxplot(data$last_evaluation,data=data, main="Boxplot",xlab="last_evaluation", ylab="Years")
boxplot(data$number_project,data=data, main="Boxplot",xlab="number_project", ylab="Years")
boxplot(data$average_montly_hours,data=data, main="Boxplot",xlab="average_montly_hours", ylab="Years")
boxplot(data$time_spend_company,data=data, main="Boxplot",xlab="time_spend_company", ylab="Years")

#Standarding data
data[,1:5] <- scale(data[,1:5])
summary(data)

############### TASK 1 ########################

#Table to filter left variable department wise
xtabs(~ data$department+data$left, data)


#Filtering employees remaining with company
data_empl_remain <- data[(data$left==0),]
xtabs(~data_empl_remain$department+data_empl_remain$salary, data) #No of employees with company - salarywise


#Calculating means for first 4 contimuous variable by department
satisfaction_level_means<-tapply(data_empl_remain$satisfaction_level,data_empl_remain$department, mean)
last_evaluation_means<-tapply(data_empl_remain$last_evaluation,data_empl_remain$department, mean)
number_project_means<-tapply(data_empl_remain$number_project,data_empl_remain$department, mean)
average_montly_hours_means<-tapply(data_empl_remain$average_montly_hours,data_empl_remain$department, mean)
departmentMeans<-data.frame(satisfaction_level_means,last_evaluation_means,number_project_means,average_montly_hours_means)
departmentMeans

#extrcting unique values of department to label
department <- unique(data_empl_remain[['department']])
department


#Distance matrix for first 4 variables
dist_mat <- round(dist(departmentMeans,method = "maximum", diag = FALSE, upper = FALSE, p = 2), digits = 4)
dist_mat

# Cluster Analysis for Hierarchical Nature
departmentMeans.hc <- hclust(dist_mat, method="single")
plot(departmentMeans.hc, hang=-1)


# Ordination Analysis for Spacial nature

library(vegan)

# 2-D Ordination 
(fit1 <- cmdscale(dist_mat,eig=TRUE, k=2))

x <- fit1$points[,1]
y <- fit1$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2", main="Metric MDS", type="n")
text(x, y, labels =department, cex=.7)

# 3-D Ordination
(fit2 <- cmdscale(dist_mat,eig=TRUE, k=3))

x <- fit2$points[,1]
y <- fit2$points[,2]
y2 <- fit2$points[,3]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2", main="Metric MDS", type="n") # Dimension 1 and Dimension 2 plot
text(x, y, labels =department, cex=.7)

plot(x, y2, xlab="Dimension 1", ylab="Dimension 3", main="Metric MDS", type="n") # Dimension 1 and Dimension 3 plot
text(x, y2, labels =department, cex=.7)





############################ TASK 2 #############################

#Filtering current employees in managemnt department
cur_mangmt_empl <- data[(data$left==0 & data$department == "management"),]
summary(cur_mangmt_empl)

# Split Testing and Training data
set.seed(1107)
library(caret)
Train <- createDataPartition(y=cur_mangmt_empl$salary,p=0.80,list = FALSE) #80/20 ratio split for training and testing data
TrainData<-cur_mangmt_empl[Train,]
TestData <- cur_mangmt_empl[-Train,]
summary(TestData)
table(TestData$salary) # Frequency table Testing data
table(TrainData$salary) # Frequency table Training data

# Discriminant Function Analysis (DFA)
# Using gievn first 4 variables
library(MASS)
(TrainData.lda_1<-lda(salary~satisfaction_level+last_evaluation+number_project+average_montly_hours, data=TrainData)) 

salary_pred_1<-predict(TrainData.lda_1, TestData) #predicts salary for test set
salary_pred_1
table(TestData$salary, salary_pred_1$class) #compares actual to predict

# Using gievn first \5 variables
(TrainData.lda_2<-lda(salary~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company, data=TrainData)) 

salary_pred_2<-predict(TrainData.lda_2, TestData) #predicts salary for test set
salary_pred_2
table(TestData$salary, salary_pred_2$class) #compares actual to predict


# Using different combinations of given variables
(TrainData.lda_3<-lda(salary~satisfaction_level+last_evaluation+number_project, data=TrainData)) 

salary_pred_3<-predict(TrainData.lda_3, TestData) #predicts salary for test set
salary_pred_3
table(TestData$salary, salary_pred_3$class) #compares actual to predict

#plot DFs by original salary categories
data.temp<-data.frame(salary_pred_1$x, class=TestData$salary)
xyplot(LD1~LD2, data=data.temp, groups=class,auto.key = list(title="Salary Observed", space = "top", cex=1.0))

#plot DFs by predicted salary categories
data.temp<-data.frame(salary_pred_1$x, class=salary_pred_1$class)
xyplot(LD1~LD2, data=data.temp, groups=class,auto.key=list(title="Salary Predicted", space = "top", cex=1.0))


# Estimating p-values to find significant difference between salary groups.
#HotellingsT2Test
library(DescTools)
#Comparing low and medium salary groups
G1<-subset(TestData, TestData$salary=="low"|TestData$salary=="medium") 
(HotellingsT2Test(cbind(G1$satisfaction_level, G1$last_evaluation, G1$number_project, G1$average_montly_hours,G1$time_spend_company) ~ G1$salary, data=G1))

#Comparing low and high salary groups
G2<-subset(TestData, TestData$salary=="low"|TestData$salary=="high")
(HotellingsT2Test(cbind(G2$satisfaction_level, G2$last_evaluation, G2$number_project, G2$average_montly_hours,G2$time_spend_company) ~ G2$salary, data=G2))

#Comparing high and medium salary groups
G3<-subset(TestData, TestData$salary=="high"|TestData$salary=="medium")
(HotellingsT2Test(cbind(G3$satisfaction_level, G3$last_evaluation, G3$number_project, G3$average_montly_hours,G3$time_spend_company) ~ G3$salary, data=G3))













