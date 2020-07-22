attrition<-read.csv("path/Attrition.CSV")
arg<-read.csv("path/Attrition.CSV")
att<-read.csv("path/Attrition.CSV")
suppressPackageStartupMessages

library(tidyverse)
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(lightgbm))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
                               suppressPackageStartupMessages(library(treemapify))
                               suppressPackageStartupMessages(library(repr))
                               suppressPackageStartupMessages(library(cowplot))
                               suppressPackageStartupMessages(library(magrittr))
                               suppressPackageStartupMessages(library(ggpubr))
                               suppressPackageStartupMessages(library(RColorBrewer))
                               suppressPackageStartupMessages(library(plotrix))
                               suppressPackageStartupMessages(library(ggrepel))
                               suppressPackageStartupMessages(library(forcats))
                               suppressPackageStartupMessages(library(reshape2))
                               suppressPackageStartupMessages(library(caTools))
                               suppressPackageStartupMessages(library(tree))
                               suppressPackageStartupMessages(library(rattle))
                               
                               options(repr.plot.width=8, repr.plot.height=6)
                               options(warn=-1)                               

                               
                               
                               
                               
                               attritions_number <- attrition %>% group_by(Attrition) %>% summarise(Count=n()) %>%
                                 ggplot(aes(x=Attrition, y=Count)) + geom_bar(stat="identity", fill="orange", color="grey40") + theme_fivethirtyeight() + coord_flip() + 
                                 geom_text(aes(x=Attrition, y=0.01, label= Count),
                                           hjust=-0.8, vjust=-1, size=3, 
                                           colour="black", fontface="bold",
                                           angle=360) + labs(title="Employee Attrition (Amount)", x="Employee Attrition",y="Amount") + theme(plot.title=element_text(hjust=0.5))
                               
                               attrition_percentage <- attrition %>% group_by(Attrition) %>% summarise(Count=n()) %>% 
                                 mutate(pct=round(prop.table(Count),2) * 100) %>% 
                                 ggplot(aes(x=Attrition, y=pct)) + geom_bar(stat="identity", fill = "dodgerblue", color="grey40") + 
                                 geom_text(aes(x=Attrition, y=0.01, label= sprintf("%.2f%%", pct)),
                                           hjust=0.5, vjust=-3, size=4, 
                                           colour="black", fontface="bold") + theme_bw() + labs(x="Employee Attrition", y="Percentage") + 
                                 labs(title="Employee Attrition (%)") + theme(plot.title=element_text(hjust=0.5))

                               plot_grid(attritions_number, attrition_percentage, align="h", ncol=2)                               
                               
                               library(ggthemes)
                               
                               
                  q=as.integer(attrition$Age)             
                               library(ggplot2)
                               theme_set(theme_classic())
                               df<-data.frame()
                               # Histogram on a Categorical variable
                               g <-ggplot(df$Age , aes(attrition$JobRole))
                               g + geom_bar(aes(fill=attrition$JobSatisfaction), width = 0.5) + 
                                 theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
                                 labs(title="Histogram on Categorical Variable", 
                                      subtitle="Jobrole") 
                               
                               
                         df<-as.data.frame(attrition, row.names = NULL, optional = FALSE,
                                             cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
                                             stringsAsFactors = default.stringsAsFactors())
                         
                         
                         
                         
                         age_job <- aggregate(attrition$Age, by=list(attrition$JobRole), FUN=mean)  # aggregate
                         colnames(age_job) <- c("job role", "Age")  # change column names
                         age_job <- age_job[order(age_job$Age), ]  # sort
                         age_job $ 'job role' <- factor(age_job$'job role' , levels = age_job$'job role')  # to retain the order in plot.
                         head(age_job, 4)
                         
                         
                         
                         
                         age_job$'job role'
                         
                         
                         
                         
                         
                         library(ggplot2)
                         theme_set(theme_bw())
                         
                        
                         
                         
                         
                         
                         #Draw plot
                         ggplot(age_job, aes(x=age_job$'job role', y=age_job$Age)) +
                           geom_bar(stat="identity", width=.5, fill="tomato3") + 
                           labs(x="JobRole",y="Age",title="Ordered Bar Chart", 
                                subtitle=" avg Age Vs  Job Role", 
                                caption="source:Attrition") + 
                           theme(axis.text.x = element_text(angle=65, vjust=0.6))
                         
                         
                         
                         options(repr.plot.width=8, repr.plot.height=6)
                         options(warn=-1)             
                       f<- attrition %>% select(JobRole,Attrition)%>% group_by(Attrition,JobRole)%>%summarise(Count=n()) %>% 
                         mutate(percentage=round(prop.table(Count),2) * 100) %>% 
                         ggplot(aes(x=JobRole, y=percentage,fill=Attrition)) + geom_bar(stat="identity") + coord_flip()
                         geom_text(aes(x=JobRole, y=0.01),
                                   hjust=0.5, vjust=-3, size=4, 
                                   colour="black", fontface="bold") + theme_bw() + labs(x="JobRole", y="Percentage") + 
                         labs(title="Employee Attrition (%)i Jobrole") + theme(plot.title=element_text(hjust=0.5))
                       
                       plot_grid(f, ncol=1)                               
                       
                         
                        
                         
                         
                         bar <- ggplot(data = attrition) + 
                           geom_bar(
                             mapping = aes(x = as.factor(attrition$JobSatisfaction), fill = as.factor(attrition$JobSatisfaction)), 
                             show.legend = FALSE,
                             width = 1
                           ) + 
                           theme(aspect.ratio = 1) +
                           labs(x = "Job satisficarion", y ="count" )
                         
                         bar + coord_flip()
                         bar + coord_polar()
                         
                         
                         library(dplyr)
                         
                         library(ggthemes)
                         # Boxplot with attrition in the X-axis and Job Satisfaction in the y-Axis
                         options(repr.plot.width=8, repr.plot.height=6) 
                         
                         
                         box.attrition <- attrition %>% select(Attrition, JobRole, Gender) %>% 
                           ggplot(aes(x=Attrition, y=JobRole, fill=Attrition)) + geom_boxplot(color="black") + theme_minimal() + facet_wrap(~Gender) + 
                           scale_fill_manual(values=c("#FA5858", "#9FF781"))
                         
                         library(cowplot)
                         # Distribution of Job Satisfaction
                         dist.role <- attrition %>% select(JobRole) %>%
                           ggplot(aes(x=JobRole)) + geom_density(color="#013ADF", fill="#81BEF7", trim=TRUE) + theme_tufte() + xlim(range(c(1,4)))
                         
                         
                         
                         plot_grid(box.attrition)

                         
                         library(dplyr)
                         # Boxplot with attrition in the X-axis and Job Satisfaction in the y-Axis
                         options(repr.plot.width=8, repr.plot.height=6) 
                         attrition$JobInvolvement<-as.integer(attrition$JobInvolvement)
                         
                         box.attrition <- attrition %>% select(Attrition, JobInvolvement, Gender) %>% 
                           ggplot(aes(x=Attrition, y=JobInvolvement, fill=Attrition)) + geom_boxplot(color="black") + theme_minimal() + facet_wrap(~Gender) + 
                           scale_fill_manual(values=c("darkorange", "cadetblue"))
                         dist.JobInvolvement <- attrition %>% select(JobInvolvement) %>%
                           ggplot(aes(x=JobInvolvement)) + geom_density(color="cyan1", fill="darkgoldenrod", trim=TRUE) + theme_bw() + xlim(range(c(1,4)))
                          
                         plot_grid(box.attrition,dist.JobInvolvement)
                         
                          boxplot(attrition$MonthlyIncome,color="red",horizontal = T)
                         
                         # Distribution of Job Satisfaction
                         dist.MonthlyIncome <- attrition %>% select(MonthlyIncome,Attrition) %>%
                           ggplot(aes(x=MonthlyIncome)) + geom_density(aes(color=Attrition),  trim=TRUE) + theme_bw() + xlim(range(c(1:75000)))
                         
                         options(repr.plot.width=60, repr.plot.height=30) 
                         genderincome <- attrition  %>% select(Gender,MonthlyIncome,Attrition) %>% group_by(Attrition,Gender) %>% summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
                           ggplot(aes(x=Gender, y=avg_income,fill=Attrition)) + geom_bar(stat="identity",  color="grey40",position="dodge")   + facet_wrap(~Attrition) +
                          theme_minimal() + theme(axis.text.x = element_text(angle = 90),plot.title=element_text(size=14, hjust=0.5))+
                         scale_fill_manual(values=c("cyan1", "tomato2")) + 
                           labs(y="Average Income", x="Gender", title="Average Income by Department \n and Attrition Status") + 
                         geom_text(aes(x=Gender, y=0.01, label= paste0("???", round(avg_income,2))),
                                   hjust=-0.5, vjust=0, size=3, 
                                   colour="black", fontface="bold",
                                   angle=90)
                          plot_grid(genderincome,dist.MonthlyIncome, ncol=2, nrow=1)
                              
                          library(plotly)
                          
                          p <- plot_ly(data = attrition, x = ~MonthlyIncome, y = ~Age, color = ~Attrition, colors = "Set1")
                          
                                                    
                          
                          p
                         
                          
                          # Change histogram plot line colors by groups
                          ggplot(attrition, aes(x=MonthlyIncome, color=Attrition)) +
                            geom_histogram(fill="white")
                          # Overlaid histograms
                          ggplot(attrition, aes(x=MonthlyIncome, color=Attrition)) +
                            geom_histogram(fill="white", alpha=0.5, position="identity")
                          min(attrition$MonthlyIncome)
                          max(attrition$MonthlyIncome)
                          
                          library(scales)
                          library(stringi)
                          library(ggplot2)
                          library(dplyr)
                          library(cowplot)
                           
                          options(repr.plot.width=60, repr.plot.height=30) 
                          Jobwise <- attrition  %>% select(JobRole,Attrition,MonthlyIncome) %>% group_by(Attrition,JobRole) %>% summarise(count=n(),salary=round(mean(MonthlyIncome))) %>%
                            ggplot(aes(x=JobRole, y=count,fill=Attrition)) + geom_bar(stat="identity",  color="grey40",position="dodge")   + facet_wrap(~Attrition) +
                            theme_minimal() + theme(axis.text.x = element_text(angle = 90),plot.title=element_text(size=14, hjust=0.5))+
                            scale_fill_manual(values=c("cyan1", "tomato2")) + 
                            labs(y="count", x="Gender", title="Average Income by Department \n and Attrition Status") + 
                            geom_text(aes(x=JobRole, y=0.01, label= paste0("???", round(salary,2))),
                                      hjust=-0.5, vjust=0, size=3, 
                                      colour="black", fontface="bold",
                                      angle=90)
                          plot_grid(Jobwise, ncol=1, nrow=1)
                          
                          
                          businesstravel <- attrition  %>% select(BusinessTravel,Attrition,MonthlyIncome) %>% group_by(Attrition,BusinessTravel) %>% summarise(count=n(),salary=round(mean(MonthlyIncome))) %>%
                            
                            ggplot(aes(x=BusinessTravel, y=count,fill=Attrition)) + geom_bar(stat="identity",  color="grey40",position="dodge")   + facet_wrap(~Attrition) +
                            theme_minimal() + theme(axis.text.x = element_text(angle = 90),plot.title=element_text(size=14, hjust=0.5))+
                            scale_fill_manual(values=c("sandybrown", "tomato2")) + 
                            labs(y="count", x="Business Travel", title="Business Travel and attrition status")  
                            
                          plot_grid(businesstravel, ncol=1, nrow=1)
                          
                          
                          Overtime<- attrition  %>% select(OverTime,Attrition,MonthlyIncome) %>% group_by(Attrition,OverTime) %>% summarise(count=n()) %>%
                            ggplot(aes(x=OverTime, y=count,fill=Attrition)) + geom_bar(stat="identity",  color="grey40",position="dodge")   + 
                            theme_minimal() + theme(axis.text.x = element_text(angle = 90),plot.title=element_text(size=14, hjust=0.5))+
                            scale_fill_manual(values=c("sandybrown", "tomato2")) + 
                            labs(y="count", x="Overtime", title="overtime and attrition status")  
                                                                      
                          plot_grid(Overtime, ncol=1, nrow=1)
                          
                          
                          attrition$breaks4 = cut(attrition$Age, 
                                                 breaks=c(seq(18,100,10)),
                                                 include.lowest=TRUE)
                          
                          library(ggplot2)
                          ggplot(attrition, aes(breaks4,fill=Attrition)) +
                            geom_bar() + 
                            
                            scale_fill_manual(values=c("cyan1", "tomato2")) + 
                            labs(y="count", x="Age Intrevals ", title=" Age  and Attrition Status") + 
                            theme_minimal()
                          
                          
                          attrition$breaks = cut(attrition$MonthlyIncome, 
                                             breaks=c(seq(0,20000,2000)),
                                             include.lowest=TRUE)
                          library(ggplot2)
                         ggplot(attrition, aes(breaks,fill=Attrition)) +
                            geom_bar() + 
                           coord_flip()+
                           scale_fill_manual(values=c("cyan1", "tomato2")) + 
                           labs(y="count", x="Income intrevals ", title=" Monthly income  and Attrition Status") + 
                           theme_minimal()
                          
                         options(repr.plot.width=60, repr.plot.height=30) 
                         stock<- attrition  %>% select(StockOptionLevel,Attrition,MonthlyIncome) %>% group_by(Attrition,StockOptionLevel) %>% summarise(count=n()) %>%
                           ggplot(aes(x=StockOptionLevel, y=count,fill=Attrition)) + geom_bar(stat="identity",  color="grey40",position="dodge")   + facet_wrap(~Attrition) +
                           theme_minimal() + theme(axis.text.x = element_text(angle = 90),plot.title=element_text(size=14, hjust=0.5))+
                           scale_fill_manual(values=c("cyan1", "tomato2")) + 
                           labs(y="count", x="Stock Option level", title=" Attrition Status by Stock Option level")  
                           
                         plot_grid(stock, ncol=1, nrow=1)
                         
                         
                         
                         
                         
                         
                         
                         dim(attrition)
                          library(plyr)
                          count(attrition, c("MonthlyIncome", "Attrition", "JobRole")) 
                          
sales<-attrition[which(attrition$JobRole =="Sales Executive" 
                       & attrition$MonthlyIncome<2000)]
                    library(dplyr)      
library(ggplot2)



options(repr.plot.width=60, repr.plot.height=30) 

library(plotly)
library(dplyr)



attrition$breaks2 = cut(attrition$YearsWithCurrManager
,                      breaks=c(seq(0,30,3)),
                       include.lowest=TRUE)
library(ggplot2)
ggplot(attrition, aes(breaks2,fill=Attrition)) +
  geom_bar(position="dodge") + 
  
scale_fill_manual(values=c("cyan1", "tomato2")) + 
  theme_minimal()+
  labs(y="count", x="Years Intervals with current Manager ", title="Years intervals with current manager  and Attrition Status")
mean1<-cat("Mean",mean(attrition$MonthlyIncome))
mean(attrition$DistanceFromHome)

options(repr.plot.width=60, repr.plot.height=30) 

box.MonthlyIncome <-attrition %>% select(MonthlyIncome) %>% 
  ggplot(aes( y=MonthlyIncome)) + geom_boxplot(color="black") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
labs(y="MonthlyIncome", x="Mean 6502.931")
mean(attrition$MonthlyRate)

box.Age <-attrition %>% select(Age) %>% 
  ggplot(aes( y=Age)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="Age", x="Mean 36.92381")

box.fromhome <-attrition %>% select(DistanceFromHome) %>% 
  ggplot(aes( y=DistanceFromHome)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="Distance from home", x="Mean 9.192517")

box.currmanager <-attrition %>% select(YearsWithCurrManager) %>% 
  ggplot(aes( y=YearsWithCurrManager)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="Years with curr manager", x="Mean 4.123129")


box.MonthlyRate <-attrition %>% select(MonthlyRate) %>% 
  ggplot(aes( y=MonthlyRate)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="MonthlyRate", x="Mean 14313.1")

box.HourlyRate <-attrition %>% select(HourlyRate) %>% 
  ggplot(aes( y=HourlyRate)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="HourlyRate", x="Mean 65.89116")


box.companiesworked <-attrition %>% select(NumCompaniesWorked) %>% 
  ggplot(aes(y= NumCompaniesWorked)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="NumCompaniesWorked", x="Mean 2.693197")

box.dailyrate<-attrition %>% select(DailyRate) %>% 
  ggplot(aes(y= DailyRate)) + geom_boxplot(color="red") + theme_minimal() +  coord_flip() +
  scale_fill_manual(values=c("darkorange", "cadetblue")) +
  labs(y="daily rate", x="Mean 802.4857")

plot_grid(box.Age,box.attrition,box.companiesworked,box.currmanager,box.HourlyRate,box.MonthlyRate,box.dailyrate,box.MonthlyIncome,box.fromhome,ncol=3,nrow=3)

mean(attrition$DailyRate)
library(dplyr)

mean(attrition$NumCompaniesWorked)
#normalize the value

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

levels(att$Attrition)[1]<-0
levels(att$Attrition)[2]<-1


att<-att %>%  select(-c(EmployeeCount,Over18, EmployeeNumber,StandardHours))
att$DailyRate<-normalize(att$DailyRate)
att$MonthlyRate<-normalize(att$MonthlyRate)
att$MonthlyIncome<-normalize(att$MonthlyIncome)
att$DistanceFromHome<-normalize(att$DistanceFromHome)
att$Age<-normalize(att$Age)
for (i in c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")){
  att[,i]=as.factor(att[,i])
}

set.seed(1234)
t <- sample(1:nrow(att), size=0.2*nrow(att))
ntest <- att[t,]
ntrain <- att[-t,]
View(ntrain)


nattrition <- dummy.data.frame(ntrain, names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")
nattritiontest<-dummy.data.frame(ntest,names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")
View(nattrition)

#model21
model31 <- glm(Attrition ~.,
               family=binomial(link='logit'),
               data=nattrition) 


summary(model31)
#regression concept using logistic regression
library(dummies)
library(dplyr)
attrition1<-attrition %>%  select(-c(EmployeeCount,Over18, EmployeeNumber,StandardHours))

View(attrition1)
for (i in c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")){
  attrition1[,i]=as.factor(attrition1[,i])
}

levels(attrition1$Attrition)[1]<-0
levels(attrition1$Attrition)[2]<-1

set.seed(3433)
t <- sample(1:nrow(attrition1), size=0.2*nrow(attrition1))
test <- attrition1[t,]
train <- attrition1[-t,]
table(train$Attrition)

attrition34 <- dummy.data.frame(train, names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")
attrition1test34<-dummy.data.frame(test,names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")
View(attrition34)
model91 <- glm(Attrition ~.,
               family=binomial(link='logit'),
               data=attrition34) 


result91 <- predict(model91,newdata=attrition1test34,type='response')
result91 <- ifelse(result91 > 0.5,1,0)
result91 <- factor(result91)
result91
summary(model91)

library(caret)
confusionMatrix(table(as.factor(result91),as.factor(attrition1test34$Attrition)))
table_mat91 <- table(attrition1test34$Attrition, result91)
table_mat91

accuracy_test91 <- sum(diag(table_mat91)) / sum(table_mat91)
paste("Accuracy for test set=",accuracy_test91)
View(result91)

library(ROCR)
predictions <- predict(model91, newdata=attrition1test34, type="response")
ROCRpred <- prediction(predictions, attrition1test34$Attrition)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1),title="Before Feature Engineering")


auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#logistic regression with balanced dataset

library(ROSE)
set.seed(1234)
data_balanced_overlog <- ovun.sample(Attrition ~ ., data = train, method = "over",N = 1980)$data
table(data_balanced_overlog$Attrition)
prop.table(table(train$Attrition))

data_balanced_underlog <- ovun.sample(Attrition ~ ., data = train, method = "under",N = 372)$data
table(data_balanced_underlog$Attrition)


data_balanced_bothlog <- ovun.sample(Attrition ~ ., data = train, method = "both",p = 0.5)$data
table(data_balanced_bothlog$Attrition)

data.roselog <- ROSE(Attrition ~ ., data = train, seed = 1)$data
table(data.rose$Attrition)





library(dummies)
#first logistic regression
attrition1 <- dummy.data.frame(data_balanced_overlog, names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")
attrition1test<-dummy.data.frame(test,names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")

#second logistic regression
attrition2 <- dummy.data.frame(data_balanced_underlog, names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")

#Third logistic regression
attrition3 <- dummy.data.frame(data_balanced_bothlog, names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")

#Fourth Logistic Regression
attrition4 <- dummy.data.frame(data.roselog, names=c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance","MaritalStatus","EducationField","Department","BusinessTravel","Gender","JobRole"), sep="_")

#model21
model21 <- glm(Attrition ~.,
             family=binomial(link='logit'),
             data=attrition1) 

#model22
model22 <- glm(Attrition ~.,
               family=binomial(link='logit'),
               data=attrition2) 
#model23

model23 <- glm(Attrition ~.,
               family=binomial(link='logit'),
               data=attrition3) 
#model 24
model24 <- glm(Attrition ~.,
               family=binomial(link='logit'),
               data=attrition4) 
summary(model21)
summary(model22)
summary(model23)
summary(model24)

## Predicting Test Data 1
result1 <- predict(model21,newdata=attrition1test,type='response')
result1 <- ifelse(result1 > 0.5,1,0)
result1 <- factor(result1)
result1

## Predicting Test Data 2
result2 <- predict(model22,newdata=attrition1test,type='response')
result2 <- ifelse(result2 > 0.5,1,0)
result2 <- factor(result2)
result2

## Predicting Test Data 3
result3 <- predict(model23,newdata=attrition1test,type='response')
result3 <- ifelse(result3 > 0.5,1,0)
result3 <- factor(result3)
result3

## Predicting Test Data 1
result4 <- predict(model24,newdata=attrition1test,type='response')
result4 <- ifelse(result4 > 0.5,1,0)
result4 <- factor(result4)
result4


attrition1test$Attrition
## Confusion matrix and statistics 1
library(caret)
confusionMatrix(table(as.factor(result1),as.factor(attrition1test$Attrition)))
table_mat <- table(attrition1test$Attrition, result1)
table_mat

accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
paste("Accuracy for test set=",accuracy_test)
View(result1)


## Confusion matrix and statistics 2
library(caret)
confusionMatrix(table(as.factor(result2),as.factor(attrition1test$Attrition)))
table_mat1 <- table(attrition1test$Attrition, result2)
table_mat1

accuracy_test1 <- sum(diag(table_mat1)) / sum(table_mat1)
paste("Accuracy for test set=",accuracy_test1)
View(result2)


## Confusion matrix and statistics 3
library(caret)
confusionMatrix(table(as.factor(result3),as.factor(attrition1test$Attrition)))
table_mat3 <- table(attrition1test$Attrition, result3)
table_mat3

accuracy_test2 <- sum(diag(table_mat3)) / sum(table_mat3)
paste("Accuracy for test set=",accuracy_test2)
View(result3)


## Confusion matrix and statistics 4
library(caret)
confusionMatrix(table(as.factor(result4),as.factor(attrition1test$Attrition)))
table_mat4 <- table(attrition1test$Attrition, result4)
table_mat4

accuracy_test4 <- sum(diag(table_mat4)) / sum(table_mat4)
paste("Accuracy for test set=",accuracy_test4)
View(result4)


## ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(model21, newdata=attrition1test, type="response")
ROCRpred <- prediction(predictions, test$Attrition)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))


## ROC Curve and calculating the area under the curve(AUC)2
library(ROCR)
predictions <- predict(model22, newdata=attrition1test, type="response")
ROCRpred <- prediction(predictions, test$Attrition)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

## ROC Curve and calculating the area under the curve(AUC)3
library(ROCR)
predictions <- predict(model23, newdata=attrition1test, type="response")
ROCRpred <- prediction(predictions, test$Attrition)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

## ROC Curve and calculating the area under the curve(AUC)1
library(ROCR)
predictions <- predict(model24, newdata=attrition1test, type="response")
ROCRpred <- prediction(predictions, test$Attrition)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

#As a rule of thumb, a model with good predictive ability 
#should have an AUC closer to 1 (1 is ideal) 
auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

table(attrition$Attrition)

#Regression using Random Forest tree cassifier

arg$BusinessTravel = factor(arg$BusinessTravel,
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                                    labels = c(1, 2, 3))



library(dplyr)
arg<-arg%>%  select(-c(EmployeeCount,Over18, EmployeeNumber,StandardHours))

set.seed(3433)
rt <- sample(1:nrow(arg), size=0.2*nrow(arg))
rtest <- arg[rt,]
rtrain <- arg[-rt,]
install.packages("ROSE")
#THOUGH IT IS THE UNBIASED CLASSIFICATIO WE ARE GOING TO TRY OVERSAMPLING AND UNDERSAMPLING ,BALANCED
dim(rtrain)
#over sampling
table(rtrain$Attrition)
library(ROSE)
2*979
library(caret)


data_balanced_over <- ovun.sample(Attrition ~ ., data = rtrain, method = "over",N = 1980)$data
table(data_balanced_over$Attrition)
prop.table(table(rtrain$Attrition))

data_balanced_under <- ovun.sample(Attrition ~ ., data = rtrain, method = "under",N = 372)$data
table(data_balanced_under$Attrition)


data_balanced_both <- ovun.sample(Attrition ~ ., data = rtrain, method = "both",p = 0.5)$data
table(data_balanced_both$Attrition)

data.rose <- ROSE(Attrition ~ ., data = rtrain, seed = 1)$data
 table(data.rose$Attrition)
library(randomForest)

r<-randomForest(Attrition ~ ., data =rtrain ,keep.forest=F, importance = TRUE)
 
 model1 <- randomForest(Attrition ~ ., data =data_balanced_over , importance = TRUE)
model1
varImp(model1)
x
x<-data.frame(varImp(model1))
View(x)
out.importance<-round(importance(model1),2)
out.importance
library(h2o)
h2o.varimp_plot(model1)
model2 <- randomForest(Attrition ~ ., data = data_balanced_under, ntree = 500, mtry = 500, importance = TRUE)
model2

model3<-randomForest(Attrition~.,data=data_balanced_both,ntree=100,importance=TRUE,mtry=500)
model3

model4<-randomForest(Attrition~.,data=data.rose,ntree=100,importance=T,mtry=50)
model4

predictions<-predict(model1,newdata=rtest,type="response")
predictions1<-predict(model2,newdata=rtest,type="response")
predictions2<-predict(model3,newdata=rtest,type="response")
predictions3<-predict(model4,newdata=rtest,type="response")
result<-as.data.frame(predict(model1,newdata=rtest,type="response"))
table(predictions,rtest$Attrition)
library(caret)
confusionMatrix(predictions,rtest$Attrition)

library(pROC)
v=importance(model1)
V
varImpPlot(model1,type=1)

library(ROCR)
p<-table(varImp(model1))
p
rocv <- roc(as.numeric(rtest$Attrition), as.numeric(predictions))
rocv1 <- roc(as.numeric(rtest$Attrition), as.numeric(predictions1))
rocv2<- roc(as.numeric(rtest$Attrition), as.numeric(predictions2))
rocv3 <- roc(as.numeric(rtest$Attrition), as.numeric(predictions3))

plot(rocv, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, main = "ROC curves", col = "salmon")
plot(rocv1, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "darkolivegreen", add = T)
plot(rocv2, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "steelblue", add = T)
plot(rocv3, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "burlywood", add = T)

rocv1$auc
rocv$auc
rocv2$auc
rocv3$auc
table_mat31 <- table(rtest$Attrition, predictions)
table_mat31

accuracy_test31 <- sum(diag(table_mat31)) / sum(table_mat31)
paste("Accuracy for test set=",accuracy_test31)
View(result4)

table_mat32 <- table(rtest$Attrition, predictions1)
table_mat32

accuracy_test32 <- sum(diag(table_mat32)) / sum(table_mat32)
paste("Accuracy for test set=",accuracy_test32)

table_mat33 <- table(rtest$Attrition, predictions2)
table_mat33

accuracy_test33 <- sum(diag(table_mat33)) / sum(table_mat33)
paste("Accuracy for test set=",accuracy_test33)

table_mat34 <- table(rtest$Attrition, predictions3)
table_mat34

accuracy_test34 <- sum(diag(table_mat34)) / sum(table_mat34)
paste("Accuracy for test set=",accuracy_test34)


ROCRpred <- prediction(as.numeric(predictions), as.numeric(rtest$Attrition))
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
importance(model1, type=1)
varImpPlot(model)

ggplot(varImp(model1),type=1) + 
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()

importance(model1)

varImp(model1)
x
install.packages("tibble")

library(tibble)

library(caret)
importance    <- model1$importance
row.names(importance)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
model1$importance
#Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
library(ggthemes)
#Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few() 


#random forest With XGBoost 
library(DMwR)
library(xgboost)



set.seed(3433)
model5 <- train(
  Attrition ~., data = rtrain, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model5$bestTune
summary(model5)
#Predicting with test data
predicted.classes <- model5 %>% predict(rtest)
head(predicted.classes)
library(pROC)
#accuracy test

mean(predicted.classes == rtest$Attrition)
rocv11 <- roc(as.numeric(rtest$Attrition), as.numeric(predicted.classes))
plot(rocv11, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, main = "ROC curves", col = "salmon")

rocv11$auc
#xg boost for under sampling 

set.seed(3433)
model6 <- train(
  Attrition ~., data = data_balanced_under, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model6$bestTune

#Predicting with test data
predicted.classes1 <- model6 %>% predict(rtest)
head(predicted.classes1)
library(pROC)
#accuracy test

mean(predicted.classes1 == rtest$Attrition)
rocv12 <- roc(as.numeric(rtest$Attrition), as.numeric(predicted.classes1))
plot(rocv12, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, main = "ROC curves", col = "salmon")


#xgb for balnced rose
set.seed(1234)
model8 <- train(
  Attrition ~., data = data.rose, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model8$bestTune

#Predicting with test data
predicted.classes3<- model8 %>% predict(rtest)
head(predicted.classes2)
library(pROC)
#accuracy test

mean(predicted.classes3 == rtest$Attrition)
rocv13 <- roc(as.numeric(rtest$Attrition), as.numeric(predicted.classes3))


#xgb for balanced both


set.seed(3433)
model9 <- train(
  Attrition ~., data = data_balanced_both, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model9$bestTune

#Predicting with test data
predicted.classes4<- model9 %>% predict(rtest)
head(predicted.classes4)
library(pROC)
#accuracy test

mean(predicted.classes4 == rtest$Attrition)
rocv14 <- roc(as.numeric(rtest$Attrition), as.numeric(predicted.classes4))



plot(rocv11, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, main = "ROC curves", col = "salmon")
plot(rocv12, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "darkolivegreen", add = T)
plot(rocv13, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "steelblue", add = T)
plot(rocv14, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "burlywood", add = T)

rocv11$auc
rocv12$auc
rocv13$auc
rocv14$auc

library(dplyr)



library(h2o)
#Importance plot
varImp(model)
importance    <- importance
x<-row.names(importance)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
importance
model1$importance
#Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few() 

importance(model1)

ggplot(varImp(model5)) + 
  geom_bar(stat = 'identity', fill = 'Darkorange', color = 'black') + 
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()+
  labs(title="Feature Importance for our XGBoost", x="Features", y="Importance")

gr <- xgb.plot.tree(model=bst, trees=0:1, render=FALSE)
