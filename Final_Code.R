#Project Motivation/Background

#R codes with proper documentations


# Merge the data
install.packages('tidyverse', repos = "http://cran.us.r-project.org")
library(tidyverse)
library(nycflights13)
dfstore <- read_csv('stores.csv')
dftrain <- read_csv('train.csv')
#dftest <- read_csv('test.csv')
dffeatures <- read_csv('features.csv')

dftrain <- merge(x=dftrain, y=dfstore, all.x = TRUE)
#dftest <- merge(x=dftest, y=dfstore, all.x = TRUE)

walmart_train <- merge(x=dftrain, y=dffeatures, all.x = TRUE)
#dftest <- merge(x=dftest, y=dffeatures, all.x = TRUE)
# clean data, create variables

walmart_train <- read_csv('Walmart_trainig_file.csv')
summary(walmart_train)
attach(walmart_train)

colSums(is.na(walmart_train))
walmart_train[is.na(walmart_train)] <- 0 #filling missing in markdown
#Categorized the weeklysales
walmart_train$sales_class[Weekly_Sales < 0] <- 'Negative'
walmart_train$sales_class[Weekly_Sales >=0 & Weekly_Sales < 10000] <- 'Low'
walmart_train$sales_class[Weekly_Sales >=10000 & Weekly_Sales < 25000] <- 'Medium'
walmart_train$sales_class[Weekly_Sales >=25000 & Weekly_Sales < 100000] <- 'High'
walmart_train$sales_class[Weekly_Sales >=100000 ] <- 'very High'
ggplot(walmart_train, aes(sales_class)) + 
  geom_bar(fill=2) + 
  geom_text(stat='count',aes(label = ..count..), vjust = -0.5 )

#walmart_train <- walmart_train%>%
#mutate(month = format.Date(as.Date(Date),"%m"))
walmart_train <- walmart_train%>%
  mutate(month = months(Date,abbreviate = T))
# Temerature x Weekly_Sales relationship 
hist(Temperature) 
ggplot(walmart_train, aes(Temperature,Weekly_Sales)) + 
  geom_point()
#ggplot(walmart_train, aes(Temperature, fill=sales_class))+geom_histogram(bins = 100) + facet_wrap(~sales_class)
ggplot(walmart_train,aes(Temperature, fill=sales_class)) + geom_density(alpha = 0.4) + 
  facet_wrap(~sales_class)

walmart_train$Tempclass[Temperature <  32] <- 'Freezing'
walmart_train$Tempclass[Temperature >= 32 & Temperature < 64] <- 'Cold'
walmart_train$Tempclass[Temperature >= 64 & Temperature < 79] <- 'Comfortable'
walmart_train$Tempclass[Temperature >= 79 & Temperature < 95] <- 'Hot'
walmart_train$Tempclass[Temperature >  95 ] <- 'Extremely Hot' 

ggplot(walmart_train, aes(Tempclass)) + 
  geom_bar(fill=2) + geom_text(stat='count',aes(label = ..count..), vjust = -1 )
attach(walmart_train)
Tempclass <- as.factor(Tempclass)
(Tempcalss_sales <- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('Tempclass')],mean))
ggplot(Tempcalss_sales, aes(Tempclass, Weekly_Sales, group=1))+
  geom_point() + geom_line() + geom_text(aes(label = Weekly_Sales), vjust = -0.5)
ggplot(walmart_train, aes(Tempclass, fill = sales_class)) + geom_bar()
#(the distribution conclusion is the same)


# Fuel_price x Weekly_Sales relationship 

hist(Fuel_Price)
ggplot(walmart_train, aes(Fuel_Price,Weekly_Sales)) + 
  geom_point()
#ggplot(walmart_train, aes(Fuel_Price, fill=sales_class))+geom_histogram(bins = 100) + facet_wrap(~sales_class)
ggplot(walmart_train,aes(Fuel_Price, fill=sales_class)) + geom_density(alpha = 0.4) + 
  facet_wrap(~sales_class)

walmart_train$Fuel_class[Fuel_Price < 3] <- 'Low'
walmart_train$Fuel_class[Fuel_Price >= 3 & Fuel_Price < 3.7 ] <- 'Middle'
walmart_train$Fuel_class[Fuel_Price >= 3.7 ] <- 'High'

attach(walmart_train)
Fuel_class <- as.factor(Fuel_class)
ggplot(walmart_train, aes(Fuel_class)) + 
  geom_bar(fill=2) + 
  geom_text(stat='count',aes(label = ..count..), vjust = -0.5 )
(Fuelclass_sales <- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('Fuel_class')],mean)) 
ggplot(Fuelclass_sales, aes(Fuel_class, Weekly_Sales, group=1))+
  geom_point() + geom_line() + geom_text(aes(label=Weekly_Sales), vjust =1)

ggplot(walmart_train, aes(Fuel_class, fill = sales_class)) + geom_bar() 
#(same conclusion)

# Relationship between CPI and Weekly_Sales
hist(CPI)
ggplot(walmart_train, aes(CPI,Weekly_Sales)) + 
  geom_point(aes(color = Type))+facet_wrap(~Type)




#The store with type A&C,the distribution is even, for type B, low CPI has much sales

ggplot(walmart_train, aes(CPI, fill=sales_class))+geom_histogram(bins = 100) + facet_wrap(~Weekly_Sales)
ggplot(walmart_train, aes(CPI, fill=sales_class))+geom_density(alpha=0.4)+facet_wrap(~Weekly_Sales)

walmart_train$CPI_class[CPI < 160] <- 'Low'
walmart_train$CPI_class[CPI >= 160 & CPI < 200] <- 'Medium'
walmart_train$CPI_class[CPI > 200] <- 'High'

attach(walmart_train)
CPI_class <- as.factor(CPI_class)
ggplot(walmart_train, aes(CPI_class)) + 
  geom_bar(fill=2) + 
  geom_text(stat='count',aes(label = ..count..), vjust = -0.5 )

(CPI_sales<- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('CPI_class')],mean))
ggplot(CPI_sales, aes(CPI_class, Weekly_Sales, group=1))+
  geom_point() + geom_line()+geom_text(aes(label = Weekly_Sales), vjust = 0.5)+
  scale_x_discrete(limits=c('Low','Medium','High'))
# The store with low CPI has much sales
# Low > medium > high

ggplot(walmart_train, aes(CPI_class, fill = sales_class)) + geom_bar()

# Relationship between Unemployment with weekly sales
hist(Unemployment)
ggplot(walmart_train, aes(Unemployment, Weekly_Sales)) + 
  geom_point()
#Under different rate of unemployment, the store with type A has much sales.

ggplot(walmart_train, aes(Unemployment, fill=sales_class))+geom_histogram(bins = 100) + facet_wrap(~sales_class)
ggplot(walmart_train,aes(Unemployment, fill=sales_class)) + geom_density(alpha = 0.4) + facet_wrap(~sales_class)
#The data distribution is quite same under different sales class, the most transactions happened in the range of 6~9

walmart_train$Unemployment_class[Unemployment < 7.33] <- 'Low'
walmart_train$Unemployment_class[Unemployment >= 7.33 & Unemployment < 10.66] <- 'Middle'
walmart_train$Unemployment_class[Unemployment >= 10.66] <- 'High'
attach(walmart_train)
Unemployment_class <- as.factor(Unemployment_class)
ggplot(walmart_train, aes(Unemployment_class)) + 
  geom_bar(fill=2) + 
  geom_text(stat='count',aes(label = ..count..), vjust = -0.5 )

(Unemploymentclass_sales <- aggregate(walmart_train[,'Weekly_Sales'],by=walmart_train[,c('Unemployment_class')],mean))
ggplot(Unemploymentclass_sales, aes(Unemployment_class, Weekly_Sales, group=1))+
  geom_point() + geom_line() + geom_text(aes(label=Weekly_Sales), hjust = -0.1)
# the store with middle unemployment rate(7.33-10.66) has much sales 
# Middle > Low > High

ggplot(walmart_train, aes(Unemployment_class, fill = sales_class)) + geom_bar()

# Month x Weekly_Sales relationship  

options(scipen = 999)
ggplot(walmart_train, aes(month,Weekly_Sales)) + geom_point() +scale_x_discrete(limits = month.abb)
ggplot(walmart_train, aes(month, Weekly_Sales,color=Type)) + geom_point()+  
  scale_x_discrete(limits = month.abb) + facet_wrap(~Type)

(monthly_sales<- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('month')],mean))
ggplot(monthly_sales, aes(month, Weekly_Sales, group=1))+
  geom_point() + geom_line()+
  ggtitle('Mean')+ 
  theme(plot.title = element_text(size = 30, face = "bold",vjust=-14, hjust = 0.05))+
  scale_x_discrete(limits = month.abb)
#mean can be changed to max


# Size x Weekly_Sales relationship  

hist(Size)
ggplot(walmart_train, aes(Size, fill = sales_class)) + geom_density(alpha = 0.4) + facet_wrap(~sales_class)
#ggplot(walmart_train, aes(Size, fill=sales_class))+geom_histogram(bins = 100) + facet_wrap(~sales_class)

# Categorized the size
walmart_train$Size_class[Size < 95000] <- 'Small'
walmart_train$Size_class[Size >= 95000 & Size < 160000] <- 'Medium'
walmart_train$Size_class[Size >= 160000] <- 'Huge'

ggplot(walmart_train, aes(Size_class)) + 
  geom_bar(fill=2) + 
  geom_text(stat='count',aes(label = ..count..), vjust = -0.5 )

(Size_sales<- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('Size_class')],mean))
ggplot(Size_sales, aes(Size_class, Weekly_Sales, group=1))+
  geom_point() + geom_line() + 
  geom_text(aes(label=Weekly_Sales), hjust = -0.1) +
  scale_x_discrete(limits=c('Small','Medium','Huge'))
```


# Type x Weekly_Sales relationship  
```{r}
ggplot(walmart_train, aes(Store, Weekly_Sales, color = Type)) + geom_point() + facet_wrap(~Type) #Size can be other variables( temperature, fuel_price...and so on)

ggplot(walmart_train, aes(month, Weekly_Sales,color=Type)) + geom_point()+  
  scale_x_discrete(limits = month.abb) + facet_wrap(~Type)

Type_sales_<-group_by(walmart_train,Type)
(Type_sales<-summarize(Type_sales_,count=n(),mean(Weekly_Sales),mean(Size)))
#(Type_sales<- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('Type')],mean))

# Store x Weekly_Sales relationship  
```{r}
ggplot(walmart_train)+geom_point(mapping = aes(x=Store,y=Weekly_Sales, color = Type))
Store_sales_<-group_by(walmart_train,Store)
(Store_sales<-summarize(Store_sales_,count=n(),mean(Weekly_Sales)))

(store_sales<- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('Store')],mean))
ggplot(store_sales, aes(Store, Weekly_Sales, group=1))+
  geom_point()+ geom_line() + ggtitle('Mean')+ 
  theme(plot.title = element_text(size = 30, face = "bold",vjust=-14, hjust = 0.98))+
  scale_x_continuous(breaks = seq(0,100, by=5))
# mean can be changed to max
```

# Dept x Weekly_Sales relationship  
```{r}
Dept_sales_<-group_by(walmart_train,Dept)
(Dept_sales<-summarize(Dept_sales_,count=n(),max(Weekly_Sales)))

(Dept_sales<- aggregate(walmart_train[,'Weekly_Sales'], by=walmart_train[,c('Dept')],max))

ggplot(Dept_sales, aes(Dept, Weekly_Sales, group=1))+
  geom_point()+ geom_line() + ggtitle('Max')+ 
  theme(plot.title = element_text(size = 30, face = "bold",vjust=-14, hjust = 0.05))+
  scale_x_continuous(breaks = seq(0,100, by=5))
# max can be changed to mean


detach(walmart_train)


write.csv(walmart_train, file = "walmart_cate1.csv",row.names=FALSE)

# Markdown x Weekly_Sales x Holiday relationship  
```{r}
walmart_train$MarkDown5=as.numeric(walmart_train$MarkDown5)
walmart_train$MarkDown4=as.numeric(walmart_train$MarkDown4)
walmart_train$MarkDown3=as.numeric(walmart_train$MarkDown3)
walmart_train$MarkDown2=as.numeric(walmart_train$MarkDown2)
walmart_train$MarkDown1=as.numeric(walmart_train$MarkDown1)

ggplot(walmart_train, aes(month, MarkDown1)) + geom_point() + 
  ggtitle('MarkDown1') +  
  theme(plot.title = element_text(size = 20, face = "bold",vjust=-14, hjust = 0.5))+
  scale_x_discrete(limits = month.abb)
# MarkDown1 can changed to be MarkDown2,MarkDown3....

#month_ <-group_by(walmart_train,month)
#(markdwon_discount<-summarize(month_,mean(MarkDown1)))
#(month_markdown<- aggregate(walmart_train[,'MarkDown5'], by=walmart_train[,c('month')],mean))
#ggplot(month_markdown, aes(month, MarkDown5, group=1))+
#geom_point()+ geom_line() + ggtitle('MarkDown5') +  
#theme(plot.title = element_text(size = 20, face = "bold",vjust=-14, hjust = 0.5))+
#scale_x_discrete(limits = month.abb)

```
# Holiday x Weekly_Sales relationship  
```{r}
ggplot(walmart_train, aes(Fuel_Price,Weekly_Sales)) + geom_smooth(aes(color=IsHoliday))
# Fuel_Pirce can be changed to any other variable like CPI,Store,Size...and so on.

ggplot(walmart_train, aes(Fuel_Price, Weekly_Sales)) + geom_smooth() 
```


Model
library(timeDate)
library(randomForest)

#set options to make sure scientific notation is disabled when writing files
options(scipen=500)

#reading all the files
df_Store <- read.csv(file='/Users/mac/Downloads/Walmart-sales-forecasting/stores.csv')
df_Train <- read.csv(file='/Users/mac/Downloads/Walmart-sales-forecasting/train.csv')
df_Test <- read.csv(file='/Users/mac/Downloads/Walmart-sales-forecasting/test.csv')
df_Features <- read.csv(file='/Users/mac/Downloads/Walmart-sales-forecasting/features.csv')
submission=read.csv(file='/Users/mac/Downloads/Walmart-sales-forecasting/sampleSubmission.csv',header=TRUE,as.is=TRUE)

# Merge Type and Size
df_TrainTmp <- merge(x=df_Train, y=df_Store, all.x=TRUE)
df_TestTmp <- merge(x=df_Test, y=df_Store, all.x=TRUE)

# Merge all the features
train <- merge(x=df_TrainTmp, y=df_Features, all.x=TRUE)
test <- merge(x=df_TestTmp, y=df_Features, all.x=TRUE)

# Make features for train
train$year = as.numeric(substr(train$Date,1,4))
train$month = as.numeric(substr(train$Date,6,7))
train$day = as.numeric(substr(train$Date,9,10))
# including the variables related to the date to include the seasonality and the pattern and give more weights to holiday days
train$days = (train$month-1)*30 + train$day
train$Type = as.character(train$Type)
train$Type[train$Type=="A"]=1
train$Type[train$Type=="B"]=2
train$Type[train$Type=="C"]=3
train$IsHoliday[train$IsHoliday=="TRUE"]=1
train$IsHoliday[train$IsHoliday=="FALSE"]=0
train$dayHoliday = train$IsHoliday*train$days
train$logsales = log(4990+train$Weekly_Sales)

#weight certain features more by duplication, not sure if helpful?
train$total_Days = 360*(train$year-2010) + (train$month-1)*30 + train$day
train$days30 = (train$month-1)*30 + train$day
smp_size <- floor(0.8 * nrow(train))
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
train=train[train_ind,]
test=train[-train_ind,]
#method to get the unique indexes store_dept_date to include it in final submission
submission$Id=NA
submission$Weekly_Sales=NA
submission = submission[FALSE,]
df = data.frame(id = numeric(0), jobs = numeric(0))
z=paste(test$Store,"_",test$Dept,"_",test$Date,sep = "")
headers<-c("Id","Weekly_Sales")
df <- as.data.frame(matrix(ncol=2,nrow=length(z)))
names(df)<-headers
submission=df
submission$Id=z

#converting Stores and Dept to factor variables
train$Store<-as.factor(train$Store)
train$Dept<-as.factor(train$Dept)
#To find important variables
######Ridge Regression and lasso
install.packages ("glmnet")
library(glmnet)
library(ISLR)
#Ridge Regression
x= model.matrix(train$logsales~.-X-Weekly_Sales-Date,train)
y=na.omit(train$logsales)
grid = 10^seq(10,-2,length=100)
ridge.mod= glmnet(x,y,alpha = 0, lambda = grid)
predict(ridge.mod, s=50,type= "coefficients")[1:24,]
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=0)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test ,])
mean((ridge.pred -y.test)^2)
out=glmnet(x,y,alpha = 0)
predict(out, type="coefficients", s=bestlam)[1:24,]
0.6089797
##Lasso
lasso.mod =glmnet (x[train ,],y[train], alpha = 1, lambda = grid)
set.seed(1)
cv.out = cv.glmnet(x[train ,],y[train],alpha=1)
bestlam= cv.out$lambda.min
bestlam
lasso.pred= predict(lasso.mod, s=bestlam, newx=x[test ,])
mean((lasso.pred -y.test)^2)
out=glmnet(x,y,alpha = 1,lambda = grid)
lasso.coef= predict(out, type="coefficients", s=bestlam)[1:24,]
lasso.coef
lasso.coef[lasso.coef!=0]
0.610171
# Important variables from LASSO
(Intercept)     	Store      	Dept      	Size       	CPI
9.110636e+00 -2.836443e-03  1.973538e-03  3.735732e-06 -5.839164e-04
month
3.726551e-03
# Since Lasso gives better results than Ridge we use coefficients from Lasso.
#Linear Model
#Running model with important variables from LASSO.
linear.model<-lm(train$logsales~Store + Dept + Size + CPI + month + year + day + days + dayHoliday + tDays + days30,data=train)
#For Size , day , days, tDays, days30 we notice that there's an NA coefficient in our model we can choose to exclude that variable and the model will still have the same coefficients as before.
linear.model<-lm(train$logsales~Store + Dept + CPI + month + year + dayHoliday ,data=train)
summary(linear.model)
# from the summary below we can see that all the Stores and Dept are significant and the dayHoliday is somewhat significant, while month year and CPI are significant.
# R-squared:  0.7708

#Random Forest
#Run model
tmpR0 = nrow(submission)
j=1
while (j < tmpR0) {
  print(j/tmpR0)#keep track of progress
  #select only relevant data for the store and department tuple
  tmpId = submission$Id[j]
  tmpStr = unlist(strsplit(tmpId,"_"))
  tmpStore = tmpStr[1]
  tmpDept = tmpStr[2]
  data_Dept = train[train$Dept==tmpDept,]
  tmpL = nrow(data_Dept[data_Dept$Store==tmpStore,])
  #since MAE is weighted 5x given in problem statement, increase weights of holiday data by 5x
  tmpF = data_Dept[data_Dept$IsHoliday==1,]
  #here we increased the weights by duplication and we can increase it by just multiplying also both will yield the same   result
  data_Dept = rbind(data_Dept,do.call("rbind", replicate(4, tmpF, simplify = FALSE)))
  dataF2 = data_Dept[data_Dept$Store==tmpStore,]  
  testF1 = test[test$Dept==tmpDept,]
  testF1 = testF1[testF1$Store==tmpStore,]
  testRows = nrow(testF1)
  p=paste(testF1$Store,"_",testF1$Dept,"_",testF1$Date,sep = "")
  submission_1=submission[submission$Id %in% p,]
  if(length(which(is.na(submission_1$Weekly_Sales)==TRUE))>0){
    print("going for rf")
    if (tmpL<10) {#sample size restrictions since rf can fail if there isn't enough data
      #this model uses all dept data (since that store + dept pair does not exist in the training set)
      tmpModel =  randomForest(logsales~Size+Type+ year + month + day + days + dayHoliday + total_Days + days30, 
                               ntree=4800, replace=TRUE, mtry=4, data=data_Dept)
    }else {
      #this model is trained on store+dept filtered data
      tmpModel =  randomForest(logsales ~ Size+year + month + day + days + dayHoliday + total_Days + days30, 
                               ntree=4800, replace=TRUE, mtry=3, data=dataF2)
    }
    tmpP = exp(predict(tmpModel,testF1))-4990
    #k = j + testRows - 1
    submission_1$Weekly_Sales = tmpP
    submission[submission$Id %in% p,"Weekly_Sales"]=submission_1$Weekly_Sales
  } 
  j = j+1
}



#Below is the result from Random Forest, we found MSE and MAPE to be following:
#Type of random forest: regression
#Number of trees: 4800
#No. of variables tried at each split: 6
#Mean of squared residuals: 0.01855983
#MAPE(Mean Absolute % error): 10%
       
#So, of all the methods, Random Forest is having the best performance, with 90% Accuracy.
     

     