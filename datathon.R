 results.20180908.150355 <- read.csv("~/Downloads/results-20180908-161700.csv")
   View(results.20180908.150355)
 df<- read.csv("~/Downloads/results-20180908-161700.csv")
 #caucasion-4
 #african american -2
 #blank-1
 #unknown -7
 #hispanic-5

#checking for nulls 
 colSums(is.na(df))
 
 #some nulls in the actual vent days
 #substituting for 0 in actual vent days
 df$ActualVentdays[is.na(df$ActualVentdays)] <- 0
 #checking for nulls again
 colSums(is.na(df))
 #all good
 
 summary(df)
 str(df)
 
 
 
 # $ patientunitstayid   : int  1602221 1619079 1679544 1695827 1636150 1613965 1690157 1629216 1679543 1619639 ...
 # $ gender              : Factor w/ 4 levels "Female","Male",..: 2 2 1 2 2 2 2 1 1 1 ...
 # $ age                 : Factor w/ 77 levels "> 89","14","15",..: 42 60 44 51 26 57 58 50 60 45 ...
 # $ Ethnicity           : Factor w/ 2 levels "Caucasian","Non-Caucasian": 2 1 1 1 2 1 1 1 1 1 ...
 # $ apachescore         : int  75 89 132 91 68 113 61 61 117 86 ...
 # $ ESRD                : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ dialysis            : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ ActualVentdays      : num  19 21 15 15 23 16 20 23 18 16 ...
 # $ teachingstatus      : Factor w/ 2 levels "false","true": 2 2 2 2 2 2 2 2 2 2 ...
 # $ hospitalid          : int  259 259 259 259 259 259 264 264 264 264 ...
 # $ numbedscategory     : Factor w/ 5 levels "","<100",">= 500",..: 5 5 5 5 5 5 3 3 3 3 ...
 # $ dischargetimedays   : num  18.9 20.7 14.4 13.9 21.9 ...
 # $ hospitalAdmitOffset : num  -15.0368 -2.8181 -0.0451 -0.0347 -0.0639 ...
 # $ hospitalFullDuration: num  34 23.5 14.4 13.9 22 ... 
 
 #need to get rid of redundant gender levels 
 
 df<-subset(df, gender!="Other")
 df<-subset(df, gender!="Unknown")
 df$gender<-droplevels((df$gender))
 
 # also some blanks in beds category
 
 df<-subset(df, numbedscategory!="")
 levels(df$numbedscategory)
 df$numbedscategory<-droplevels(df$numbedscategory)
 #now change variable types , age as integer, dialysis as factor, ESRD as factor
 
 df$age<-as.integer(df$age)
 df$ESRD<-as.factor(df$ESRD)
 df$dialysis<-as.factor(df$dialysis)
 df$gender1<-as.factor(as.numeric(df$gender))
 #male-2
 #female-1
 df$Ethnicity1<-as.factor(as.numeric(df$Ethnicity))
 #Non-C 2
 #Cauc -1
 df$teachingstatus1<-as.factor(as.numeric(df$teachingstatus))
 #true- 2
 #false-1
 df$numbedscategory1<-as.factor(as.numeric(df$numbedscategory))
 #250 - 499-4, >500-2, 100-249 -3,<100-1
 
 
 
 #need to drop unnecessary columns
 
 df1<-df[,-c(1:2,4,9:11,12,13)]
 # 'data.frame':	7728 obs. of  10 variables:
 #   $ age                 : int  42 60 44 51 26 57 58 50 60 45 ...
 # $ apachescore         : int  75 89 132 91 68 113 61 61 117 86 ...
 # $ ESRD                : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
 # $ dialysis            : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
 # $ ActualVentdays      : num  19 21 15 15 23 16 20 23 18 16 ...
 # $ hospitalFullDuration: num  34 23.5 14.4 13.9 22 ...
 # $ gender1             : Factor w/ 2 levels "1","2": 2 2 1 2 2 2 2 1 1 1 ...
 # $ Ethnicity1          : Factor w/ 2 levels "1","2": 2 1 1 1 2 1 1 1 1 1 ...
 # $ teachingstatus1     : Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2 2 2 ...
 # $ numbedscategory1    : Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 2 2 2 2 ...
 
 
 #create vent binary variable
 
 df1$vent[df1$ActualVentdays >0]<-1
 df1$vent[df1$ActualVentdays == 0]<-0
 df1$vent<-as.factor(df1$vent)
 
 # chi squared tests
 table1<-table(df1$age,df1$apachescore)
 table2<-table(df1$ActualVentdays,df1$apachescore)
 table3<-table(df1$hospitalFullDuration,df1$Ethnicity1)
 cor(df5)
# chisq.test(df1) 
 #chisq.test(df4) 
 
 chisq.test(table1, simulate.p.value = TRUE) 
 chisq.test(table2, simulate.p.value = TRUE)
 chisq.test(table3, simulate.p.value = TRUE)
 
 #If the p-value is greater  than the .05 significance level, 
 #we do not reject the null hypothesis that the x1 is 
 #independent of x2.
 
 
 #move on to logistic regression
model <- glm(ActualVentdays ~.,data=train1)
train <- df2[1:6188,]
 test <- df2[6188:7729,]
 

# Now, let’s fit the model. Be sure to specify the parameter family=binomial in the glm() function.
 #By using function summary() we obtain the results of our model:
 dfvent<-df1[-c(5:6)]
 train1<- dfvent[1:6188,]
 test1 <- dfvent[6188:7728,]
 
 model2 <- glm(vent ~.,family=binomial(link='logit'),data=train1)
 summary(model2)
 glm.probs <- predict(model2,type = "response")
 glm.pred <- ifelse(glm.probs > 0.5, "Vent", "NoVent")
 
 table(glm.pred,train1$vent)
# glm.pred    0    1
# NoVent  713  488 68% error
 #Vent   1273 3714 34% error
 
 
 mean(glm.pred == "Vent")
# 0.8059
 #accuracy
 # (713+3714)/(713+488+1273+3714)
# [1] 0.7154169

 
library(pscl) 
 
 pR2(model2)
 
# llh       llhNull            G2      McFadden          r2ML          r2CU 
 #-3287.6141433 -3883.4575729  1191.6868592     0.1534312     0.1751719     0.2450063  
# McFadden’s R2, which is defined as 1−[ln(LM)/ln(L0)] where ln(LM) 
# is the log likelihood value for the fitted model and ln(L0) is 
# the log likelihood for the null model with only an intercept as a predictor. 
 #The measure ranges from 0 to just under 1, 
# with values closer to zero indicating that the model has no predictive power. 
 
 varImp(model2)
 
 
  
prob= train1$glm.probs
 library(pROC)
 g <- roc(vent ~ prob, data = train1)
 plot(g)   
 