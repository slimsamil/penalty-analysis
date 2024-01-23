library(psych)
library(HH)
library(pROC)
library(caret)
set.seed(22)
data <- read.table("/Users/samil/Documents/df_data_scientist.csv", header=TRUE, sep=",", dec=".")


set.seed(22)

df <- data[sample(nrow(data), 10000), ]
describe(df)
summary(df)

hist(df$corner)
train$corner <- train$corner == 1
test$corner <- test$corner == 1

#SPLIT
split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[split, ]
test   <- df[!split, ]

#DUMMY
train$starker_fuß <- ifelse(train$starker_fuß == 'rechts', 1, 0)
train$second <- ifelse(train$spielphase == 'Zweite Halbzeit', 1, 0)
train$extra <- ifelse(train$spielphase == 'Verlängerung', 1, 0)
train$penalties <- ifelse(train$spielphase == 'Elfmeterschießen', 1, 0)
test$starker_fuß <- ifelse(test$starker_fuß == 'rechts', 1, 0)
test$second <- ifelse(test$spielphase == 'Zweite Halbzeit', 1, 0)
test$extra <- ifelse(test$spielphase == 'Verlängerung', 1, 0)
test$penalties <- ifelse(test$spielphase == 'Elfmeterschießen', 1, 0)

trainX <- cbind(train$age,train$visitors,train$differenz,
                train$second,train$extra, train$penalties, train$starker_fuß)
cov(trainX)#Kovarianzmatrix vor Standardisierung
trainX <- scale(trainX)#Standarisieren => Durchschnitt = 0; Var = 1
cov(trainX)#Kovarianzmatrix nach Standardisierung
trainY <- train$corner == 1
validX <- cbind(test$age,test$visitors,test$differenz,
                test$second,test$extra, test$penalties, test$starker_fuß) 
validX <- scale(validX)#Standarisieren => Durchschnitt = 0; Var = 1
validY <- test$corner == 1
#KNN mit unterschiedlichen k
parameterKNN <- c(400,300,200,100,10*(9:1),(9:1))
mseTrainingKNN <- rep(0,length(parameterKNN))
mseValidierungKNN <- rep(0,length(parameterKNN))
i <- 0
for(k in parameterKNN)
{
  print(paste("Bearbeite Parameter k = ",k))
  i <- i + 1
  
  schaetzwert <- knn(train = trainX,test = trainX,cl = trainY,prob=TRUE, k=k)
  schaetzwertProb <- (schaetzwert==TRUE)*attr(schaetzwert,"prob") + (schaetzwert==FALSE)*(1 - attr(schaetzwert,"prob"))#geschaetzte Wkeitsverteilung  
  mseTrainingKNN[i] <- mean((trainY - schaetzwertProb)^2)  
  
  schaetzwert <- knn(train = trainX,test = validX,cl = trainY,prob=TRUE, k=k)
  schaetzwertProb <- (schaetzwert==TRUE)*attr(schaetzwert,"prob") + (schaetzwert==FALSE)*(1 - attr(schaetzwert,"prob"))#geschaetzte Wkeitsverteilung  
  mseValidierungKNN[i] <- mean((validY - schaetzwertProb)^2)  
}
minMSEValidierung <- var(test$corner - test$pcorner)
minMSEValidierung
#optimales k ermitteln und zu MSE-Werte hinzufuegen
kOptPosition <- (1:length(parameterKNN))[mseValidierungKNN == min(mseValidierungKNN)]
kOpt <- parameterKNN[kOptPosition]

mseTraining <- cbind(mseTrainingKNN[kOptPosition],mseTrainingKNN[length(parameterKNN)])
mseValidierung <- cbind(mseValidierungKNN[kOptPosition],mseValidierungKNN[length(parameterKNN)])
###graphische Darstellung KNN
dotchart2(mseTrainingKNN,labels = parameterKNN, horizontal = FALSE,col="red",xlab="MSE",ylab = "k",xlim=c(0,0.2),ylim=c(0,0.2),main = "Optimierung KNN")
legend("topleft",legend = c("Training","Validierung"),col=c("red", "black"),lty = 3, bty="n")
dotchart2(mseValidierungKNN,horizontal = FALSE,add = TRUE,col="black")

min(mseValidierungKNN)

