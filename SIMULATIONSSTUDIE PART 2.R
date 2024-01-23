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
par(mfcol=c(1,2))# Einstellung: 2 Graphiken in einer Abbildung

hist(df$visitors, col=c("dodgerblue"), main = "Besucherzahl", ylab = "Häufigkeit", xlab = "Anzahl Besucher")
hist(df$age, col=c("dodgerblue"), main = "Alter", ylab = "Häufigkeit", xlab = "Alter in Jahren")
hist(df$Anlaufdistanz, col=c("dodgerblue"), main = "Anlaufdistanz", ylab = "Häufigkeit", xlab = "Anlaufdistanz in Metern")
hist(df$differenz, col=c("dodgerblue"), main = "Differenz", ylab = "Häufigkeit", xlab = "Differenz in Toren")
barplot(table(df$starker_fuß), col=c("dodgerblue"), main = "Starker Fuß", ylab = "Häufigkeit", xlab = "Starker Fuß")
barplot(table(df$spielphase), col=c("dodgerblue"), main = "Spielphase", ylab = "Häufigkeit", xlab = "Spielphase")

#SPLIT
split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
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
par(mfcol=c(1,1))# Einstellung: 2 Graphiken in einer Abbildung

#MODEL 1 ALLE VARIABLEN
lr <- glm(corner ~ starker_fuß + visitors + age + Anlaufdistanz + differenz + second + extra + penalties, data = train)
summary(lr)

vif(lr)

mse1 <- mean((lr$residuals)^2)

#MODEL 2 keine zweite Halbzeit
lr <- glm(corner ~ starker_fuß + visitors + age + Anlaufdistanz + differenz + extra + penalties, data = train)
summary(lr)

vif(lr)

mse2 <- mean((lr$residuals)^2)

#MODEL 3 keine staker fus
lr <- glm(corner ~ visitors + age + Anlaufdistanz + differenz + extra + penalties, data = train)
summary(lr)

vif(lr)

mse3 <- mean((lr$residuals)^2)

#MODEL 4 keine anlaufdistanz
lr <- glm(corner ~ visitors + age + differenz + extra + penalties, data = train)
summary(lr)

vif(lr)

mse4 <- mean((lr$residuals)^2)

#MODEL 4 keine verlängerung
lr <- glm(corner ~ visitors + age + differenz + penalties, data = train)
summary(lr)

vif(lr)

mse5 <- mean((lr$residuals)^2)

schaetzwert <- predict(lr,newdata = test,type="response")
mean((train$corner - schaetzwert)^2)
roc(test$corner, schaetzwert, plot = TRUE, legacy.axes = TRUE)