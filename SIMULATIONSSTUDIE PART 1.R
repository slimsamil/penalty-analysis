library(psych)
library(pracma)
options(scipen=999)
set.seed(22)
n <- 1000000
df <- data.frame()

#https://www.foottheball.com/football-top-10/best-left-footed-current-footballers-in-the-world/#:~:text=Only%2019%25%20of%20professional%20footballers,elusive%20than%20the%20right%2Dfooted.
starker_fuß <- sample(x = c('rechts', 'links'), size = n, replace = TRUE, prob = c(0.81, 0.19)) 
tabelle <- table(starker_fuß)#Absolute Haeufigkeiten
tabelle <- prop.table(tabelle)#In relative Haeufgkeit umrechnen
barplot(tabelle,xlab = "Starker Fuß", ylab = "Relative Häufigkeit",main= "Diskrete Verteilung Starker Fuß", col=c("azure4"))
df <- data.frame(starker_fuß)


# https://www.diefalsche9.de/zuschauer-und-auswaertsfahrertabelle-der-bundesliga-2022-23/#:~:text=Zuschauerschnitt,Stadionauslastung%20haben%20M%C3%BCnchen%20und%20Bremen.
minvisitors <- 17005
maxvisitors <- 81365
avgvisitors <- 42551
bvisitors <- maxvisitors - minvisitors
bvisitors
exvisitors <- ((avgvisitors - minvisitors) / bvisitors)
q <- 3.8#wählbar
p <-(0.397/0.603)*q
visitors <- round(minvisitors + bvisitors * rbeta(n, p, q))
hist(visitors,freq = FALSE, main = "Histogramm Zuschauerzahlen", ylab = "Relative Häufigkeit", xlab = "Anzahl Zuschauer", col=c("azure4"))
df <- cbind(df, visitors)


#Alter Schütze
minage <- 16
maxage <- 38
avgage <- 28
bage <- maxage - minage
exage <- ((avgage - minage) / bage)
q <- 1.25#wählbar
p <- (0.55/0.45) * q
age <- round(minage + bage * rbeta(n, p, q))
hist(age, freq= FALSE, main = "Histogramm Alter des Schützen", ylab = "Relative Häufigkeit", xlab = "Alter in Jahren", xaxt = 'n', col=c("azure4"))
axis(side=1, at=seq(16,38,2), labels=seq(16,38,2))
df <- cbind(df, age)


#Anlaufdistanz
mean <- 4
sd <- 0.8
Anlaufdistanz <- rnorm(n,mean,sd)
hist(Anlaufdistanz,freq=FALSE, xlab="Anlaufdistanz in Metern",ylab ="Relative Häufigkeit", main="Normalverteilung Anlaufdistanz", xaxt = 'n', col=c("azure4"))
axis(side=1, at=seq(0,8,1), labels=seq(0,8,1))
df <- cbind(df, Anlaufdistanz)


#Spielphase
spielphase <- sample(x = c('Erste Halbzeit', 'Zweite Halbzeit', 'Verlängerung', 'Elfmeterschießen'), size = n, replace = TRUE, prob = c(0.3, 0.45, 0.1, 0.15)) 
spielphase_factor <- factor(spielphase, levels = c("Erste Halbzeit", "Zweite Halbzeit", "Verlängerung", "Elfmeterschießen"))
tabelle <- table(spielphase_factor)
barplot(tabelle,xlab = "Spielphase", ylab = "Häufigkeit",main= "Diskrete Verteilung Spielphase Zeitpunkt Elfmeter", col=c("azure4"))
df <- cbind(df, spielphase)


#https://de.statista.com/statistik/daten/studie/1622/umfrage/bundesliga-entwicklung-der-durchschnittlich-erzielten-tore-pro-spiel/
#https://www.bundesligatrend.de/bundesliga-statistik-haeufigstes-ergebnis-tore-nach-minuten-anzahl-der-tore.html
differenz <- rep(n, 0)

for (x in 1:n) {
  if(df$spielphase[x] == 'Erste Halbzeit'){
    heim <- rpois(1, 0.7875)
    gast <- rpois(1, 0.5775)
    differenz[x] <- heim - gast
  }else if(df$spielphase[x] == 'Zweite Halbzeit'){
    heim <- rpois(1, 1.8)
    gast <- rpois(1, 1.32)
    differenz[x] <- heim - gast
  }else if(df$spielphase[x] == 'Verlängerung'){
    heim <- rpois(1, 1.7)
    gast <- rpois(1, 1.7)
    differenz[x] <- heim - gast
  }else if(df$spielphase[x] == 'Elfmeterschießen'){
    heim <- rpois(1, 1.8)
    gast <- rpois(1, 1.7)
    differenz[x] <- heim - gast
  }
}

tabelle <- table(differenz)#Absolute Haeufigkeiten
tabelle <- prop.table(tabelle)#In relative Haeufgkeit umrechnen
barplot(tabelle,xlab = "Differenz", ylab = "Relative Häufigkeit",main= "Poissonverteilung Differenz zum Zeitpunkt des Elfmeters", col=c("azure4"))

df <- cbind(df, differenz)

df$firsthalf <- ifelse(df$spielphase == 'Erste Halbzeit', 1, 0)
df$secondhalf <- ifelse(df$spielphase == 'Zweite Halbzeit', 1, 0)
df$extratime <- ifelse(df$spielphase == 'Verlängerung', 1, 0)
df$penalties <- ifelse(df$spielphase == 'Elfmeterschießen', 1, 0)

df$rechts <- ifelse(df$starker_fuß == 'rechts', 1, 0)
df$links <- ifelse(df$starker_fuß == 'links', 1, 0)

'scaler <- function(x){(x-min(x))/(max(x)-min(x))}
df$visitors <- scaler(df$visitors)
df$age <- scaler(df$age)
df$differenz <- scaler(df$differenz)'



b0 <- 10
brechts <- 1
blinks <- 1

bdifferenz <- 0.1
bfirsthalf <- 0.0
bsecondhalf <- 0.1
bextratime <- 0.3
bpenalties <- 0.4
bzuschauer <- 0.0001
balter <- -0.5
banlaufdistanz <- -0.2

u1 <- runif(n,0,1)
y1 <- rep(0, n)


pcorner <- (b0 
            + brechts * df$rechts
            + blinks * df$links
            + bdifferenz * df$differenz
            + bfirsthalf * df$firsthalf
            + bsecondhalf * df$secondhalf
            + bextratime * df$extratime
            + bpenalties * df$penalties
            + bzuschauer * df$visitors
            + balter * df$age)
hist(pcorner)


pcorner <- sigmoid(b0 
                   + brechts * df$rechts
                   + blinks * df$links
                   + bdifferenz * df$differenz
                   + bfirsthalf * df$firsthalf
                   + bsecondhalf * df$secondhalf
                   + bextratime * df$extratime
                   + bpenalties * df$penalties
                   + bzuschauer * df$visitors
                   + balter * df$age)
hist(pcorner, freq = FALSE, xlab = "Wahrscheinlichkeit", ylab = "Häufigkeit")
df <- cbind(df, pcorner)

for (x in 1:n){
  if(df$starker_fuß[x] == 'links'){
    df$pcorner[x] = 1 - df$pcorner[x]
  }
}
hist(df$pcorner, freq = FALSE, xlab = "Wahrscheinlichkeit", ylab = "Häufigkeit")

barplot(c(sum(df$pcorner)/n,(n-sum(df$pcorner))/n),
        names.arg = c("Rechts = 1","Links = 0"),ylab="Anteil", main = "Verteilung Eck")

hist(df$pcorner)
y1[u1<=pcorner] <- 1
corner <- y1
varEpsilon <- var(y1 - pcorner)
barplot(varEpsilon, main = "Rauschen")
df <- cbind(df, corner)
hist(df$corner, main = "Eck")
table(df$corner)
barplot(table(df$corner), ylab = "Häufigkeit", xlab = "0 = linkes Eck, 1 = rechtes Eck", main = "Verteilung der Endvariable")


df = subset(df, select = -c(firsthalf,secondhalf,extratime,penalties,rechts,
                                links))
write.csv(df, file = "/Users/samil/Documents/df_data_scientist.csv", row.names = FALSE)
