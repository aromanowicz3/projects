#Pakiety
#install.packages("mvtnorm")
#install.packages("coda")
library(MLmetrics)

rm(list = ls())
#Katalog roboczy
setwd("C:/Users/Aleksandra Romanowic/Desktop/SGH/Ekonometria bayesowska/projekt zaliczeniowy")

dane <- read.csv("crimes_and_safety_schools.csv", header = TRUE, sep = ",", dec = ".")

colnames(dane)

df = subset(dane, select = -c(REPFWT1,REPFWT2,REPFWT3,REPFWT4,REPFWT5,REPFWT6,REPFWT7,REPFWT8,REPFWT9,REPFWT10,REPFWT11,REPFWT12,REPFWT13,REPFWT14,REPFWT15,REPFWT16,REPFWT17,REPFWT18,REPFWT19,REPFWT20,REPFWT21,REPFWT22,REPFWT23,REPFWT24,REPFWT25,REPFWT26,REPFWT27,REPFWT28,REPFWT29,REPFWT30,REPFWT31,REPFWT32,REPFWT33,REPFWT34,REPFWT35,REPFWT36,REPFWT37,REPFWT38,REPFWT39,REPFWT40,REPFWT41,REPFWT42,REPFWT43,REPFWT44,REPFWT45,REPFWT46,REPFWT47,REPFWT48,REPFWT49,REPFWT50,IC0110,IC0112,IC0114,IC0116,IC0120,IC0121,IC0122,IC0124,IC0126,IC0128,IC0130,IC0134,IC0136,IC0138,IC0139,IC0140,IC0141,IC0142,IC0143,IC0144,IC0146,IC0148,IC0150,IC0151,IC0153,IC0155,IC0157,IC0158,IC0162,IC0166,IC0169,IC0170,IC0173,IC0163,IC0165,IC0167,IC0174,IC0175,IC0176,IC0177,IC0178,IC0179,IC0180,IC0181,IC0182,IC0183,IC0186,IC0600,IC0602,IC0604,IC0606,IC0608,IC0190,IC0192,IC0194,IC0196,IC0198,IC0200,IC0202,IC0204,IC0206,IC0208,IC0210,IC0212,IC0214,IC0216,IC0218,IC0610,IC0612,IC0614,IC0616,IC0618,IC0620,IC0622,IC0624,IC0626,IC0628,IC0630,IC0632,IC0634,IC0636,IC0638,IC0640,IC0642,IC0644,IC0646,IC0648,IC0650,IC0652,IC0654,IC0656,IC0658,IC0660,IC0662,IC0664,IC0666,IC0668,IC0670,IC0672,IC0674,IC0676,IC0678,IC0680,IC0682,IC0684,IC0686,IC0265,IC0266,IC0267,IC0268,IC0269,IC0270,IC0271,IC0272,IC0273,IC0274,IC0276,IC0277,IC0280,IC0282,IC0284,IC0286,IC0288,IC0290,IC0292,IC0294,IC0296,IC0298,IC0300,IC0302,IC0304,IC0306,IC0308,IC0688,IC0690,IC0374,IC0376,IC0378,IC0380,IC0381,IC0382,IC0383,IC0384,IC0386,IC0389,IC0391,IC0393,IC0390,IC0392,IC0394,IC0396,IC0398,IC0400,IC0402,IC0404,IC0406,IC0408,IC0410,IC0412,IC0414,IC0416,IC0418,IC0420,IC0422,IC0424,IC0426,IC0428,IC0430,IC0432,IC0434,IC0436,IC0438,IC0440,IC0442,IC0444,IC0446,IC0448,IC0450,IC0452,IC0454,IC0456,IC0518,IC0520,IC0526,IC0528,IC0532,IC0534,IC0536,IC0538,IC0560,IC0562,IC0568,IC0570,IC0572,IC0578,IC0580))

colnames(df)


names(df)[names(df) == "C0690_R"] <- "hate_crimes"

colnames(df)

unique(df["hate_crimes"])


#install.packages("sqldf")

library(sqldf)

sqldf('select hate_crimes, count(*) from df group by hate_crimes')

# 2 no, 1 yes

#leaving 89 variables
data1 = subset(df, select = c(hate_crimes,C0112,C0134,C0143,C0146,C0169,C0175,C0178,C0183,C0186,C0604,C0606,C0608,C0194,C0202,C0204,C0206,C0208,C0210,C0212,C0214,C0216,C0218,C0610,C0662,C0664,C0666,C0668,C0670,C0672,C0678,C0680,C0682,C0684,C0686,C0265,C0266,C0267,C0268,C0269,C0270,C0271,C0272,C0273,C0274,C0276,C0277,C0280,C0282,C0284,C0286,C0288,C0290,C0292,C0294,C0296,C0298,C0300,C0302,C0304,C0306,C0308,C0688_R,C0374,C0376,C0378,C0380,C0381,C0382,C0383,C0384,C0386,C0389,C0391,C0393,C0518,C0520,C0532,C0534,C0536,C0560,C0562,DISTOT16,INCID16,REMOVL16,STUOFF16,VIOINC16,DISRUPT,PERCWHT))

data1$hate_crimes[data1$hate_crimes==2] <- 0


sum(is.na(data1))
mean(is.na(data1))


#variable selection
#panelDat1 <- data.raw1[, -c(1:2)] #usuwamy kolumny ze zmiennymi identyfikuj?cymi okres i jednostki w panelu


modelCd1 <- bms(data1, g = "UIP", mprior = "fixed", mprior.size = 44, nmodel = 500)
image(modelCd1) #podsumowanie wynik?w
plotModelsize(modelCd1) 

modelCd1


data_fin = subset(data1, select = c(hate_crimes,INCID16,DISRUPT,
                                    PERCWHT,
                                    C0206
                                    ,
                                    C0183
                                    ,
                                    C0112
                                    ,
                                    C0391))

data_fin$C0183[data_fin$C0183==2] <- 0
data_fin$C0183[data_fin$C0112==2] <- 0
data_fin$C0183[data_fin$C0206==2] <- 0

library(mlr)

library(ROSE)

table(data_fin$hate_crimes)


table(data_fin$C0183)

#oversampling
data_balanced_over <- ovun.sample(hate_crimes ~ ., data = data_fin, method = "over",N = 4098)$data
table(data_balanced_over$hate_crimes)



ols.results <- glm(hate_crimes ~INCID16+DISRUPT+PERCWHT+C0206+C0183+C0112+C0391, data = data_balanced_over, family = 'binomial')
summary(ols.results)


ols.beta <- ols.results$coefficients
ols.sigma <- vcov(ols.results)
ols.sum.sq.residuals <- sum(ols.results$residuals ^ 2)

v.posterior <- nrow(dane) - length(ols.beta)
beta.posterior <- ols.beta
U.posterior <- ols.sigma / (ols.sum.sq.residuals / v.posterior)
s2.posterior <- ols.sum.sq.residuals / v.posterior




#Pr?bkowanie z funkcji q
library(mvtnorm)
S <- 10 ^ 5
set.seed(1)
#probkowanie z funkcji waznosci
losowanie.beta <- rmvt(S, sigma = s2.posterior * U.posterior,
                       df = v.posterior, delta = beta.posterior,
                       type = "shifted")
colnames(losowanie.beta) <- rownames(summary(ols.results)$coefficients)
head(losowanie.beta) #Ten sam wsp??czynnik wyst?puje z r??nymi znakami


beta0 <- losowanie.beta[, 1] # stała
beta1 <- losowanie.beta[, 2] # przestępstwa	zarejestrowane
beta2 <- losowanie.beta[, 3] # liczba	zakłóceń	porządku
beta3 <- losowanie.beta[, 4] # procent	białych
beta4 <- losowanie.beta[, 5] # zaangażowanie	społeczności	w	prace	społeczne
beta5 <- losowanie.beta[, 6] # trening	uczenia emocjonalnego
beta6 <- losowanie.beta[, 7] # kontrola	wejścia/monitoring	
beta7 <- losowanie.beta[, 8] # jak często szkolne środowisko jest narażone na cybernękanie

t(t(apply(losowanie.beta,2,mean)))

#Importance sampling
important.beta <- losowanie.beta[beta1 > 0 & beta2 > 0,  ]

length(important.beta)

#p-stwo a posteriori spełnienia wszystkich restrykcji
nrow(important.beta)/nrow(losowanie.beta)

grey_line = rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_line = rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)


#b1
important.beta1 <- important.beta[, 2]
windows(width = 1000, height = 800)
restricted.h <- hist(important.beta1, breaks = (- 30:30) / 100, 
                     col = "gray", 
                     xlab = "Zarejestrowana iczba incydentów", 
                     main = "Rozkład a posteriori przy uciętym rozkładzie a priori")
unrestricted.h <- hist(beta1, breaks = (-30:30) / 100, col = "gray", 
                       xlab = "Zarejestrowana iczba incydentów", 
                       main = "Rozkład a posteriori przy skrajnie nieinformacyjnym rozkładzie a priori")
plot(x = (- 29:30) / 100, y = restricted.h$density, col = green_line, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru beta1 (zarejestrowana iczba incydentów)",
     xlab = "wartość parametru beta1", ylab = "gęstość")
lines(x = (- 29:30) / 100, y = unrestricted.h$density, col = grey_line, lwd = 1)
legend(x = "left", fill = c(grey_line, green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta1), col = green_line, lwd = 3, lty = 3)
abline(v = mean(beta1), col = grey_line, lwd = 1, lty = 3)

#b2
important.beta2 <- important.beta[, 3]
windows(width = 1000, height = 800)
restricted.h <- hist(important.beta2, breaks = (- 30:30) / 100, 
                     col = "gray", 
                     xlab = "Liczba zakłóceń porządku", 
                     main = "Rozkład a posteriori przy uciętym rozkładzie a priori")
unrestricted.h <- hist(beta2, breaks = (-30:30) / 100, col = "gray", 
                       xlab = "Liczba zakłóceń porządku", 
                       main = "Rozkład a posteriori przy skrajnie nieinformacyjnym rozkładzie a priori")
plot(x = (- 29:30) / 100, y = restricted.h$density, col = green_line, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru beta2 (liczba zakłóceń porządku)",
     xlab = "wartość parametru beta2", ylab = "gęstość")
lines(x = (- 29:30) / 100, y = unrestricted.h$density, col = grey_line, lwd = 1)
legend(x = "left", fill = c(grey_line, green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta2), col = green_line, lwd = 3, lty = 3)
abline(v = mean(beta2), col = grey_line, lwd = 1, lty = 3)

#b5
important.beta5 <- important.beta[, 6]
windows(width = 1000, height = 800)
restricted.h <- hist(important.beta5, breaks = (- 30:30) / 100, 
                     col = "gray", 
                     xlab = "Szkolenia z uczenia emocjonalnego dla studentów", 
                     main = "Rozkład a posteriori przy uciętym rozkładzie a priori")
unrestricted.h <- hist(beta5, breaks = (-30:30) / 100, col = "gray", 
                       xlab = "Szkolenia z uczenia emocjonalnego dla studentów", 
                       main = "Rozkład a posteriori przy skrajnie nieinformacyjnym rozkładzie a priori")
plot(x = (- 29:30) / 100, y = restricted.h$density, col = green_line, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru beta5
     (szkolenia z uczenia emocjonalnego dla studentów)",
     xlab = "wartość parametru beta5", ylab = "gęstość")
lines(x = (- 29:30) / 100, y = unrestricted.h$density, col = grey_line, lwd = 1)
legend(x = "left", fill = c(grey_line, green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta5), col = green_line, lwd = 3, lty = 3)
abline(v = mean(beta5), col = grey_line, lwd = 1, lty = 3)



##-------------------------


dane_short <- data_balanced_over

# Długości próby testowej
O <- c(205, 410, 615, 820, 1025, 1230)

# Ile razy maksymalnie dzielimy naszą próbę (losowo)
K_max <- 100

S <- 10^4
final.results <- matrix(NA, length(O), 5)
rownames(final.results) <- O
colnames(final.results) <- c("GLM logloss", "GLM Calculation Errors", "Bayesian logloss", "Bayesian Calculation Errors", "Probability")
set.seed(1)
iteracja <- 0


for(i in 1:length(O)){
  o <- O[i]
  
  #Na ile sposob?w dzielimy pr?b?:
  K <- min(choose(nrow(dane_short), o), K_max)
  forecast.errors <- matrix(NA, K, 5)
  for(k in 1:K){
    
    #Liczymy iterację
    iteracja <- iteracja + 1
    print(paste("Iteracja ", iteracja, " spośród ", (length(O) - 1) * K_max + nrow(dane_short), ".", sep = ""))
    outofsample <- dane_short[sample(1:nrow(dane_short), o), ]
    insample <- dane_short[-as.numeric(rownames(outofsample)), ]
    y_empirical <- outofsample$hate_crimes
    
    #Ucinamy zmienna objasniana
    outofsample <- outofsample[, -1]
    outofsample <- cbind(1, as.matrix(outofsample))
    ols <- glm(hate_crimes ~INCID16+DISRUPT+PERCWHT+C0206+C0183+C0112+C0391, data = insample, family = 'binomial')

    ols.beta <- ols$coefficients
    ols.sigma <- vcov(ols)
    v <- nrow(insample) - ncol(insample)
    
    #Przybliżenie Monte Carlo - generujemy wektory oszacowan z rozkladu a posteriori
    #bez restrykcji
    losowanie.beta <- tryCatch(rmvt(S, sigma = ols.sigma, df = v, delta = ols.beta, type = "shifted"),
                               error = function(e) matrix(NA, S, ncol(insample)))
    
    #shifted daje odpowiedni rozklad dla symulacji z rozkladu a posteriori
    b0 <- losowanie.beta[, 1] # stała
    b1 <- losowanie.beta[, 2] # przestępstwa zarejestrowane
    b2 <- losowanie.beta[, 3] # liczba zakłóceń porządku
    b3 <- losowanie.beta[, 4] # procent białych
    b4 <- losowanie.beta[, 5] # zaangażowanie społeczności w prace społeczne
    b5 <- losowanie.beta[, 6] # trening uczenia
    b6 <- losowanie.beta[, 7] # kontrola wejścia/monitoring
    b7 <- losowanie.beta[, 8] # jak często szkolne środowisko jest narażone na cybernękanie
    important.beta <- losowanie.beta[b1 > 0 & b2 > 0 & b5>0, ]
   
    bayes.beta <- tryCatch(apply(important.beta, 2, mean), error = function(e) rep(NA, ncol(insample)))
    bayes.prob <- nrow(important.beta) / nrow(losowanie.beta) 
    
    #Liczymy prognozy
    #Doliczamy logarytmy dla pr?by testowej
    #outofsample[, 3:4] <- log(outofsample[, 3:4])
    ols.pred <- outofsample %*% ols.beta
    ols.predict <- round(exp(ols.pred)/(1+exp(ols.pred)))
    bayes.pred <- outofsample %*% bayes.beta
    bayes.predict <- round(exp(bayes.pred)/(1+exp(bayes.pred)))
 
    #liczymy błędy
    logloss.ols <- LogLoss(ols.predict, y_empirical)
    logloss.bayes <- LogLoss(bayes.predict, y_empirical)
    
    
    errors.ols <- sum(is.na(ols.predict))
    errors.bayes <- sum(is.na(bayes.predict))
    
    #średnie prawdopodobie?stwo a posteriori spe?nienia wszystkich restrykcji
    prob.bayes <- mean(bayes.prob, na.rm = TRUE)
    
    #Wypełniamy tabelkę ze średnimi błędami prognoz dla każdej wylosowanej próby testowej
    forecast.errors[k, ] <- c(logloss.ols, errors.ols, logloss.bayes, errors.bayes, prob.bayes)
  }
  #Uśredniamy wyniki tak, by otrzymać sumaryczne miary dla każdej wielkości próby testowej
  final.results[i, ] <- apply(forecast.errors, 2, mean, na.rm = TRUE)
}

logloss.ratio <- final.results[, 3] / final.results[, 1]
final.results <- cbind(final.results, logloss.ratio)
colnames(final.results)[6] <- "logloss ratio"

final.results