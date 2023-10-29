# Wczytujemy biblioteki 

library(knitr)
library(xtable) 
library(scorecard) 
library(InformationValue) 
library(tidyverse)
library(kableExtra)
library(forecast)
library(DataExplorer)
library(egg)
library(dplyr)
library(sjmisc)
library(corrplot)
library(broom)
library(ROCR)

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 1 #####
# wczytujemy zbiór danych danych germancredit
data("germancredit")
dane <- germancredit

# sprawdzamy struktore danych
str(dane) 
glimpse(dane)

# podsumowanie dla każdej zmiennej
lapply(dane, summary)

# poziomy danej zmiennej
levels(dane$personal.status.and.sex)

# niewykorzystywane poziomy usuwamy poleceniem droplevels
dane$personal.status.and.sex <- droplevels(dane$personal.status.and.sex)

# WSZYSTKIE KOLUMNY ZE ZBIORU (21 KOLUMN)
# Status of existing checking account
# Duration in month
# Credit history
# Purpose (car,furniture,education)
# Credit amount
# Savings account/bonds
# Present employment since
# Installment rate in percentage of disposable income
# Personal status and sex
# Other debtors / guarantors
# Present residence since
# Property(real estate,life insurance)
# Age in years
# Other installment plans(bank,stores,none)
# housing(rent,own,free)
# Number of existing credits at this bank
# Job
# Number of people being liable to provide maintenance for
# Telephone
# foreign worker 
# creditability

# zamieniamy nazwy kolumn na krótsze, żeby łatwiej się w analizy wpisywało
colnames(dane)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")

# Zmieniamy typy danych - duration, credit amount oraz age ze zmiennej typu numeric na zmienną typu factor. 
# dane <- mutate(dane, duration = as.factor(duration), amount = as.factor(amount), age = as.factor(age))
# str(dane) - sprawdzałam, czy działa

# sprawdzamy, czy mamy jakieś brakujące kolumny itd
metadata <- t(introduce(dane))
colnames(metadata)<-"Values"
xtable(metadata)

# rysujemy wykres tabelki powyżej
plot_intro(dane)

# duration
g1 <- ggplot(dane, aes(x = as.factor(response), y = duration, fill = as.factor(response))) + geom_boxplot() + theme(legend.position = "none")

# amount
g2 <- ggplot(dane, aes(x = as.factor(response), y = amount, fill = as.factor(response))) + geom_boxplot() +
  theme(legend.position = "none")

# age
g3 <- ggplot(dane, aes(x = as.factor(response), y = age, fill = as.factor(response))) + 
  geom_boxplot() + theme(legend.position = "none")

ggarrange(g1, g2,g3, labels = c("Duration", "Amount", "Age"),
          ncol = 3, nrow = 1)

# zmieniliśmy good na numer 1, bad na numer 0
dane <- dane %>% mutate(response = fct_recode(response, "1" = "good", "0" = "bad")) 

X <- dane %>% filter(response == "1") %>% count(age) 
xtable(summary(X))

Y <- dane %>% filter(response == "0") %>% count(age)
summary(Y)

# kategoryzacja zmiennej wiek - part1
dane <- mutate(dane, age = cut(age, breaks = c(0, 18, 30, 50, 80, Inf), labels = c("<= 18", "18-30", "30-50", "50-80", ">=80")))
dane$age

age <- dane %>% group_by(age) %>% count()
tabela_D <- data.frame(c(age))
colnames(tabela_D) <- c("KATEGORIA WIEKOWA", "$n$")
tab_D <- xtable(tabela_D, caption = "Kategoryzacja zmiennej wiek")
print(tab_D, type = "latex", table.placement = "H")

# kategoryzacja zmiennej wiek - part2
dane <- mutate(dane, age = cut(age, breaks = c(0, 25, 44, 75, Inf), labels = c("<= 25", "25-44", "44-75",  ">=75")))
dane$age

age <- dane %>% group_by(age) %>% count()
tabela_D <- data.frame(c(age))
colnames(tabela_D) <- c("KATEGORIA WIEKOWA", "$n$")
tab_D <- xtable(tabela_D, caption = "Kategoryzacja zmiennej wiek")
print(tab_D, type = "latex", table.placement = "H")

# wykres kołowy zmiennej purpose i tabelki
par(mfrow=c(1,1))
arg <- table(dane$purpose)
pie(arg, col = c("#FF2052", "#007FFF", "#FE6F5E", "#8A2BE2", "#DE5D83", "#A52A2A","#ACE1AF", "#800020","#00CC99", "#702963"))
arg <- table(dane$purpose)
barplot(arg, col = c("#F984E5", "#FADADD", "#DB7093"), main = "Purpose")
pur <- dane %>% group_by(purpose) %>% count()
xtable(pur)

X <- dane %>% filter(purpose == "radio/television") %>% count(response) 

Y <- dane %>% filter(purpose == "car (new)") %>% count(response) 

Z <- dane %>% filter(purpose == "furniture/equipment") %>% count(response) 

Q <- dane %>% filter(purpose == "education") %>% count(response) 

# wykres kołowy zmiennej personal status i sex i tabelki
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 
arg <- table(dane$sex)
pie(arg, border="white", col=myPalette )

arg1 <- table(dane$sex)
pie(arg1, col = c("#8A2BE2", "#DE5D83", "#00DDDD", "#D19FE8"))
sex <- dane %>% group_by(sex) %>% count()
xtable(sex)

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 2 #####

train <- split_df(dane)[[1]] # 70%
test <- split_df(dane)[[2]] # 30%

# sprawdzamy stosunek good do bad creditability w zbiorze treningowym i testowym
train_cred <- train %>% group_by(response) %>% count()

test_cred <- test %>% group_by(response) %>% count()

# dwa boxploty zmiennej age zbioru treningowego i testowego
par(mfrow=c(1,2))
boxplot(train$age, col = "#E52B50", xlab="Train - age")
boxplot(test$age, col = "#FFBF00", xlab="Test - age")

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 3 #####

# tworzymy model regresji logistycznej
model_glm <- glm(formula = response~., data = train, family = binomial('logit'))
summary_glm <- summary(model_glm)

# sprawdzamy wartości AIC
summary_glm$aic
model_glm$aic

model_glm_simple <- glm(formula = response~1, data = train, family = binomial('logit'))

# AIC_model_forward
AIC_model_forward <- step(
  object = model_glm_simple,
  scope=list(lower=model_glm_simple,upper=model_glm),
  data = train,
  direction = "forward",
  family = binomial('logit'),
  trace = FALSE)

# AIC_model_forward

# AIC_model_backward
AIC_model_backward <- step(object = model_glm, direction = "backward", trace = FALSE)
# AIC_model_backward

# AIC_model_both
AIC_model_both <- step(object = model_glm, direction = "both", trace = FALSE)
# AIC_model_both

# sprawdzamy wartości AIC
AIC_model_forward$aic
AIC_model_backward$aic
AIC_model_both$aic


# model BIC
BIC_model_forward <- step(
  object = model_glm_simple,
  scope=list(lower=model_glm_simple,upper=model_glm),
  data = train,
  direction = "forward",
  family = binomial('logit'),
  trace = FALSE,
  k=log(nrow(train)))

BIC_model_backward <- step(object = model_glm, direction = "backward", trace = FALSE,k=log(nrow(train)))

BIC_model_both <- step(object = model_glm, direction = "both", trace = FALSE,k=log(nrow(train)))

# sprawdzamy gdzie jest najniższa wartość AIC
BIC_model_forward$aic
BIC_model_backward$aic
BIC_model_both$aic

AIC.both <- step(model_glm, direction = "both")
AIC.forward <- step(model_glm, direction = "forward")
AIC.backward <- step(model_glm, direction = "backward")

BIC.both <- step(model_glm, k=log(nrow(train)), direction = "both")
BIC.forward <- step(model_glm, k=log(nrow(train)), direction = "forward")
BIC.backward <- step(model_glm, k=log(nrow(train)), direction = "backward")

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 4 #####

# rysyjemy krzywą ROC i obliczamy wartość pola AUC pod nim

par(mfrow=c(1,1))
pred.gc.train <- predict(AIC.backward, type="response")
# pred.gc.train

pred0 <- prediction(pred.gc.train, train$response)
perf0 <- performance(pred0, "tpr", "fpr")

plot(perf0, col="#9966CC", lwd = 2)
unlist(slot(performance(pred0, "auc"), "y.values"))

M1 <- AIC_model_both
M2 <- BIC_model_both

AIC_pred <- predict.glm(object=M1, newdata=test, type='response')
BIC_pred <- predict.glm(object=M2, newdata=test, type='response')

# Macierz pomyłek ogólnego modelu

AIC_conf_mat <- table(real = test$response, pred = as.numeric(AIC_pred>0.5))
xtable(AIC_conf_mat)
# AIC_conf_mat[1,1] # 51 TN
# AIC_conf_mat[1,2] # 47 FP
# AIC_conf_mat[2,1] # 40 FN
# AIC_conf_mat[2,2] # 181 TP

BIC_conf_mat <- table(real = test$response, pred = as.numeric(BIC_pred>0.5))
xtable(BIC_conf_mat)
# BIC_conf_mat[1,1] # 39 TN
# BIC_conf_mat[1,2] # 59 FP
# BIC_conf_mat[2,1] # 34 FN
# BIC_conf_mat[2,2] # 187 TP

# wskaźniki dla ogólnego modelu AIC
test$response <- ifelse(test$response=="good",1,0)
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
AIC_metric_acc <- 1-misClassError(actuals = test$creditability, predictedScores = AIC_pred)
M1_acc0 <- (AIC_conf_mat[2,2] + AIC_conf_mat[1,1])/(AIC_conf_mat[2,2] + AIC_conf_mat[1,1] + AIC_conf_mat[1,2] + AIC_conf_mat[2,1])
M1_acc0 # 0.7272727

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
AIC_metric_err <- misClassError(actuals = test$creditability, predictedScores = AIC_pred)
M1_err0 <- 1 - (AIC_conf_mat[1,1]+AIC_conf_mat[2,2])/sum(rowSums(AIC_conf_mat))
M1_err0 # 0.2727273

# sensitivity
# (TP)/(FN + TP)
AIC_metric_sens <- sensitivity(actuals = test$creditability, predictedScores = AIC_pred)
M1_sens0 <- (AIC_conf_mat[2,2])/((AIC_conf_mat[2,1] + AIC_conf_mat[2,2]))
M1_sens0 # 0.8190045

# specificity
# (TN)/(TN + FP)
AIC_metric_spec <- specificity(actuals = test$creditability, predictedScores = AIC_pred)
M1_spec0 <- (AIC_conf_mat[1,1])/((AIC_conf_mat[1,1]) + (AIC_conf_mat[1,2]))
M1_spec0 # 0.5204082

# precision (positive prediction)
# (TP)/(FP + TP)
AIC_metric_PP <- precision(actuals = test$creditability, predictedScores = AIC_pred)
M1_PP0 <- (AIC_conf_mat[2,2])/(AIC_conf_mat[1,2] + AIC_conf_mat[2,2])
M1_PP0 # 0.7938596

# negative prediction
# (TN)/(TN + FN)
AIC_metric_NP <- npv(actuals = test$creditability, predictedScores = AIC_pred)
M1_NP0 <- (AIC_conf_mat[1,1])/(AIC_conf_mat[1,1] + AIC_conf_mat[2,1])
M1_NP0 # 0.5604396

# wskaźniki dla ogólnego modelu BIC
test$response <- ifelse(test$response=="good",1,0)
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
BIC_metric_acc <- 1-misClassError(actuals = test$creditability, predictedScores = BIC_pred)
M1_acc0B <- (BIC_conf_mat[2,2] + BIC_conf_mat[1,1])/(BIC_conf_mat[2,2] + BIC_conf_mat[1,1] + BIC_conf_mat[1,2] + BIC_conf_mat[2,1])
M1_acc0B # 0.7084639

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
BIC_metric_err <- misClassError(actuals = test$creditability, predictedScores = BIC_pred)
M1_err0B <- 1 - (BIC_conf_mat[1,1]+BIC_conf_mat[2,2])/sum(rowSums(BIC_conf_mat))
M1_err0B # 0.2915361

# sensitivity
# (TP)/(FN + TP)
BIC_metric_sens <- sensitivity(actuals = test$creditability, predictedScores = BIC_pred)
M1_sens0B <- (BIC_conf_mat[2,2])/((BIC_conf_mat[2,1] + BIC_conf_mat[2,2]))
M1_sens0B # 0.8461538

# specificity
# (TN)/(TN + FP)
BIC_metric_spec <- specificity(actuals = test$creditability, predictedScores = BIC_pred)
M1_spec0B <- (BIC_conf_mat[1,1])/((BIC_conf_mat[1,1]) + (BIC_conf_mat[1,2]))
M1_spec0B # 0.3979592

# precision (positive prediction)
# (TP)/(FP + TP)
BIC_metric_PP <- precision(actuals = test$creditability, predictedScores = BIC_pred)
M1_PP0B <- (BIC_conf_mat[2,2])/(BIC_conf_mat[1,2] + BIC_conf_mat[2,2])
M1_PP0B # 0.7601626

# negative prediction
# (TN)/(TN + FN)
BIC_metric_NP <- npv(actuals = test$creditability, predictedScores = BIC_pred)
M1_NP0B <- (BIC_conf_mat[1,1])/(BIC_conf_mat[1,1] + BIC_conf_mat[2,1])
M1_NP0B # 0.5342466

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 5 #####
data("germancredit")
train <- split_df(germancredit)[[1]]

# kategoryzujemy predyktory typu ciaglego za pomocą woebin z pakietu scorecard
woe <- woebin(dt = as.data.frame(train),
              y="creditability",
              positive = "bad")

# wyznaczamy IV
inf.v <- rep(0,20)
for (i in 1:20){
  inf.v[i] <- woe[[i]][[1,10]]
  }

# wybieramy zmienne, dla których IV > 0.1
nazwy <- colnames(train[,-21])
nazwy_iv <- nazwy[which(inf.v>0.1)]
nazwy_iv

# ramka danych data_iv z wartościami IV dla wszystkich 20 zmiennych
data_iv <- as.data.frame(cbind(nazwy, round(inf.v, 4)))
colnames(data_iv) <- c("nazwy","InformationValue")
data_iv$InformationValue <- as.numeric(data_iv$InformationValue)

# wykres mocy predykcyjnej dla wszystkich 20 zmiennych z zaznaczonym poziomem 0.1
plot_iv <- ggplot(data_iv,aes(nazwy,InformationValue)) +
            geom_bar(stat="identity")+
            geom_hline(aes(yintercept=0.10),colour="blue")+
            theme(axis.text.x=element_text(angle=90, hjust=1))+ylim(0,1)+
            labs(title="Wartości Information Value dla wszystkich zmiennych")
print(plot_iv)

# nowy zbiór treningowy i nowy zbiór testowy
# ze skategoryzowanymi zmiennym (nazwy_iv_all), dla których IV > 0.1
nazwy_iv_all <- c(nazwy_iv,"creditability")
train_selection <- train[,..nazwy_iv_all]
train_M3<-woebin_ply(train_selection,woe,to="bin")
train_M3[] <- lapply(train_M3, factor)

# i podobnie: nowy zbiór testowy
# ze skategoryzowanymi zmiennym (nazwy_iv_all), dla których IV > 0.1
test_selection <- test[,..nazwy_iv_all]
test_M3<-woebin_ply(test_selection,woe,to="bin")
test_M3[] <- lapply(test_M3, factor)

# sprawdzamy założenia spełnia założenie dotyczące monotoniczności względem prawdopodobieństwa sukcesu estymowanego na podstawie zbioru treningowego
# chk_acct
p1 <- ggplot(data=woe$chk_acct,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej chk_acct")
print(p1)

# duration
p11 <- ggplot(data=woe$duration,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej duration")
print(p11)

# credit_his
p111 <- ggplot(data=woe$credit_his,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej credit_his")
print(p111)

# purpose
p2 <- ggplot(data=woe$purpose,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej purpose")
print(p2)

# amount
p22 <- ggplot(data=woe$amount,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej amount")
print(p22)

# saving_acct
p222 <- ggplot(data=woe$saving_acct,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej saving_acct")
print(p222)

# present_emp
p3 <- ggplot(data=woe$present_emp,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej present_emp")
print(p3)

# property
p33 <- ggplot(data=woe$property,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej property")
print(p33)

# age
p333 <- ggplot(data=woe$age,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej age")
print(p333)

# housing
p4 <- ggplot(data=woe$housing,aes(x=woe, y=1-posprob))+geom_line()+labs(title="Zależność prawdopodobieństwa sukcesu od WoE dla zmiennej housing")
print(p4)

M3 <- glm(formula = response~., 
          data = train_M3, 
          family = binomial('logit'))
# betas1 <- M3$coefficients
# sum_M3 <- summary(M3)
coef_M3 <- cbind(M3$coefficients)
coef_M3

# nowy zbiór treningowy i nowy zbiór testowy
# każdej klasie pochodzącej z danej zmiennej przyporządkowujemy odpowiadającą jej wartość WoE
train_M4 <- woebin_ply(train_selection,woe,to="woe")
test_M4 <- woebin_ply(test_selection,woe,to="woe")

# model M4 oparty na tak zmienionych zmiennych
M4<-glm(response~.,
        data = train_M4,
        family = binomial('logit'))

coef_M4 <- cbind(M4$coefficients)
coef_M4 <- data.frame(coef_M4)
colnames(coef_M4) <- c("Oszacowanie")

tab_coef_M4 <- xtable(coef_M4,
                      digits = 3, 
                      row.names = TRUE, 
                      caption = "Oszacowania współczynników w modelu M4", 
                      label = "tab:coefM4")

print(tab_coef_M4,type = "latex", table.placement = "H")

bins1 <- woebin(germancredit, y="creditability", x="duration.in.month")
plot_woe1 <- woebin_plot(bins1, line_value = "woe", show_barval=FALSE)
print(plot_woe1)

bins2 <- woebin(germancredit, y="creditability", x="present.employment.since")
plot_woe2 <- woebin_plot(bins2, line_value = "woe", show_barval=FALSE)
print(plot_woe2)

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 6 #####

#predykcja wartości zmiennej creditability w oparciu o model regresji logistycznej ze wszystkimi predyktorami typu dyskretnego
M3_pred<-predict.glm(object = M3,
                     newdata = test_M3,
                     type = "response")
M3_conf_mat<-table(real = test_M3$response, pred = as.numeric(M3_pred>0.5))
xtable(M3_conf_mat)

#predykcji wartości zmiennej creditability na dwa sposoby w oparciu o model regresji logistycznej ze wszystkimi predyktorami typu ciągłego z “nowymi” wartościami tych zmiennych, które odpowiadają WOE
M4_pred<-predict.glm(object = M4,
                     newdata = test_M4,
                     type = "response")
M4_conf_mat<-table(real=test_M4$response, pred=as.numeric(M4_pred>0.5))

tab.M3.conf.mat <- xtable(M3_conf_mat,
                          digits = 3, 
                          row.names = TRUE, 
                          caption = "macierz pomyłek dla danych testowych w modelu M3", 
                          label = "tab:confmatM3")

print(tab.M3.conf.mat,type = "latex", table.placement = "H")


tab.M4.conf.mat <- xtable(M4_conf_mat,
                          digits = 3, 
                          row.names = TRUE, 
                          caption = "macierz pomyłek dla danych testowych w modelu M4", 
                          label = "tab:confmatM4")

print(tab.M4.conf.mat,type = "latex", table.placement = "H")

# WYZNACZAMY ACC, ERROR, SENS, SPEC, PP, NP

# wskaźniki dla ogólnego modelu tab.M3.conf.mat
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
M3_acc0 <- (M3_conf_mat[2,2] + M3_conf_mat[1,1])/(M3_conf_mat[2,2] + M3_conf_mat[1,1] + M3_conf_mat[1,2] + M3_conf_mat[2,1])
M3_acc0 # 0.7084639

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
M3_err0 <- 1 - (M3_conf_mat[1,1]+M3_conf_mat[2,2])/sum(rowSums(M3_conf_mat))
M3_err0 # 0.2915361

# sensitivity
# (TP)/(FN + TP)
M3_sens0 <- (M3_conf_mat[2,2])/((M3_conf_mat[2,1] + M3_conf_mat[2,2]))
M3_sens0 # 0.8280543

# specificity
# (TN)/(TN + FP)
M3_spec0 <- (M3_conf_mat[1,1])/((M3_conf_mat[1,1]) + (M3_conf_mat[1,2]))
M3_spec0 # 0.4387755

# precision (positive prediction)
# (TP)/(FP + TP)
M3_PP0 <- (M3_conf_mat[2,2])/(M3_conf_mat[1,2] + M3_conf_mat[2,2])
M3_PP0 # 0.7689076

# negative prediction
# (TN)/(TN + FN)
M3_NP0 <- (M3_conf_mat[1,1])/(M3_conf_mat[1,1] + M3_conf_mat[2,1])
M3_NP0 # 0.5308642

# wskaźniki dla ogólnego modelu tab.M4.conf.mat
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
M4_acc0 <- (M4_conf_mat[2,2] + M4_conf_mat[1,1])/(M3_conf_mat[2,2] + M4_conf_mat[1,1] + M4_conf_mat[1,2] + M4_conf_mat[2,1])
M4_acc0 # 0.7018634

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
M4_err0 <- 1 - M4_acc0
M4_err0 # 0.2981366

# sensitivity
# (TP)/(FN + TP)
M4_sens0 <- (M4_conf_mat[2,2])/((M4_conf_mat[2,1] + M4_conf_mat[2,2]))
M4_sens0 # 0.8144796

# specificity
# (TN)/(TN + FP)
M4_spec0 <- (M4_conf_mat[1,1])/((M4_conf_mat[1,1]) + (M4_conf_mat[1,2]))
M4_spec0 # 0.4693878

# precision (positive prediction)
# (TP)/(FP + TP)
M4_PP0 <- (M4_conf_mat[2,2])/(M4_conf_mat[1,2] + M4_conf_mat[2,2])
M4_PP0 # 0.7758621

# negative prediction
# (TN)/(TN + FN)
M4_NP0 <- (M4_conf_mat[1,1])/(M4_conf_mat[1,1] + M4_conf_mat[2,1])
M4_NP0 # 0.5287356

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 7 #####

# DODAJ CZTERY MACIERZE POMYŁEK ODPOWIADAJĄCE POSZCZEGÓLNYM MODELOM
tab.M1.conf.mat <- xtable(M1_conf_mat,
               digits = 3, 
               row.names = TRUE, 
               caption = "macierz pomyłek dla danych testowych w modelu M1", 
               label = "tab:confmatM3")

print(tab.M1.conf.mat,type = "latex", table.placement = "H")


tab.M2.conf.mat <- xtable(M2_conf_mat,
               digits = 3, 
               row.names = TRUE, 
               caption = "macierz pomyłek dla danych testowych w modelu M2", 
               label = "tab:confmatM4")

print(tab.M2.conf.mat,type = "latex", table.placement = "H")

tab.M3.conf.mat <- xtable(M3_conf_mat,
               digits = 3, 
               row.names = TRUE, 
               caption = "macierz pomyłek dla danych testowych w modelu M3", 
               label = "tab:confmatM3")

print(tab.M3.conf.mat,type = "latex", table.placement = "H")


tab.M4.conf.mat <- xtable(M4_conf_mat,
               digits = 3, 
               row.names = TRUE, 
               caption = "macierz pomyłek dla danych testowych w modelu M4", 
               label = "tab:confmatM4")

print(tab.M4.conf.mat,type = "latex", table.placement = "H")

# DODAJ TABELĘ WARTOŚCI WSZYSTKICH DOKŁADNOŚCI ITD
# MODEL M1
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
M1_acc0 <- (M1_conf_mat[2,2] + M1_conf_mat[1,1])/(M1_conf_mat[2,2] + M1_conf_mat[1,1] + M1_conf_mat[1,2] + M1_conf_mat[2,1])
M1_acc0 # 0.7272727

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
M1_err0 <- 1 - (M1_conf_mat[1,1]+M1_conf_mat[2,2])/sum(rowSums(M1_conf_mat))
M1_err0 # 0.2727273

# sensitivity
# (TP)/(FN + TP)
M1_sens0 <- (M1_conf_mat[2,2])/((M1_conf_mat[2,1] + M1_conf_mat[2,2]))
M1_sens0 # 0.8190045

# specificity
# (TN)/(TN + FP)
M1_spec0 <- (M1_conf_mat[1,1])/((M1_conf_mat[1,1]) + (M1_conf_mat[1,2]))
M1_spec0 # 0.5204082

# precision (positive prediction)
# (TP)/(FP + TP)
M1_PP0 <- (M1_conf_mat[2,2])/(M1_conf_mat[1,2] + M1_conf_mat[2,2])
M1_PP0 # 0.7938596

# negative prediction
# (TN)/(TN + FN)
M1_NP0 <- (M1_conf_mat[1,1])/(M1_conf_mat[1,1] + M1_conf_mat[2,1])
M1_NP0 # 0.5604396

# MODEL M2
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
M2_acc0 <- (M2_conf_mat[2,2] + M2_conf_mat[1,1])/(M2_conf_mat[2,2] + M2_conf_mat[1,1] + M2_conf_mat[1,2] + M2_conf_mat[2,1])
M2_acc0 # 0.7084639

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
M2_err0 <- 1 - (M2_conf_mat[1,1]+M2_conf_mat[2,2])/sum(rowSums(M2_conf_mat))
M2_err0 # 0.2915361

# sensitivity
# (TP)/(FN + TP)
M2_sens0 <- (M2_conf_mat[2,2])/((M2_conf_mat[2,1] + M2_conf_mat[2,2]))
M2_sens0 # 0.8461538

# specificity
# (TN)/(TN + FP)
M2_spec0 <- (M2_conf_mat[1,1])/((M2_conf_mat[1,1]) + (M2_conf_mat[1,2]))
M2_spec0 # 0.3979592

# precision (positive prediction)
# (TP)/(FP + TP)
M2_PP0 <- (M2_conf_mat[2,2])/(M2_conf_mat[1,2] + M2_conf_mat[2,2])
M2_PP0 # 0.7601626

# negative prediction
# (TN)/(TN + FN)
M2_NP0 <- (M2_conf_mat[1,1])/(M2_conf_mat[1,1] + M2_conf_mat[2,1])
M2_NP0 # 0.5342466

# MODEL M3
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
M3_acc0 <- (M3_conf_mat[2,2] + M3_conf_mat[1,1])/(M3_conf_mat[2,2] + M3_conf_mat[1,1] + M3_conf_mat[1,2] + M3_conf_mat[2,1])
M3_acc0 # 0.7084639

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
M3_err0 <- 1 - (M3_conf_mat[1,1]+M3_conf_mat[2,2])/sum(rowSums(M3_conf_mat))
M3_err0 # 0.2915361

# sensitivity
# (TP)/(FN + TP)
M3_sens0 <- (M3_conf_mat[2,2])/((M3_conf_mat[2,1] + M3_conf_mat[2,2]))
M3_sens0 # 0.8280543

# specificity
# (TN)/(TN + FP)
M3_spec0 <- (M3_conf_mat[1,1])/((M3_conf_mat[1,1]) + (M3_conf_mat[1,2]))
M3_spec0 # 0.4387755

# precision (positive prediction)
# (TP)/(FP + TP)
M3_PP0 <- (M3_conf_mat[2,2])/(M3_conf_mat[1,2] + M3_conf_mat[2,2])
M3_PP0 # 0.7689076

# negative prediction
# (TN)/(TN + FN)
M3_NP0 <- (M3_conf_mat[1,1])/(M3_conf_mat[1,1] + M3_conf_mat[2,1])
M3_NP0 # 0.5308642

# MODEL M4
# accuracy
# (TP + TN)/(TN + FP + FN + TP)
M4_acc0 <- (M4_conf_mat[2,2] + M4_conf_mat[1,1])/(M3_conf_mat[2,2] + M4_conf_mat[1,1] + M4_conf_mat[1,2] + M4_conf_mat[2,1])
M4_acc0 # 0.7018634

# error
# (1 - (TP + TN)/(TN + FP + FN + TP))
M4_err0 <- 1 - M4_acc0
M4_err0 # 0.2981366

# sensitivity
# (TP)/(FN + TP)
M4_sens0 <- (M4_conf_mat[2,2])/((M4_conf_mat[2,1] + M4_conf_mat[2,2]))
M4_sens0 # 0.8144796

# specificity
# (TN)/(TN + FP)
M4_spec0 <- (M4_conf_mat[1,1])/((M4_conf_mat[1,1]) + (M4_conf_mat[1,2]))
M4_spec0 # 0.4693878

# precision (positive prediction)
# (TP)/(FP + TP)
M4_PP0 <- (M4_conf_mat[2,2])/(M4_conf_mat[1,2] + M4_conf_mat[2,2])
M4_PP0 # 0.7758621

# negative prediction
# (TN)/(TN + FN)
M4_NP0 <- (M4_conf_mat[1,1])/(M4_conf_mat[1,1] + M4_conf_mat[2,1])
M4_NP0 # 0.5287356

# Drugi sposób oceny efektywności modelu scoringowego opiera się na rozkładzie klientów dobrych i złych

# Wykres krzywej roc dostajemy poleceniem 
plotROC(actuals = test$creditability, predictedScores = M1_pred)
plotROC(actuals = test$creditability, predictedScores = M2_pred)
plotROC(actuals = test_M3$creditability, predictedScores = M3_pred)
plotROC(actuals = test_M4$creditability, predictedScores = M4_pred)

# Wartość AUC dostajemy poleceniem
M1_AUC <- AUROC(actuals = test$creditability, predictedScores = M1_pred)
M1_AUC # 0.7559101
M2_AUC <- AUROC(actuals = test$creditability, predictedScores = M2_pred)
M2_AUC # 0.7354326
M3_AUC <- AUROC(actuals = test_M3$creditability, predictedScores = M3_pred)
M3_AUC # 0.7567873
M4_AUC <- AUROC(actuals = test_M4$creditability, predictedScores = M4_pred)
M4_AUC # 0.7595346

# Tworzymy odpowiednie obiekty dla ggplot funkcją roc z pakietu pROC
roc.M1 <- roc(test$creditability, M1_pred, auc=TRUE)
roc.M2 <- roc(test$creditability, M2_pred)
roc.M3 <- roc(test_M3$creditability, M3_pred)
roc.M4 <- roc(test_M4$creditability, M4_pred)

# wpisujemy je w listę
roc.list <- list(M1 = roc.M1, M2 = roc.M2,M3 = roc.M3,M4 = roc.M4)

# tworzymy etykiety z wartośiami AUC do wpisania w legendę
et1 <- paste0("M1, AUC = ",round(M1_AUC,4))
et2 <- paste0("M2, AUC = ",round(M2_AUC,4))
et3 <- paste0("M3, AUC = ",round(M3_AUC,4))
et4 <- paste0("M4, AUC = ",round(M4_AUC,4))

# umieszczamy 4 krzywe ROC na jednym obrazku
ggroc(roc.list, legacy.axes = TRUE)+
  scale_color_discrete(labels=c(et1,et2,et3,et4))+
  labs(x="1-Specyficzność", y= "Czułość",color="Krzywa ROC i wyznaczone AUC poszczególnych modeli")+
  theme(legend.position=c(0.75,0.25))

# Statystyka Kolmogorowa - Smirnowa
# Wartości AUC oraz KS wyznaczamy poniżej
ks_M1 <- ks_stat(test$creditability, M1_pred)
ks_M2 <- ks_stat(test$creditability, M2_pred)
ks_M3 <- ks_stat(test_M3$creditability, M3_pred)
ks_M4 <- ks_stat(test_M4$creditability, M4_pred)

auc.ks_tab <- data.frame(AUC=c(M1_AUC,M2_AUC,M3_AUC,M4_AUC), KS=c(ks_M1,ks_M2,ks_M3,ks_M4)) 
row.names(auc.ks_tab) <- c("M1","M2","M3","M4")

auc.ks_tab <- xtable(auc.ks_tab,
                     digits = 4, 
                     row.names = TRUE, 
                     caption = "Wartości AUC oraz statystyki Kołmogorowa-Smirnowa.", 
                     label = "tab:aucks")

print(auc.ks_tab,type = "latex", table.placement = "H")

##### DOROBIĆ #####
# Który z modeli ma najwięcej/najmniej błędów I/II rodzaju, które z tych błędów są istotniejsze w badanym zagadnieniu?
# Czy jesteśmy w stanie jednoznacznie powiedzieć, który z modeli jest najlepszy? Innymi słowy: czy pewien model osiąga najlepszych wartości każdego z parametrów jednocześnie?
# Opisać, który model ma najlepszą dokładność, a który czułość itd.

####################################################################################################
####################################################################################################
####################################################################################################

##### ZADANIE 8 #####

# M1
cut.off.M1.ones <- optimalCutoff(test$creditability, M1_pred,"Ones")
cut.off.M1.zeros <- optimalCutoff(test$creditability, M1_pred,"Zeros")
cut.off.M1.both <- optimalCutoff(test$creditability, M1_pred,"Both")
cut.off.M1.def <- optimalCutoff(test$creditability, M1_pred,"misclasserror")

# M2
cut.off.M2.ones <- optimalCutoff(test$creditability, M2_pred,"Ones")
cut.off.M2.zeros <- optimalCutoff(test$creditability, M2_pred,"Zeros")
cut.off.M2.both <- optimalCutoff(test$creditability, M2_pred,"Both")
cut.off.M2.def <- optimalCutoff(test$creditability, M2_pred,"misclasserror")

# M3
cut.off.M3.ones <- optimalCutoff(test_M3$creditability, M3_pred,"Ones")
cut.off.M3.zeros <- optimalCutoff(test_M3$creditability, M3_pred,"Zeros")
cut.off.M3.both <- optimalCutoff(test_M3$creditability, M3_pred,"Both")
cut.off.M3.def <- optimalCutoff(test_M3$creditability, M3_pred,"misclasserror")

# M4
cut.off.M4.ones <- optimalCutoff(test_M4$creditability, M4_pred,"Ones")
cut.off.M4.zeros <- optimalCutoff(test_M4$creditability, M4_pred,"Zeros")
cut.off.M4.both <- optimalCutoff(test_M4$creditability, M4_pred,"Both")
cut.off.M4.def <- optimalCutoff(test_M4$creditability, M4_pred,"misclasserror")


cut.off_tab <- data.frame(Ones = c(cut.off.M1.ones,
                                   cut.off.M2.ones,
                                   cut.off.M3.ones,
                                   cut.off.M4.ones), 
                          Zeros = c(cut.off.M1.zeros,
                                    cut.off.M2.zeros,
                                    cut.off.M2.zeros,
                                    cut.off.M2.zeros),
                          Both = c(cut.off.M1.both,
                                   cut.off.M2.both,
                                   cut.off.M3.both,
                                   cut.off.M4.both),
                          missclasserror = c(cut.off.M1.def,
                                             cut.off.M2.def,
                                             cut.off.M3.def,
                                             cut.off.M4.def)) 

row.names(cut.off_tab) <- c("M1","M2","M3","M4")

cut.off_tab <- xtable(cut.off_tab,
                      digits = 4, 
                      row.names = TRUE, 
                      caption = "Punkty odcięcia dla każdego z modeli", 
                      label = "tab:cutoff")

print(cut.off_tab,type = "latex", table.placement = "H")

##### ##### ##### ##### ##### ##### ##### #####
##### CUT.OFF.BOTH #####
##### ##### ##### ##### ##### ##### ##### #####

# MODEL M1
#accuracy
M1_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.both)
M1_acc # 0.6458

# error
M1_err <- misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.both)
M1_err # 0.3542

#sensitivity
M1_sens <- sensitivity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.both)
M1_sens # 0.5791855

s#specificity
M1_spec <- specificity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.both)
M1_spec # 0.7959184

#precision (positive prediction)
M1_PP <- precision(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.both)
M1_PP # 0.8648649

#negative prediction
M1_NP <- npv(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.both)
M1_NP # 0.4561404

# MODEL M2
#accuracy
M2_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.both)
M2_acc # 0.7022

# error
M2_err <- misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.both)
M2_err # 0.2978

#sensitivity
M2_sens <- sensitivity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.both)
M2_sens # 0.7058824

#specificity
M2_spec <- specificity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.both)
M2_spec # 0.6938776

#precision (positive prediction)
M2_PP <- precision(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.both)
M2_PP # 0.8387097

#negative prediction
M2_NP <- npv(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.both)
M2_NP # 0.5112782

# MODEL M3
#accuracy
M3_acc <- 1-misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.both)
M3_acc # 0.6928

# error
M3_err <- misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.both)
M3_err # 0.3072

#sensitivity
M3_sens <- sensitivity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.both)
M3_sens # 0.6742081

#specificity
M3_spec <- specificity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.both)
M3_spec # 0.7346939

#precision (positive prediction)
M3_PP <- precision(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.both)
M3_PP # 0.8514286

#negative prediction
M3_NP <- npv(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.both)
M3_NP # 0.5

# MODEL M4
#accuracy
M4_acc <- 1-misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.both)
M4_acc # 0.7179

# error
M4_err <- misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.both)
M4_err # 0.2821

#sensitivity
M4_sens <- sensitivity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.both)
M4_sens # 0.7239819

#specificity
M4_spec <- specificity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.both)
M4_spec # 0.7040816

#precision (positive prediction)
M4_PP <- precision(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.both)
M4_PP # 0.8465608

#negative prediction
M4_NP <- npv(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.both)
M4_NP # 0.5307692

##### ##### ##### ##### ##### ##### ##### #####
##### CUT.OFF.ONES #####
##### ##### ##### ##### ##### ##### ##### #####

# MODEL M1
#accuracy
M1_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.ones)
M1_acc # 0.7085

# error
M1_err <- misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.ones)
M1_err # 0.2915

#sensitivity
M1_sens <- sensitivity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.ones)
M1_sens # 0.5791855

s#specificity
M1_spec <- specificity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.ones)
M1_spec # 0.9909502

#precision (positive prediction)
M1_PP <- precision(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.ones)
M1_PP # 0.7064516

#negative prediction
M1_NP <- npv(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.ones)
M1_NP # 0.7777778

# MODEL M2
#accuracy
M2_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.ones)
M2_acc # 0.7022

# error
M2_err <- misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.ones)
M2_err # 0.2978

#sensitivity
M2_sens <- sensitivity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.ones)
M2_sens # 1

#specificity
M2_spec <- specificity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.ones)
M2_spec # 0.03061224

#precision (positive prediction)
M2_PP <- precision(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.ones)
M2_PP # 0.6993671

#negative prediction
M2_NP <- npv(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.ones)
M2_NP # 1

# MODEL M3
#accuracy
M3_acc <- 1-misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.ones)
M3_acc # 0.6959

# error
M3_err <- misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.ones)
M3_err # 0.3041

#sensitivity
M3_sens <- sensitivity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.ones)
M3_sens # 1

#specificity
M3_spec <- specificity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.ones)
M3_spec # 0.01020408

#precision (positive prediction)
M3_PP <- precision(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.ones)
M3_PP # 0.6949686

#negative prediction
M3_NP <- npv(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.ones)
M3_NP # 1

# MODEL M4
#accuracy
M4_acc <- 1-misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.ones)
M4_acc # 0.6959

# error
M4_err <- misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.ones)
M4_err # 0.3041

#sensitivity
M4_sens <- sensitivity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.ones)
M4_sens # 1

#specificity
M4_spec <- specificity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.ones)
M4_spec # 0.01020408

#precision (positive prediction)
M4_PP <- precision(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.ones)
M4_PP # 0.6949686

#negative prediction
M4_NP <- npv(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.ones)
M4_NP # 1

##### ##### ##### ##### ##### ##### ##### #####
##### CUT.OFF.ZEROS #####
##### ##### ##### ##### ##### ##### ##### #####

# MODEL M1
#accuracy
M1_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.zeros)
M1_acc # 0.3574

# error
M1_err <- misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.zeros)
M1_err # 0.6426

#sensitivity
M1_sens <- sensitivity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.zeros)
M1_sens # 0.07239819

#specificity
M1_spec <- specificity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.zeros)
M1_spec # 1

#precision (positive prediction)
M1_PP <- precision(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.zeros)
M1_PP # 1

#negative prediction
M1_NP <- npv(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.zeros)
M1_NP # 0.3234323

# MODEL M2
#accuracy
M2_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.zeros)
M2_acc # 0.3323

# error
M2_err <- misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.zeros)
M2_err # 0.6677

#sensitivity
M2_sens <- sensitivity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.zeros)
M2_sens # 0.0361991

#specificity
M2_spec <- specificity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.zeros)
M2_spec # 1

#precision (positive prediction)
M2_PP <- precision(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.zeros)
M2_PP # 1

#negative prediction
M2_NP <- npv(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.zeros)
M2_NP # 0.3151125

# MODEL M3
#accuracy
M3_acc <- 1-misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.zeros)
M3_acc # 0.3542

# error
M3_err <- misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.zeros)
M3_err # 0.6458

#sensitivity
M3_sens <- sensitivity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.zeros)
M3_sens # 0.0678733

#specificity
M3_spec <- specificity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.zeros)
M3_spec # 1

#precision (positive prediction)
M3_PP <- precision(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.zeros)
M3_PP # 1

#negative prediction
M3_NP <- npv(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.zeros)
M3_NP # 0.3223684

# MODEL M4
#accuracy
M4_acc <- 1-misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.zeros)
M4_acc # 0.3103

# error
M4_err <- misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.zeros)
M4_err # 0.6897

#sensitivity
M4_sens <- sensitivity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.zeros)
M4_sens # 0.004524887

#specificity
M4_spec <- specificity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.zeros)
M4_spec # 1

#precision (positive prediction)
M4_PP <- precision(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.zeros)
M4_PP # 1

#negative prediction
M4_NP <- npv(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.zeros)
M4_NP # 0.3081761

##### ##### ##### ##### ##### ##### ##### #####
##### CUT.OFF.missclasserror #####
##### ##### ##### ##### ##### ##### ##### #####

# MODEL M1
#accuracy
M1_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.def)
M1_acc # 0.7398

# error
M1_err <- misClassError(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.def)
M1_err # 0.2602

#sensitivity
M1_sens <- sensitivity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.def)
M1_sens # 0.9095023

s#specificity
M1_spec <- specificity(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.def)
M1_spec # 0.3571429

#precision (positive prediction)
M1_PP <- precision(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.def)
M1_PP # 0.7613636

#negative prediction
M1_NP <- npv(actuals = test$creditability, predictedScores = M1_pred, threshold = cut.off.M1.def)
M1_NP # 0.6363636

# MODEL M2
#accuracy
M2_acc <- 1-misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.def)
M2_acc # 0.7398

# error
M2_err <- misClassError(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.def)
M2_err # 0.2602

#sensitivity
M2_sens <- sensitivity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.def)
M2_sens # 0.9773756

#specificity
M2_spec <- specificity(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.def)
M2_spec # 0.2040816

#precision (positive prediction)
M2_PP <- precision(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.def)
M2_PP # 0.7346939

#negative prediction
M2_NP <- npv(actuals = test$creditability, predictedScores = M2_pred, threshold = cut.off.M2.def)
M2_NP # 0.8

# MODEL M3
#accuracy
M3_acc <- 1-misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.def)
M3_acc # 0.6928

# error
M3_err <- misClassError(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.def)
M3_err # 0.7398

#sensitivity
M3_sens <- sensitivity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.def)
M3_sens # 0.9411765

#specificity
M3_spec <- specificity(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.def)
M3_spec # 0.2857143

#precision (positive prediction)
M3_PP <- precision(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.def)
M3_PP # 0.7482014

#negative prediction
M3_NP <- npv(actuals = test_M3$creditability, predictedScores = M3_pred, threshold = cut.off.M3.def)
M3_NP # 0.6829268

# MODEL M4
#accuracy
M4_acc <- 1-misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.def)
M4_acc # 0.7335

# error
M4_err <- misClassError(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.def)
M4_err # 0.2665

#sensitivity
M4_sens <- sensitivity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.def)
M4_sens # 0.9230769

#specificity
M4_spec <- specificity(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.def)
M4_spec # 0.3061224

#precision (positive prediction)
M4_PP <- precision(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.def)
M4_PP # 0.75

#negative prediction
M4_NP <- npv(actuals = test_M4$creditability, predictedScores = M4_pred, threshold = cut.off.M4.def)
M4_NP # 0.6382979
