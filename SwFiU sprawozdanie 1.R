###################################
###################################
##### ZADANIE 1
###################################
###################################

# WCZYTUJEMY BIBLIOTEKI

library(libstableR)
library(ggplot2)
library(gridExtra)
library(grid)
library(xtable)

# Ustalamy parametry. Generujemy N = 100 prób rozmiaru n1 = 100 oraz n2 = 1000.
N <- 100
n1 <- 100
n2 <- 1000

# Zadane rozkłady stabilne. 
pars1 <- c(1, 0, 1, 0)
pars2 <- c(2, 0, 1, 0)
pars3 <- c(1/2, 0, 1, 0)

# Przykładowy wykres jednomodalny
plot(function(x) dnorm(x), -4, 4, main = "Wykres gęstości rozkładu normalnego N(0,1)", col = "green", ylab = "funkcja prawdopodobieństwa")


# Generujemy wykresy gęstości zadanych rozkładów. 
sek <- seq(-6, 6, 0.01)
pdf1 <- stable_pdf(sek, pars1)
pdf2 <- stable_pdf(sek, pars2)
pdf3 <- stable_pdf(sek, pars3)

df1 <- data.frame(pdf1)
df2 <- data.frame(pdf2)
df3 <- data.frame(pdf3)

plot1 <- ggplot(df1, aes(x = sek, y = pdf1)) +
  stat_function(fun = stable_pdf, args = list(pars = pars1)) +
  labs(x = "(1, 0, 1, 0)", y = NULL) + geom_line(colour = "#008000")

plot2 <- ggplot(df2, aes(x = sek, y = pdf2)) +
  stat_function(fun = stable_pdf, args = list(pars = pars2)) +
  labs(x = "(2, 0, 1, 0)", y = NULL) + geom_line(colour = "#008000")

plot3 <- ggplot(df3, aes(x = sek, y = pdf3)) +
  stat_function(fun = stable_pdf, args = list(pars = pars3)) +
  labs(x = "(1/2, 0, 1, 0)", y = NULL) + geom_line(colour = "#008000")

grid.arrange(plot1, plot2, plot3, nrow = 3, ncol = 1)

###################################

# Generujemy N = 100 wektorów rozmiaru n1 = 100 z każdego rozkładu stabilnego

# PARS1 = (1, 0, 1, 0)
rnd1 <- stable_rnd(N * n1, pars1)

mat1 <- matrix(
  data = rnd1,
  nrow = N,
  ncol = n1,
  byrow = TRUE
)

maximas1 <- apply(mat1, 1, max)

min1 <- min(maximas1)
q1 <- quantile(maximas1, 0.25)
q3 <- quantile(maximas1, 0.75)
max1 <- max(maximas1)

mat1_summ <- t(as.matrix(summary(maximas1)[c(-(3:4))]))
print(xtable(mat1_summ, caption = "Podstawowe parametry rozkładu stabilnego (1, 0, 1, 0) dla próby n = 100"), include.rownames = FALSE)

# PARS2 = (2, 0, 1, 0)
rnd2 <- stable_rnd(N * n1, pars2)

mat2 <- matrix(
  data = rnd2,
  nrow = N,
  ncol = n1,
  byrow = TRUE
)

maximas2 <- apply(mat2, 1, max)

min2 <- min(maximas2)
q12 <- quantile(maximas2, 0.25)
q32 <- quantile(maximas2, 0.75)
max2 <- max(maximas2)

mat2_summ <- t(as.matrix(summary(maximas2)[c(-(3:4))]))
print(xtable(mat2_summ, caption = "Podstawowe parametry rozkładu stabilnego (2, 0, 1, 0) dla próby n = 100"), include.rownames = FALSE)

# PARS3 = (1/2, 0, 1, 0)
rnd3 <- stable_rnd(N * n1, pars3)

mat3 <- matrix(
  data = rnd3,
  nrow = N,
  ncol = n1,
  byrow = TRUE
)

maximas3 <- apply(mat3, 1, max)

min3 <- min(maximas3)
q13 <- quantile(maximas3, 0.25)
q33 <- quantile(maximas3, 0.75)
max3 <- max(maximas3)

mat3_summ <- t(as.matrix(summary(maximas3)[c(-(3:4))]))
print(xtable(mat3_summ, caption = "Podstawowe parametry rozkładu stabilnego (1/2, 0, 1, 0), dla próby n = 100"), include.rownames = FALSE)

###################################

# Generujemy N = 1000 wektorów rozmiaru n2 = 1000 z każdego rozkładu stabilnego

# PARS1 = (1, 0, 1, 0)
rnd1_1 <- stable_rnd(N * n2, pars1)

mat1_1 <- matrix(
  data = rnd1_1,
  nrow = N,
  ncol = n2,
  byrow = TRUE
)

maximas1_1 <- apply(mat1_1, 1, max)

min1_1 <- min(maximas1_1)
q1_1 <- quantile(maximas1_1, 0.25)
q3_1 <- quantile(maximas1_1, 0.75)
max1_1 <- max(maximas1_1)

mat1_1_summ <- t(as.matrix(summary(maximas1_1)[c(-(3:4))]))
print(xtable(mat1_1_summ, caption = "Podstawowe parametry rozkładu stabilnego (1, 0, 1, 0), dla próby n = 1000"), include.rownames = FALSE)

# PARS2 = (2, 0, 1, 0)
rnd2_1 <- stable_rnd(N * n2, pars2)

mat2_1 <- matrix(
  data = rnd2_1,
  nrow = N,
  ncol = n2,
  byrow = TRUE
)

maximas2_1 <- apply(mat2_1, 1, max)

min2_1 <- min(maximas2_1)
q12_1 <- quantile(maximas2_1, 0.25)
q32_1 <- quantile(maximas2_1, 0.75)
max2_1 <- max(maximas2_1)

mat2_1_summ <- t(as.matrix(summary(maximas2_1)[c(-(3:4))]))
print(xtable(mat2_1_summ, caption = "Podstawowe parametry rozkładu stabilnego (2, 0, 1, 0), dla próby n = 1000"), include.rownames = FALSE)

# PARS3 = (1/2, 0, 1, 0)
rnd3_1 <- stable_rnd(N * n2, pars3)

mat3_1 <- matrix(
  data = rnd3_1,
  nrow = N,
  ncol = n2,
  byrow = TRUE
)

maximas3_1 <- apply(mat3_1, 1, max)

min3_1 <- min(maximas3_1)
q13_1 <- quantile(maximas3_1, 0.25)
q33_1 <- quantile(maximas3_1, 0.75)
max3_1 <- max(maximas3_1)

mat3_1_summ <- t(as.matrix(summary(maximas3_1)[c(-(3:4))]))
print(xtable(mat3_1_summ, caption = "Podstawowe parametry rozkładu stabilnego (1/2, 0, 1, 0), dla próby n = 1000"), include.rownames = FALSE)


# Funkcja DOP - dopasowanie parametrow dla zadanej proby
dop <- function(n, par){
    p <- stable_rnd(n, par)
    wyn_i <- stable_fit_init(p)
    wyn_k <- stable_fit_koutrouvelis(p, wyn_i)
    wyn_m <- stable_fit_mle(p, wyn_k)
    wyn_m2 <- stable_fit_mle2d(p, wyn_k)
    return(list(p,rbind(wyn_i, wyn_k, wyn_m, wyn_m2)))
}

# Funkcja GD - dopasowanie parametrów dla wygenerowanej symulacji

gd <- function(n, param, l){
  s_i <- matrix(0, nrow=0, ncol=4)
  s_k <- matrix(0, nrow=0, ncol=4)
  s_m <- matrix(0, nrow=0, ncol=4)
  s_m2 <- matrix(0, nrow=0, ncol=4)
  proba <- matrix(0, ncol=0, nrow = n)
  for (i in 1:l){
    set.seed(i)
    m0 <- dop(n,param)
    proba <- cbind(proba,m0[[1]])
    m <-m0[[2]]
    s_i <- rbind(s_i, m[1,])
    s_k <- rbind(s_k, m[2,])
    s_m <- rbind(s_m, m[3,])
    s_m2 <- rbind(s_m2, m[4,])
    }
  tab <- data.frame(gr=c(rep(1,l), rep(2,l), rep(3,l), rep(4,l)),
    p1 = c(s_i[,1], s_k[,1], s_m[,1], s_m2[,1]),
    p2 = c(s_i[,2], s_k[,2], s_m[,2], s_m2[,2]),
    p3 = c(s_i[,3], s_k[,3], s_m[,3], s_m2[,3]),
    p4 = c(s_i[,4], s_k[,4], s_m[,4], s_m2[,4]))
  tab$gr <- as.factor(tab$gr)
  return(list(proba, tab))
}

# Generujemy dla małej próby numb1 = 20

numb1 = 20  

a_11 <- gd(n=n1, param=pars1, l=numb1)
a_12 <- gd(n=n1, param=pars2, l=numb1)
a_13 <- gd(n=n1, param=pars3, l=numb1)

a_11_1 <- round(apply(a_11[[2]][a_11[[2]]$gr == 1,2:5],2,median),digits=4)
a_11_2 <- round(apply(a_11[[2]][a_11[[2]]$gr == 2,2:5],2,median),digits=4)
a_11_3 <- round(apply(a_11[[2]][a_11[[2]]$gr == 3,2:5],2,median),digits=4)
a_11_4 <- round(apply(a_11[[2]][a_11[[2]]$gr == 4,2:5],2,median),digits=4)

a_12_1 <- round(apply(a_12[[2]][a_12[[2]]$gr == 1,2:5],2,median),digits=4)
a_12_2 <- round(apply(a_12[[2]][a_12[[2]]$gr == 2,2:5],2,median),digits=4)
a_12_3 <- round(apply(a_12[[2]][a_12[[2]]$gr == 3,2:5],2,median),digits=4)
a_12_4 <- round(apply(a_12[[2]][a_12[[2]]$gr == 4,2:5],2,median),digits=4)

a_13_1 <- round(apply(a_13[[2]][a_13[[2]]$gr == 1,2:5],2,median),digits=4)
a_13_2 <- round(apply(a_13[[2]][a_13[[2]]$gr == 2,2:5],2,median),digits=4)
a_13_3 <- round(apply(a_13[[2]][a_13[[2]]$gr == 3,2:5],2,median),digits=4)
a_13_4 <- round(apply(a_13[[2]][a_13[[2]]$gr == 4,2:5],2,median),digits=4)

# Generujemy dla dużej próby numb2 = 10

numb2 = 10

a_21 <- gd(n=n2, param=pars1, l=numb2)
a_22 <- gd(n=n2, param=pars2, l=numb2)
a_23 <- gd(n=n2, param=pars3, l=numb2)


a_21_1 <- round(apply(a_21[[2]][a_21[[2]]$gr == 1,2:5],2,median),digits=4)
a_21_2 <- round(apply(a_21[[2]][a_21[[2]]$gr == 2,2:5],2,median),digits=4)
a_21_3 <- round(apply(a_21[[2]][a_21[[2]]$gr == 3,2:5],2,median),digits=4)
a_21_4 <- round(apply(a_21[[2]][a_21[[2]]$gr == 4,2:5],2,median),digits=4)

a_22_1 <- round(apply(a_22[[2]][a_22[[2]]$gr == 1,2:5],2,median),digits=4)
a_22_2 <- round(apply(a_22[[2]][a_22[[2]]$gr == 2,2:5],2,median),digits=4)
a_22_3 <- round(apply(a_22[[2]][a_22[[2]]$gr == 3,2:5],2,median),digits=4)
a_22_4 <- round(apply(a_22[[2]][a_22[[2]]$gr == 4,2:5],2,median),digits=4)

a_23_1 <- round(apply(a_23[[2]][a_23[[2]]$gr == 1,2:5],2,median),digits=4)
a_23_2 <- round(apply(a_23[[2]][a_23[[2]]$gr == 2,2:5],2,median),digits=4)
a_23_3 <- round(apply(a_23[[2]][a_23[[2]]$gr == 3,2:5],2,median),digits=4)
a_23_4 <- round(apply(a_23[[2]][a_23[[2]]$gr == 4,2:5],2,median),digits=4)

# Tworzymy tabelkę z oszacowaniami parametrów rozkładu stabilnego dla metody McCullocha i Koutrouvelisa

met1 <- data.frame(n1p1=c(a_11_1,a_11_2),
                  n1p2=c(a_12_1,a_12_2),
                  n1p3=c(a_13_1,a_13_2),
                  n2p1=c(a_21_1,a_21_2),
                  n2p2=c(a_22_1,a_22_2),
                  n2p3=c(a_23_1,a_23_2)
                  )
met1 <- t(as.matrix(met1))
rownames(met1) <- c("(n1,p1)","(n1,p2)","(n1,p3)","(n2,p1)","(n2,p2)","(n2,p3)")
colnames(met1) <- c("alpha1","beta1","sigma1","mu1","alpha2","beta2","sigma2","mu2")

met1 <- xtable(met1, 
               digits = 3, 
               row.names = TRUE, 
               caption = "Oszacowania parametrów rozkładu stabilnego dla metody I oraz II", 
               label = "tab:met1")

print(met1, type = "latex", table.placement = "H")

# Tworzymy tabelkę z oszacowaniami parametrów rozkładu stabilnego dla metody NW i ZNW

met2 <- data.frame(n1p1=c(a_11_3,a_11_4),
                  n1p2=c(a_12_3,a_12_4),
                  n1p3=c(a_13_3,a_13_4),
                  n2p1=c(a_21_3,a_21_4),
                  n2p2=c(a_22_3,a_22_4),
                  n2p3=c(a_23_3,a_23_4)
                  )
met2 <- t(as.matrix(met2))
rownames(met2) <- c("(n1,p1)","(n1,p2)","(n1,p3)","(n2,p1)","(n2,p2)","(n2,p3)")
colnames(met2) <- c("alpha1","beta1","sigma1","mu1","alpha2","beta2","sigma2","mu2")

met2 <- xtable(met2, 
               digits = 3, 
               row.names = TRUE, 
               caption = "Oszacowania parametrów rozkładu stabilnego dla metody III oraz IV", 
               label = "tab:met2")

print(met2, type = "latex", table.placement = "H")

# Tworzymy boxploty do wygenerowanych metod

bp11a <- ggplot(a_11[[2]],aes(x=gr,y=p1)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p1")
bp11b <- ggplot(a_11[[2]],aes(x=gr,y=p2)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p2")
bp11c <- ggplot(a_11[[2]],aes(x=gr,y=p3)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p3")
bp11d <- ggplot(a_11[[2]],aes(x=gr,y=p4)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p4")

bp21a <- ggplot(a_21[[2]],aes(x=gr,y=p1)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p1")
bp21b <- ggplot(a_21[[2]],aes(x=gr,y=p2)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p2")
bp21c <- ggplot(a_21[[2]],aes(x=gr,y=p3)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p3")
bp21d <- ggplot(a_21[[2]],aes(x=gr,y=p4)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p4")

grid.arrange(bp11a,bp21a,bp11b,bp21b,bp11c,bp21c,bp11d,bp21d,nrow=4,ncol=2)

bp11a <- ggplot(a_12[[2]],aes(x=gr,y=p1)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p1")
bp11b <- ggplot(a_12[[2]],aes(x=gr,y=p2)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p2")
bp11c <- ggplot(a_12[[2]],aes(x=gr,y=p3)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p3")
bp11d <- ggplot(a_12[[2]],aes(x=gr,y=p4)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p4")

bp21a <- ggplot(a_22[[2]],aes(x=gr,y=p1)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p1")
bp21b <- ggplot(a_22[[2]],aes(x=gr,y=p2)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p2")
bp21c <- ggplot(a_22[[2]],aes(x=gr,y=p3)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p3")
bp21d <- ggplot(a_22[[2]],aes(x=gr,y=p4)) + geom_boxplot(fill="slateblue", alpha=0.2)+ labs(x="n2, p4")

grid.arrange(bp11a,bp21a,bp11b,bp21b,bp11c,bp21c,bp11d,bp21d,nrow=4,ncol=2)

bp11a <- ggplot(a_13[[2]],aes(x=gr,y=p1)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p1")
bp11b <- ggplot(a_13[[2]],aes(x=gr,y=p2)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p2")
bp11c <- ggplot(a_13[[2]],aes(x=gr,y=p3)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p3")
bp11d <- ggplot(a_13[[2]],aes(x=gr,y=p4)) + geom_boxplot(fill="slateblue", alpha=0.2) + labs(x="n1, p4")


grid.arrange(bp11a,bp11b,bp11c,bp11d,nrow=2,ncol=2)

# Oszacowania parametrów wbudowanymi funkcjami

stable_fit_init(pars1)
stable_fit_init(pars2)
stable_fit_init(pars3)

stable_fit_koutrouvelis(pars1)
stable_fit_koutrouvelis(pars2)
stable_fit_koutrouvelis(pars3)

stable_fit_mle(pars1)
stable_fit_mle(pars2)
stable_fit_mle(pars3)

stable_fit_mle2d(pars1)
stable_fit_mle2d(pars2)
stable_fit_mle2d(pars3)

###################################
###################################
##### ZADANIE 2
###################################
###################################

# WCZYTUJEMY BIBLIOTEKI

library(libstableR)
library(ggplot2)
library(gridExtra)
library(grid)
library(xtable)

###################################

kwant<-function(alpha,est_param,proba,l){
  wyn<-matrix(0,nrow=0,ncol=7)
  for (i in 1:l){
    set.seed(i)
    x1<-t(est_param[i,2:5])
    x2<-t(est_param[i+l,2:5])
    x3<-t(est_param[i+2*l,2:5])
    x4<-t(est_param[i+3*l,2:5])
    proba0<-proba[,i]
    wyn<-rbind(wyn,c(stable_q(1-alpha,x1),stable_q(1-alpha,x2),
      stable_q(1-alpha,x3),stable_q(1-alpha,x4),
      quantile(proba0,probs=1-alpha,t=1,names=FALSE),
      quantile(proba0,probs=1-alpha,t=6,names = FALSE),
      quantile(proba0,probs=1-alpha,t=8,names = FALSE)))
  }
  return(wyn)}

###################################
numb1 = 20

a_11_new <- gd(n=n1, param=pars1, l=numb1)
a_12_new <- gd(n=n1, param=pars2, l=numb1)
a_13_new <- gd(n=n1, param=pars3, l=numb1)

###################################

# Oszacowujemy kwantyle rzędu 0.95

kw1 <- kwant(alpha=0.05,est_param=a_11_new[[2]],proba=a_11_new[[1]],l=numb1)
kw1_minus <- stable_q(1-0.05,pars1)
kw1 <- abs(apply(kw1,2,median)-rep(kw1_minus,7))
kw1a <- t(kw1)


kw2 <- kwant(alpha=0.05,est_param=a_12_new[[2]],proba=a_12_new[[1]],l=numb1)
kw2_minus <- stable_q(1-0.05,pars2)
kw2 <- abs(apply(kw2,2,median)-rep(kw2_minus,7))
kw2a <- t(kw2)

kw3 <- kwant(alpha=0.05,est_param=a_13_new[[2]],proba=a_13_new[[1]],l=numb1)
kw3_minus <- stable_q(1-0.05,pars3)
kw3 <- abs(apply(kw3,2,median)-rep(kw3_minus,7))
kw3a <- t(kw3)

kw_all <- rbind(kw1a,kw2a,kw3a)
kw_all <- xtable(data.frame(kw_all),
                 digits=3,
                caption = "Bezwzględny błąd oszacowania kwantyli rzędu 0.95")

rownames(kw_all) <- c("(n1, p1, 0.95)","(n1, p2, 0.95)","(n1, p3, 0.95)")
names(kw_all) <- c("1P","2P","3P","4P","1NP","2NP","3NP")
print(kw_all, type = "latex", table.placement = "H")

###################################

# Oszacowujemy kwantyle rzędu 0.99

kw1aa <- kwant(alpha=0.01,est_param=a_11_new[[2]],proba=a_11_new[[1]],l=numb1)
kw1_minusa <- stable_q(1-0.01,pars1)
kw1aa <- abs(apply(kw1aa,2,median)-rep(kw1_minusa,7))
kw1b <- t(kw1)

kw2aa <- kwant(alpha=0.01,est_param=a_12_new[[2]],proba=a_12_new[[1]],l=numb1)
kw2_minusa <- stable_q(1-0.01,pars2)
kw2aa <- abs(apply(kw2aa,2,median)-rep(kw2_minusa,7))
kw2b <- t(kw2)

kw3aa <- kwant(alpha=0.01,est_param=a_13_new[[2]],proba=a_13_new[[1]],l=numb1)
kw3_minusa <- stable_q(1-0.01,pars3)
kw3aa <- abs(apply(kw3aa,2,median)-rep(kw3_minusa,7))
kw3b <- t(kw3)

kw_all <- rbind(kw1b,kw2b,kw3b)
kw_all <- xtable(data.frame(kw_all),
                 digits=3,
                caption = "Bezwzględny błąd oszacowania kwantyli rzędu 0.99")

rownames(kw_all) <- c("(n1, p1, 0.99)","(n1, p2, 0.99)","(n1, p3, 0.99)")
names(kw_all) <- c("1P","2P","3P","4P","1NP","2NP","3NP")
print(kw_all, type = "latex", table.placement = "H")

# Rysujemy boxploty zadanych kwantyli rzedu 0.95 
par(mfrow=c(1,3))

bb1 <-boxplot(kw1, col = "#D70A53")
bb2 <- boxplot(kw2, col = "#A9203E")
bb3 <- boxplot(kw3, col = "#EF3038")


# Rysujemy boxploty kwantyli rzedu 0.99

par(mfrow=c(1,3))
ba1 <- boxplot(kw1aa, col = "#CC00CC")
ba2 <- boxplot(kw2aa, col = "#9955BB")
ba3 <- boxplot(kw3aa, col = "#FF1493")

###################################
###################################
#### ZADANIE 3
###################################
###################################

#WCZYTUJEMY BIBLIOTEKI

library(libstableR)
library(ggplot2)
library(gridExtra)
library(grid)
library(xtable)
library(quantmod)
library(MASS)

# VALUE AT RISK
VaR <- function(alpha, z, r, a, P_0){
  if(r == 1){
    p <- fitdistr(z,densfun = 'normal')$estimate
    v <- qnorm(alpha, p[[1]], p[[2]])
    }
  if(r==2){
    p <- stable_fit_init(z)
    v <- stable_q(alpha, pars = p)
    }
  if(r==3){
    v <- quantile(z, probs = alpha, type = 8)
    }
  w <- a * P_0 * (1-exp(v))
  return(w)
}

f_p <- function(z, r, a, P_0){
  val <- function(alpha){
    return(VaR(alpha, z, r, a, P_0))
    }
  return(val)
  }

###################################

# EXPECTED SHORTFALL
ES <- function(z, r, a, P_0, u){
  integral <- as.numeric(integrate(f_p(z, r, a, P_0),lower = 0, upper = u)[1])
  ES <- integral/u
  return(ES)
}


q_est1 <- function(x){
  p <- fitdistr(x, densfun = 'normal')$estimate
  v <- qnorm(0.05, p[[1]], p[[2]])
  return(v)
  }

q_est2 <- function(x){
  p <- stable_fit_init(x)
  v <- stable_q(0.05, pars = p)
  return(v)
  }

q_est3 <- function(x){
  v <- quantile(x, probs = 0.05)[[1]]
  return(v)
  }

B = 300
boot_n <- function(x, estymator=q_est, B=300, ...){
  n <- length(x)
  rep.est <- numeric(B)
  for(b in 1:B){
    x.B <- sample(x, n, replace = TRUE)
    rep.est[b] <- estymator(x.B, ...)
    }
  return(rep.est)
}

###################################

# BMW
getSymbols(Symbols = "BMW.DE",
           src = "yahoo",
           from = Sys.Date() - 365, to = Sys.Date())

# WYZNACZAMY LOGARYTMICZNE STOPY ZWROTU
r.BMW <- log(BMW.DE$BMW.DE.Open / lag(BMW.DE$BMW.DE.Open,k=1))
r.BMW <- na.omit(r.BMW)

# WYKRES CEN OTWARCIA
BMW_p1 <- ggplot(data = BMW.DE, aes(x = index(BMW.DE), y = BMW.DE.Open)) + geom_line(colour = "#21ABCD") + labs(x = "BMW",y = NULL)

# HISTOGRAM LOGARYTMICZNYCH STÓP ZWORTU
BMW_p2 <- ggplot(data = data.frame(czas = index(r.BMW), r.BMW), aes(x = BMW.DE.Open)) + geom_histogram(fill = "#F4C2C2") + labs(x = "BMW", y = NULL)

# Wykres cen akcji BMW w czasie oraz histogram logarytmicznych stóp zwrotu
grid.arrange(BMW_p1, BMW_p2)

p.BMW <- BMW.DE$BMW.DE.Open[[1]]
money = 10**4
n.BMW <- money/p.BMW

# Value at Risk dla BMW
var1.BMW <- VaR(alpha=0.05, z=r.BMW, r=1, a=n.BMW, P_0=p.BMW)
var2.BMW <- VaR(alpha=0.05, z=r.BMW, r=2, a=n.BMW, P_0=p.BMW)
var3.BMW <- VaR(alpha=0.05, z=r.BMW, r=3, a=n.BMW, P_0=p.BMW)

# Expected Shortfall dla BMW
es1.BMW <- ES(z = r.BMW, r = 1, a = n.BMW, P_0 = p.BMW, u = 0.05)
es2.BMW <- ES(z = r.BMW, r = 2, a = n.BMW, P_0 = p.BMW, u = 0.05)
es3.BMW <- ES(z = r.BMW, r = 3, a = n.BMW, P_0 = p.BMW, u = 0.05)

alpha.BMW <- stable_fit_init(r.BMW)
alpha.BMW <- stable_fit_koutrouvelis(r.BMW, alpha.BMW)[1]

tab.var.es_BMW <- data.frame(VaR1 = c(var1.BMW),
                        VaR2 = c(var2.BMW),
                        VaR3 = c(var3.BMW),
                        ES1 = c(es1.BMW),
                        ES2 = c(es2.BMW),
                        ES3 = c(es3.BMW),
                        Alpha = c(alpha.BMW),
                        Value = c(p.BMW * n.BMW)
)
tab.var.es_BMW <- xtable(tab.var.es_BMW, caption = "Oszacowania punktowe VaR oraz ES dla akcji BMW")

rownames(tab.var.es_BMW) <- c("BMW")
colnames(tab.var.es_BMW) <- c("VaR (Z1)", "VaR (Z2)", "VaR (Z3)", "ES (Z1)", "ES (Z2)", "ES(Z3)", "Alfa", "Wartość")
print(tab.var.es_BMW, type = "latex", table.placement = "H")

p_BMW <- c()
for (i in (1:length(r.BMW))){
  p_BMW[i] <- pbinom(i-1, size = length(r.BMW), prob = 0.05)}

i_BMW <- max(which(p_BMW <= 0.05/2))
j_BMW <- min(which(1-p_BMW <= 0.05/2))
# i=6, j=21

# Dokładne przedziały ufności dla BMW
U1 <- n.BMW * p.BMW * (1-exp(sort(as.numeric(r.BMW))[i_BMW]))
L1 <- n.BMW * p.BMW * (1-exp(sort(as.numeric(r.BMW))[j_BMW]))
R1 <- U1 - L1

# Przedziały ufności dla BMW
CI_11 <- boot_n(r.BMW, q_est1)
L11 <- n.BMW*p.BMW*(1-exp(sort(CI_11)[floor(B*(1-0.05/2))]))
U11 <- n.BMW*p.BMW*(1-exp(sort(CI_11)[floor(B*(0.05/2))]))
R11 <- U11 - L11

CI_21 <- boot_n(r.BMW,q_est2)
L21 <- n.BMW*p.BMW*(1-exp(sort(CI_21)[floor(B*(1-0.05/2))]))
U21 <- n.BMW*p.BMW*(1-exp(sort(CI_21)[floor(B*(0.05/2))]))
R21 <- U21 - L21

CI_31 <- boot_n(r.BMW,q_est3)
L31 <- n.BMW*p.BMW*(1-exp(sort(CI_31)[floor(B*(1-0.05/2))]))
U31 <- n.BMW*p.BMW*(1-exp(sort(CI_31)[floor(B*(0.05/2))]))
R31 <- U31 - L31

# tabela z dolnymi i górnymi końcami przedziałów ufności
tab.CI <- data.frame(LBMW = c(L1,L11,L21,L31),
                     UBMW = c(U1,U11,U21,U31),
                     R = c(R1, R11, R21, R31)
                     )

tab.CI <- xtable(tab.CI,
                caption = paste("Przedziały ufności BMW VaR = ",
                round(var1.BMW, 2)
                )
)

rownames(tab.CI) <- c("Dokładne (Z3)","Bootstrap (Z1)","Bootstrap (Z2)","Bootstrap (Z3)")
colnames(tab.CI) <- c("dolny BMW","górny BMW", "różnica")
print(tab.CI, type = "latex", table.placement = "H")

###################################

# APPLE
getSymbols(Symbols = "AAPL",
           src = "yahoo",
           from = Sys.Date() - 365, to = Sys.Date())

# WYZNACZAMY LOGARYTMICZNE STOPY ZWROTU
r.AAPL <- log(AAPL$AAPL.Open / lag(AAPL$AAPL.Open,k=1))
r.AAPL <- na.omit(r.AAPL)

# WYKRES CEN OTWARCIA
AAPL_p1 <- ggplot(data = AAPL, aes(x = index(AAPL), y = AAPL.Open)) + geom_line(colour = "#8A2BE2") + labs(x = "APPLE",y = NULL)


# HISTOGRAM LOGARYTMICZNYCH STÓP ZWORTU
AAPL_p2 <- ggplot(data = data.frame(czas = index(r.AAPL), r.AAPL), aes(x = AAPL.Open)) + geom_histogram(fill = "#DE5D83") + labs(x = "APPLE", y = NULL)


# Wykres cen akcji APPLE w czasie oraz histogram logarytmicznych stóp zwrotu"
grid.arrange(AAPL_p1, AAPL_p2)

p.AAPL <- AAPL$AAPL.Open[[1]]
money = 10**4
n.AAPL <- money/p.AAPL

# Value at Risk dla BMW
var1.AAPL <- VaR(alpha=0.05, z=r.AAPL, r=1, a=n.AAPL, P_0=p.AAPL)
var2.AAPL <- VaR(alpha=0.05, z=r.AAPL, r=2, a=n.AAPL, P_0=p.AAPL)
var3.AAPL <- VaR(alpha=0.05, z=r.AAPL, r=3, a=n.AAPL, P_0=p.AAPL)

# Expected Shortfall dla BMW
es1.AAPL <- ES(z = r.AAPL, r = 1, a = n.AAPL, P_0 = p.AAPL, u = 0.05)
es2.AAPL <- ES(z = r.AAPL, r = 2, a = n.AAPL, P_0 = p.AAPL, u = 0.05)
es3.AAPL <- ES(z = r.AAPL, r = 3, a = n.AAPL, P_0 = p.AAPL, u = 0.05)

alpha.AAPL <- stable_fit_init(r.AAPL)
alpha.AAPL <- stable_fit_koutrouvelis(r.AAPL, alpha.AAPL)[1]

tab.var.es_AAPL <- data.frame(VaR1 = c(var1.AAPL),
                        VaR2 = c(var2.AAPL),
                        VaR3 = c(var3.AAPL),
                        ES1 = c(es1.AAPL),
                        ES2 = c(es2.AAPL),
                        ES3 = c(es3.AAPL),
                        Alpha = c(alpha.AAPL),
                        Value = c(p.AAPL * n.AAPL)
)
tab.var.es_AAPL <- xtable(tab.var.es_AAPL, caption = "Oszacowania punktowe VaR oraz ES dla akcji APPLE")

rownames(tab.var.es_AAPL) <- c("AAPL")
colnames(tab.var.es_AAPL) <- c("VaR (Z1)", "VaR (Z2)", "VaR (Z3)", "ES (Z1)", "ES (Z2)", "ES(Z3)", "Alfa", "Wartość")
print(tab.var.es_AAPL, type = "latex", table.placement = "H")

p_AAPL <- c()
for (i in (1:length(r.AAPL))){
  p_AAPL[i] <- pbinom(i-1, size = length(r.AAPL), prob = 0.05)}

i_AAPL <- max(which(p_AAPL <= 0.05/2))
j_AAPL <- min(which(1-p_AAPL <= 0.05/2))
# i=6, j=21

# Dokładne przedziały ufności dla APPLE
U1_AAPL <- n.AAPL * p.AAPL * (1-exp(sort(as.numeric(r.AAPL))[i_AAPL]))
L1_AAPL <- n.AAPL * p.AAPL * (1-exp(sort(as.numeric(r.AAPL))[j_AAPL]))
R1A <- U1_AAPL - L1_AAPL

# Przedziały ufności dla APPLE
CI_11_AAPL <- boot_n(r.AAPL, q_est1)
L11_AAPL <- n.AAPL*p.AAPL*(1-exp(sort(CI_11_AAPL)[floor(B*(1-0.05/2))]))
U11_AAPL <- n.AAPL*p.AAPL*(1-exp(sort(CI_11_AAPL)[floor(B*(0.05/2))]))
R1A2 <- U11_AAPL - L11_AAPL

CI_21_AAPL <- boot_n(r.AAPL, q_est2)
L21_AAPL <- n.AAPL*p.AAPL*(1-exp(sort(CI_21_AAPL)[floor(B*(1-0.05/2))]))
U21_AAPL <- n.AAPL*p.AAPL*(1-exp(sort(CI_21_AAPL)[floor(B*(0.05/2))]))
R1A3 <- U21_AAPL - L21_AAPL

CI_31_AAPL <- boot_n(r.AAPL, q_est3)
L31_AAPL <- n.AAPL*p.AAPL*(1-exp(sort(CI_31_AAPL)[floor(B*(1-0.05/2))]))
U31_AAPL <- n.AAPL*p.AAPL*(1-exp(sort(CI_31_AAPL)[floor(B*(0.05/2))]))
R1A4 <- U31_AAPL - L31_AAPL

# tabela z dolnymi i górnymi końcami przedziałów ufności
tab.CI_AAPL <- data.frame(LAAPL = c(L1_AAPL,L11_AAPL,L21_AAPL,L31_AAPL),
                     UAAPL = c(U1_AAPL,U11_AAPL,U21_AAPL,U31_AAPL),
                     RAAPL = c(R1A, R1A2, R1A3, R1A4)
                     )

tab.CI_AAPL <- xtable(tab.CI_AAPL,
                caption = paste("Przedziały ufności APPLE VaR = ",
                round(var1.AAPL, 2)
                )
)

rownames(tab.CI_AAPL) <- c("Dokładne (Z3)","Bootstrap (Z1)","Bootstrap (Z2)","Bootstrap (Z3)")
colnames(tab.CI_AAPL) <- c("dolny APPLE","górny APPLE", "różnica")
print(tab.CI_AAPL, type = "latex", table.placement = "H")

###################################

# TESLA 
getSymbols(Symbols = "TSLA",
           src = "yahoo",
           from = Sys.Date() - 365, to = Sys.Date())

# WYZNACZAMY LOGARYTMICZNE STOPY ZWROTU
r.TSLA <- log(TSLA$TSLA.Open / lag(TSLA$TSLA.Open,k=1))
r.TSLA <- na.omit(r.TSLA)

# WYKRES CEN OTWARCIA
TSLA_p1 <- ggplot(data = TSLA, aes(x = index(TSLA), y = TSLA.Open)) + geom_line(colour = "#702963") + labs(x = "TESLA",y = NULL)

# HISTOGRAM LOGARYTMICZNYCH STÓP ZWORTU
TSLA_p2 <- ggplot(data = data.frame(czas = index(r.TSLA), r.TSLA), aes(x = TSLA.Open)) + geom_histogram(fill = "#F4BBFF") + labs(x = "TESLA", y = NULL)


# Wykres cen akcji TESLA w czasie oraz histogram logarytmicznych stóp zwrotu
grid.arrange(TSLA_p1, TSLA_p2)

p.TSLA <- TSLA$TSLA.Open[[1]]
money = 10**4
n.TSLA <- money/p.TSLA

# Value at Risk dla TESLA
var1.TSLA <- VaR(alpha=0.05, z=r.TSLA, r=1, a=n.TSLA, P_0=p.TSLA)
var2.TSLA <- VaR(alpha=0.05, z=r.TSLA, r=2, a=n.TSLA, P_0=p.TSLA)
var3.TSLA <- VaR(alpha=0.05, z=r.TSLA, r=3, a=n.TSLA, P_0=p.TSLA)

# Expected Shortfall dla TESLA
es1.TSLA <- ES(z = r.TSLA, r = 1, a = n.TSLA, P_0 = p.TSLA, u = 0.05)
es2.TSLA <- ES(z = r.TSLA, r = 2, a = n.TSLA, P_0 = p.TSLA, u = 0.05)
es3.TSLA <- ES(z = r.TSLA, r = 3, a = n.TSLA, P_0 = p.TSLA, u = 0.05)

alpha.TSLA <- stable_fit_init(r.TSLA)
alpha.TSLA <- stable_fit_koutrouvelis(r.TSLA, alpha.TSLA)[1]

tab.var.es_TSLA <- data.frame(VaR1 = c(var1.TSLA),
                        VaR2 = c(var2.TSLA),
                        VaR3 = c(var3.TSLA),
                        ES1 = c(es1.TSLA),
                        ES2 = c(es2.TSLA),
                        ES3 = c(es3.TSLA),
                        Alpha = c(alpha.TSLA),
                        Value = c(p.TSLA * n.TSLA)
)
tab.var.es_TSLA <- xtable(tab.var.es_TSLA, caption = "Oszacowania punktowe VaR oraz ES dla akcji TESLA")

rownames(tab.var.es_TSLA) <- c("AAPL")
colnames(tab.var.es_TSLA) <- c("VaR (Z1)", "VaR (Z2)", "VaR (Z3)", "ES (Z1)", "ES (Z2)", "ES(Z3)", "Alfa", "Wartość")
print(tab.var.es_TSLA, type = "latex", table.placement = "H")


p_TSLA <- c()
for (i in (1:length(r.TSLA))){
  p_TSLA[i] <- pbinom(i-1, size = length(r.TSLA), prob = 0.05)}

i_TSLA <- max(which(p_TSLA <= 0.05/2))
j_TSLA <- min(which(1-p_TSLA <= 0.05/2))
# i=6, j=21

# Dokładne przedziały ufności dla BMW
U1_TSLA <- n.TSLA * p.TSLA * (1-exp(sort(as.numeric(r.TSLA))[i_TSLA]))
L1_TSLA <- n.TSLA * p.TSLA * (1-exp(sort(as.numeric(r.TSLA))[j_TSLA]))
T1 <- U1_TSLA - L1_TSLA

# Przedziały ufności dla BMW
CI_11_TSLA <- boot_n(r.TSLA, q_est1)
L11_TSLA <- n.TSLA*p.TSLA*(1-exp(sort(CI_11_TSLA)[floor(B*(1-0.05/2))]))
U11_TSLA <- n.TSLA*p.TSLA*(1-exp(sort(CI_11_TSLA)[floor(B*(0.05/2))]))
T1A <- U11_TSLA - L11_TSLA

CI_21_TSLA <- boot_n(r.TSLA, q_est2)
L21_TSLA <- n.TSLA*p.TSLA*(1-exp(sort(CI_21_TSLA)[floor(B*(1-0.05/2))]))
U21_TSLA <- n.TSLA*p.TSLA*(1-exp(sort(CI_21_TSLA)[floor(B*(0.05/2))]))
T1AA <- U21_TSLA - L21_TSLA

CI_31_TSLA <- boot_n(r.TSLA, q_est3)
L31_TSLA <- n.AAPL*p.TSLA*(1-exp(sort(CI_31_TSLA)[floor(B*(1-0.05/2))]))
U31_TSLA <- n.AAPL*p.TSLA*(1-exp(sort(CI_31_TSLA)[floor(B*(0.05/2))]))
T1AAA <- U31_TSLA - L31_TSLA

# tabela z dolnymi i górnymi końcami przedziałów ufności
tab.CI_TSLA <- data.frame(LTSLA = c(L1_TSLA,L11_TSLA,L21_TSLA,L31_TSLA),
                     UTSLA = c(U1_TSLA,U11_TSLA,U21_TSLA,U31_TSLA),
                     TSLA = c(T1, T1A, T1AA, T1AAA)
                     )

tab.CI_TSLA <- xtable(tab.CI_TSLA,
                caption = paste("Przedziały ufności TESLA VaR = ",
                round(var1.TSLA, 2)
                )
)

rownames(tab.CI_TSLA) <- c("Dokładne (Z3)","Bootstrap (Z1)","Bootstrap (Z2)","Bootstrap (Z3)")
colnames(tab.CI_TSLA) <- c("dolny TESLA","górny TESLA", "różnica")
print(tab.CI_TSLA, type = "latex", table.placement = "H")

###################################

