##### WCZYTUJEMY BIBLIOTEKI #####

library(knitr)
library(xtable) 
library(ggplot2)
library(gridExtra)

##############################
##### ZADANIE 1 #####
##############################

# Stała intensywność A1
A1 <- function(t0, lambda, mu, sigma){
  ti <- c()
  while(sum(ti) <= t0){
  next_ti <- rexp(1, lambda)
  ti <- c(ti, next_ti)
  }
  ti <- head(ti, length(ti) - 1)
  punkty <- cumsum(ti)
  znaki <- cumsum(rlnorm(length(punkty), mu, sigma))
  dane_A1 <- cbind(punkty = c(0, punkty), znaki = c(0, znaki))
  plot_A1 <- ggplot(data = as.data.frame(dane_A1), aes(x = punkty, y = znaki)) + 
    geom_step(data = as.data.frame(znaki), aes(y = znaki), col = "#9966CC") + 
    xlab("skumulowane czasy wektora ti") + 
    ylab("skumulowane losowo wygenerowane liczby z rozkładu LN") +
    ggtitle("Wykres procesu znakowanego o stałej intensywności A1") 
  return(plot_A1) 
}

wykres_A1_30 <- A1(30, 2, 2, 2)
# wykres_A1_30

wykres_A1_50 <- A1(50, 2, 2, 2)
# wykres_A1_50

wykres_A1_1000 <- A1(1000, 2, 2, 2)
# wykres_A1_1000

wykres_A1_30N <- A1(30, 1, 2, 1)
# wykres_A1_30N

# Stała intensywność A2
A2 <- function(t0,lambda,mu,sigma){
  N <- rpois(1, lambda*t0)
  punkty <- runif(N, min = 0, max = t0)
  punkty <- sort(punkty)
  znaki <- cumsum(rlnorm(N, mu, sigma))
  dane_A2 <- cbind(punkty = c(0, punkty), znaki = c(0, znaki))
  plot_A2 <- ggplot(data = as.data.frame(dane_A2), aes(x = punkty, y = znaki)) + 
    geom_step(data = as.data.frame(znaki), aes(y = znaki), col = "#FF2052") +
    xlab("zbiór N losowych punktów z rozkładu jednostajnego") +
    ylab("losowo wygenerowane N punktów z rozkładu LN") +
    ggtitle("Wykres procesu znakowanego o stałej intensywności A2")
  return(plot_A2)
}

wykres_A2_30 <- A2(30, 2, 2, 2)
# wykres_A2_30

wykres_A2_50 <- A2(50, 2, 2, 2)
# wykres_A2_50

wykres_A2_1000 <- A2(1000, 2, 2, 2)
#wykres_A2_1000

wykres_A2_30N <- A2(30, 1, 2, 1)
# wykres_A2_30N

# Zmienna intensywność B1
LAMBDA <- function(t, alpha, beta){
  alpha * t^(beta)
}

LAMBDAinverse <- function(t, alpha, beta){
  (t/alpha)^(1/beta)
}

B1 <- function(t0, alpha1, beta1, mu, sigma){
  lambda_max_B1 <- LAMBDA(t0, alpha = alpha1, beta = beta1)
  N <- rpois(1, lambda_max_B1)
  punkty_j <- runif(N, min = 0, max = t0)
  punkty_j <- sort(punkty_j)
  punkty <- sapply(punkty_j, LAMBDAinverse, alpha = alpha1, beta = beta1)
  znaki <- cumsum(rlnorm(N, mu, sigma))
  dane_B1 <- cbind(punkty = c(0, punkty), znaki = c(0, znaki))
  plot_B1 <- ggplot(data = as.data.frame(dane_B1), aes(x = punkty, y = znaki)) + 
    geom_step(data = as.data.frame(znaki), aes(y = znaki), col = "#006B3C") +
    xlab("zbiór N losowych punktów z rozkładu jednostajnego") +
    ylab("losowo wygenerowane N punktów z rozkładu LN") +
    ggtitle("Wykres procesu znakowanego o zmiennej intensywności B1")
  return(plot_B1)
} 
  
wykres_B1_30 <- B1(30, 1, 3, 0, 1)
# wykres_B1_30

wykres_B1_50 <- B1(50, 1, 3, 0, 1)
# wykres_B1_50

wykres_B1_1000 <- B1(1000, 1, 3, 0, 1)
# wykres_B1_1000
 
# Zmienna intensywność B2 
lambda <- function(t, alpha, beta){
  alpha * beta * (t^(beta - 1))
}  

B2 <- function(t0, alpha, beta, mu, sigma){
  lambda_max_B2 <- lambda(t0, alpha, beta)
  N <- rpois(1, lambda_max_B2 * t0)
  punkty <- runif(N, min = 0, max = t0)
  punkty <- sort(punkty)
  u <- runif(N)
  t_delete <- c()
  for (i in 1:N){
    if(u[i] >= lambda(punkty[i], alpha, beta)/lambda_max_B2){
      t_delete <- c(t_delete, punkty[i])
    }
  }
    punkty <- punkty[! punkty %in% t_delete]
    k <- length(punkty)
    znaki <- cumsum(rlnorm(k, mu, sigma))
    dane_B2 <- cbind(punkty = c(0, punkty), znaki = c(0, znaki))
    plot_B2 <- ggplot(data = as.data.frame(dane_B2), aes(x = punkty, y = znaki)) + 
      geom_step(data = as.data.frame(znaki), aes(y = znaki), col = "#ED872D") + 
      xlab("zbiór N losowych punktów z rozkładu jednostajnego") +
      ylab("losowo wygenerowane N punktów z rozkładu LN") +
      ggtitle("Wykres procesu znakowanego o zmiennej intensywności B2")
    return(plot_B2)
}
  
wykres_B2_30 <- B2(30, 1, 3, 0, 1)
# wykres_B2_30

wykres_B2_50 <- B2(50, 1, 3, 0, 1)
# wykres_B2_50

wykres_B2_1000 <- B2(1000, 1, 3, 0, 1)
# wykres_B2_1000

# Porównanie funkcji A1, A2, B1, B2 dla parametrów 
t0 <- 30
lambda_proba <- 2
alpha_proba <- 1
beta_proba <- 1
mu_proba <- 0
sigma_proba <- 1

A1_proba <- A1(t0 = t0, lambda = lambda_proba, mu = mu_proba, sigma = sigma_proba)
A2_proba <- A2(t0 = t0, lambda = lambda_proba, mu = mu_proba, sigma = sigma_proba)
B1_proba <- B1(t0 = t0, alpha1 = alpha_proba, beta1 = beta_proba, mu = mu_proba, sigma = sigma_proba)
B2_proba <- B2(t0 = t0, alpha = alpha_proba, beta = beta_proba, mu = mu_proba, sigma = sigma_proba)

# A1_proba
# A2_proba
# B1_proba
# B2_proba

# Sprawdzamy, która funkcja jest lepsza

system.time(
  for (i in 1:100) {
    A1(30, 2, 2, 2)
  }
)

system.time(
  for (i in 1:5) {
    B1(100, 1, 3, 0, 1)
  }
)

system.time(
  for (i in 1:5) {
    B2(50, 1, 3, 0, 1)
  }
)

system.time(
  for (i in 1:5) {
    B2(100, 1, 3, 0, 1)
  }
)

# Dla każdej z funkcji postępowałam analogicznie. Nie dodawałam nowych funkcji tylko w powyżej zmieniałam parametry
# Dla funkcji B2 i t0 = 50 niestety zabrakło mi RAM (8GM), więc przesłałam ją znajomemu, który ma większą ilość RAM (64GB)
# Tak samo dla funkcji B1 i t0 = 1000 oraz B2 i t0 = 1000

t0 <- 30
lambda_proba <- 2
alpha_proba <- 1
beta_proba <- 1
mu_proba <- 0
sigma_proba <- 1

system.time(
  for (i in 1:100) {
    B2(t0 = t0, alpha = alpha_proba, beta = beta_proba, mu = mu_proba, sigma = sigma_proba)
  }
)

##############################
##### ZADANIE 2 #####
##############################

# Funkcja wyznaczająca zadany przedział ufności
pu <- function(mu, sigma, lambda, alpha, a1 = NULL){
  if (is.null(a1)){a1 <- alpha/2}
  ex <- exp(mu + (sigma^2)/2)
  ex2 <- exp(2 * mu + 2*(sigma^2))
  a <- lambda * ex + qnorm(a1) * sqrt(lambda*ex2)
  b <- lambda * ex + qnorm(1 - (alpha - a1)) * sqrt(lambda * ex2)
  if(a < 0){a <- 0}
  return(c(a, b))
}

# Funkcja wyznaczająca prawdpodobodbieństwo pokrycia
pr_p <- function(mu, sigma, lambda, alpha, M, a1 = NULL){
  if(is.null(a1)){a1 <- alpha/2}
  pu1 <- pu(mu, sigma, lambda, alpha, a1)
  w <- rep(0, M)
  for(i in 1:M){
    N <- rpois(1, lambda)
    S_N <- sum(rlnorm(N, mu, sigma))
    w[i] <- ifelse(pu1[1] < S_N & S_N < pu1[2], 1, 0)
    }
  prawd <- sum(w)/M
  return(prawd)
}

# Testujemy obydwie napisane funkcje
10 * exp(1 + (0.5^2)/2)
pu(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.05, a1 = NULL)
pr_p(mu=1, sigma=0.5, lambda=10, alpha=0.05, M=50, a1=NULL)

# 1. Przedstawić dla kilku wybranych przykładowych zbiorów parametrów uzyskane wartości końców przedziału 
pu(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.05, a1 = NULL)
pu(mu = 2, sigma = 0.5, lambda = 10, alpha = 0.05, a1 = NULL)
pu(mu = 5, sigma = 0.5, lambda = 10, alpha = 0.05, a1 = NULL)

pu(mu = 1, sigma = 0.3, lambda = 10, alpha = 0.05, a1 = NULL)
pu(mu = 1, sigma = 0.01, lambda = 10, alpha = 0.05, a1 = NULL)
pu(mu = 1, sigma = 0.9, lambda = 10, alpha = 0.05, a1 = NULL)

pu(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.05, a1 = NULL)
pu(mu = 1, sigma = 0.5, lambda = 100, alpha = 0.05, a1 = NULL)
pu(mu = 1, sigma = 0.5, lambda = 1000, alpha = 0.05, a1 = NULL)
pu(mu = 1, sigma = 0.5, lambda = 0.01, alpha = 0.05, a1 = NULL)
# skomentować te wyniki!

# 2. Jak zmienia się prawdopodobieństwo pokrycia dla różnych parametrów
pr_p(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 2, sigma = 0.5, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 5, sigma = 0.5, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)

pr_p(mu = 1, sigma = 0.3, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.01, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 1, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)

pr_p(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.5, lambda = 100, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.5, lambda = 1000, alpha = 0.05, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.5, lambda = 0.01, alpha = 0.05, M = 50, a1 = NULL)

pr_p(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.2, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.02, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.3, M = 50, a1 = NULL)
pr_p(mu = 1, sigma = 0.5, lambda = 10, alpha = 0.03, M = 50, a1 = NULL)


# Wyznaczamy zależność prawdopodobieńtwa od wartości M
# W tym zadaniu zmieniałam jedynie wartości parametru M.max, funkcja zostawała ta sama, dlatego tylko jedna jest, a nie dla każdego parametru osobna
M.max <- 10000
mu.pr.m <- 1
sigma.pr.m <- 0.5
lambda.pr.m <- 10
alpha.pr.m <- 0.05

prm <- rep(0, M.max)  
 
for (i in 1:M.max){
prm[i] <- pr_p(mu = mu.pr.m,
                 sigma = sigma.pr.m,
                 lambda = lambda.pr.m,
                 alpha = alpha.pr.m,
                 M = i)
 }

daneprM <- cbind(M=c(1:M.max), pr = c(prm))
plot.pr.m <- ggplot(data = as.data.frame(daneprM), aes(x = M, y = pr))+
geom_line(color = "#E68FAC") +ylab("prawdopodobieństwo")
print(plot.pr.m)

##############################
##### ZADANIE 3 #####
##############################

# estymowane przedziały ufności 
pu_est <- function(X1, alpha, a1 = NULL){
  if(is.null(a1)){a1 <- alpha/2}
  l <- length(X1)
  mu_e <- mean(log(X1))
  sigma_e <- sd(log(X1))
  ex_e <- exp(mu_e + (sigma_e^2)/2)
  ex2_e <- exp(2*mu_e + 2*(sigma_e^2))
  a_e <- l*ex_e + qnorm(a1) * sqrt(l*ex2_e)
  b_e <- l*ex_e + qnorm(1 - (alpha - a1))* sqrt(l*ex2_e)
  return(c(a_e, b_e))
}

# estymaowane prawdopodobieństwo pokrycia
pr_p_est <- function(mu, sigma, lambda, alpha, M, a1 = NULL){
  if(is.null(a1)){a1 <- alpha/2}
  w <- rep(0, M)
  for (i in 1:M){
  while(TRUE){
    N1 <- rpois(1, lambda)
    if (N1 > 1){break}
  }
      X1 <- rlnorm(N1, mu, sigma)
      pu1_e <- pu_est(X1, alpha, a1)
      mu_e <- mean(log(X1))
      sigma_e <- sd(log(X1))
      N1 <- rpois(1, lambda)
      S_N1 <- sum(rlnorm(N1, mu_e, sigma_e))
      w[i] <- ifelse(pu1_e[1] < S_N1 & S_N1 < pu1_e[2], 1, 0)
    }
    prawd <- sum(w)/M
    return(prawd)
    }
  

# wykres zależności prawdopodobieństwa pokrycia od M
M.max <- 1000
mu.pr.m <- 1
sigma.pr.m <- 0.5
lambda.pr.m <- 10
alpha.pr.m <- 0.05

prm2 <- rep(0, M.max)  

for (i in 1:M.max){
prm2[i] <- pr_p_est(mu = mu.pr.m, sigma = sigma.pr.m, lambda = lambda.pr.m, alpha = alpha.pr.m, M = i)
}

daneprM <- cbind(M=c(1:M.max), pr = c(prm2))
plot.pr.m2 <- ggplot(data=as.data.frame(daneprM),aes(x=M,y=pr))+
geom_line(color="blue")
print(plot.pr.m2)

A <- rlnorm(10, 0, 1) 
B <- rlnorm(10, 3, 1)
C <- rlnorm(10, 1/8, 1)
D <- rlnorm(10, 1.5, 1)
E <- rlnorm(10, 0.01, 1)

pu_est(X1 = C, alpha = 0.05, a1 = NULL)
pu_est(X1 = E, alpha = 0.05, a1 = NULL)

pr_p_est(30, 0.0001, 10, alpha = 0.05, M = 100000, a1 = NULL)

# Generujemy wykres

M.max <- 10000
mu.pr.m <- 1
sigma.pr.m <- 0.5
lambda.pr.m <- 10
alpha.pr.m <- 0.05

prm2 <- rep(0, M.max)  
 
for (i in 1:M.max){
prm2[i] <- pr_p_est(mu = mu.pr.m,
                 sigma = sigma.pr.m,
                 lambda = lambda.pr.m,
                 alpha = alpha.pr.m,
                 M = i)
}

daneprM2 <- cbind(M = c(1:M.max), pr = c(prm2))
plot.pr.m2 <- ggplot(data = as.data.frame(daneprM2), aes(x = M, y = pr))+
geom_line(color = "#32CD32") +ylab("prawdopodobieństwo")
print(plot.pr.m2)

##############################
##### ZADANIE 4 #####
##############################

# Skladka_netto

hr <- function(d, s){
  lambda <- 1/mean(s)
  return(exp(-d*lambda)/lambda)
}

# Przedziały ufności 
pu_hr <- function(s, alpha, d){
  es <- mean(s)
  gSn <- es * exp(-d/es)
  gSn_prim <- ( 1 + d/es) * exp(-d/es)
  mes <- es/sqrt(length(s))
  lewy <- gSn - qnorm(1 - alpha/2) * mes * gSn_prim
  prawy <- gSn + qnorm(1 - alpha/2) * mes * gSn_prim
  if (lewy < 0){lewy <- 0}
  return(c(lewy, prawy))
  }

# PRZYPADEK 1 -  d = 1,2,3, (lambda = 1, m = 30)
# PRZYPADEK 2 - lambda = 1,2,3 (d = 1, m = 30)
# PRZYPADEK 3 - m = 30,50,100 (lambda = 1, d = 1)
# próba s zależy od lambda oraz m.
# próba dla lambda = 1, m = 30

s_1_30 <- rexp(n = 100, rate = 1)
pkt_4_d <- sapply(list(1, 1, 1), hr, s = s_1_30)
pu_4_d <- sapply(c(1, 1, 1), pu_hr, s = s_1_30, alpha=0.05)
dane_4_d <- cbind(d = c(1, 2, 3),
                  lambda = c(1, 1, 1),
                  m = c(30, 30, 30),
                  Punktowe = pkt_4_d,
                  Dolne = pu_4_d[1,],
                  Górne = pu_4_d[2,]
                  )
dane_4_d <- data.frame(dane_4_d)
colnames(dane_4_d) <- c("d","$\\lambda$","m","punktowe","dolne","górne")

# Funkcje w zadaniu 4 wykorzystywałam cały czas tą samą i jedynie zmieniałam parametry i później opisy
s_1_30 <- rexp(n=30, rate=1)
pkt_4_d <- sapply(1, hr, s=s_1_30)
pu_4_d <- sapply(1, pu_hr, s=s_1_30, alpha=0.1)
dane_4_d <- cbind(d = 1,
                  lambda = 1,
                  m = 30,
                  Punktowe = pkt_4_d,
                  Dolne = pu_4_d[1,],
                  Górne = pu_4_d[2,]
                  )
dane_4_d <- data.frame(dane_4_d)
colnames(dane_4_d) <- c("d","$\\lambda$","m","punktowe","dolne","górne")

##############################
##### ZADANIE 5 #####
##############################

# prawdopodobieństwo pokrycia
pu_hr_prawd <- function(m, lambda, d, M, alpha1){
  h_est <- exp(-d*lambda)/lambda
  w <- rep(0, M)
  for (i in 1:M){
    s1 <- rexp(m, lambda)
    pu1_e <- pu_hr(s1, alpha1, d)
    w[i] <- ifelse(pu1_e[1] < h_est & h_est < pu1_e[2], 1, 0)
    }  
  prawd <- sum(w)/M
  return(prawd)
}

M.max5 <- 1000
m_5 <- 30
lambda_5 <- 1
d_5 <- 1
alpha_5 <- c(0.01, 0.05, 0.10, 0.20, 0.50, 0.80, 0.99)

wyniki_5 <- sapply(alpha_5, pu_hr_prawd, lambda=lambda_5, d=d_5, m=m_5, M=M.max5)
dane_5 <- cbind(alpha_5, 1 - alpha_5, wyniki_5, abs(wyniki_5 - (1-alpha_5)))
colnames(dane_5)<-c("$\\alpha$","$1-\\alpha$", "prawdopodobieństwo", "różnica bezwzględna")

print(xtable(dane_5,digits = 2,
               caption = "Wyniki dla różnych poziomów ufności.",
               label = "tab:zad5:prawd"),type = "latex", table.placement = "H", include.rownames=FALSE,sanitize.text.function=function(x){x})


M.max <- 800
prm5 <- rep(0,M.max)

for (i in 1:M.max){
prm5[i] <- pu_hr_prawd(m = 30,
                      lambda = 1,
                      d = 1,
                      M = i,
                      alpha1 = 0.05
                      )
}

daneprM5 <- cbind(M=c(1:M.max), pr = c(prm5))
plot.pr.m5 <- ggplot(data=as.data.frame(daneprM5),aes(x=M,y=pr))+
geom_line(color="#0F4D92")
print(plot.pr.m5)

