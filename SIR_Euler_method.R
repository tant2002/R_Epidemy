`%+%` <- function(a, b) paste(a, b, sep="")


# Parametry , wektory SIR i wartosci poczatkowe
beta <- 0.5
gamma <- 0.1
N <- 1000
Time <- 100

S <- rep(0, Time)
I <- rep(0, Time)
R <- rep(0, Time)


I[1] <- 1
S[1] <- N - I[1]
R[1] <- 0 

# Ewolucja czasowa

for (t in seq(1, Time-1)) {
  S[t+1] <- S[t] - beta * I[t]* S[t]/ N
  I[t+1] <- I[t] + beta * I[t]* S[t]/ N - gamma * I[t]
  R[t+1] <- R[t] + gamma *I[t]
}



library(tidyr)
SIR <- data.frame(T = seq(1, Time), S, I ,R )
SIR_long <- gather(SIR, key = 'key', value = 'value', -T )
SIR_long$Method <- 'Euler'

# Wykres lini SIR 
library(ggplot2)
ggplot(SIR_long, aes(x = T, y = value, col = key))+
  geom_line()+
  xlab( 'Czas [ np . dni ]')+
  ylab ( ' Liczba przypadkow ( osob ) ')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())

# Wykres lini SIR - skala logarytmiczna
ggplot(SIR_long, aes(x = T, y = log(value), col = key))+
  geom_line()+
  xlab( 'Czas [ np . dni ]')+
  ylab ( 'log(Liczba przypadkow ( osob ))')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())


# Prosta regresji dla pierwszych 10 punktow
T0 <- 10
B <- lm(log(I)~T, data = SIR%>%filter(T <= T0))
B$coefficients

# Wykres lini I z zaznaczoną regresją dla x <= TO 

ggplot(SIR_long%>%filter(key == 'I'), aes(x = T, y = log(value)))+
  geom_point(data = SIR_long%>%filter(key == 'I', T <= T0), aes(fill="Punkty do regresji"))+
  geom_line(aes(col = key))+
  geom_abline(aes(intercept = B$coefficients[1], slope = B$coefficients[2],
                  linetype="Prosta regresji y="%+% round(B$coefficients[2],3) %+% "x-"%+% round(B$coefficients[1],3)), colour = 'black')+
  labs(x = 'Czas [ np . dni ]',
       y = 'log(Liczba przypadkow ( osob ))')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank(), legend.position = c(0.85, 0.7))


