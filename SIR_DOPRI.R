# Parametry i wielkości początkowe
beta <- 0.5
gamma <- 0.1
N <- 1000
Time <- 100


# Konieczne jest załadowanie pakietu deSolve funkcje do rozwiązywania układu równań rózniczkowych  
# (General Solver for Ordinary Differential Equations)
library(deSolve)

# Definicja układu równań  
sir <- function(T, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta/N * S * I
    dI <-  beta/N * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


# Ustawienie parametrów w postaci dla funkcji ODE 
init       <- c(S = N - 1 , I = 1 , R = 0)
parameters <- c(beta, gamma, N)
T      <- seq(1, Time, by = 1)



# Rozwiązanie z wykorzystaniem funkcji ode (General Solver for Ordinary Differential Equations)
SIR2 <- ode(y = init, times = T, func = sir, parms = parameters, method = "ode45")
SIR2 <- as.data.frame(out)
names(SIR2)[1] <- "T"
SIR2_long <- gather(SIR2, key = "key", value = "value", -T )
SIR2_long$Method <- "DOPRI"


# Wykres lini SIR (DOPRI + Euler)
library(ggplot2)
ggplot(SIR2_long, aes(x = T, y = value, col = key, linetype = Method))+
  geom_line()+
  geom_line(data = SIR_long)+
  xlab( 'Czas [ np . dni ]')+
  ylab ( ' Liczba przypadkow ( osob ) ')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())

# Wykres lini SIR - skala logarytmiczna ((DOPRI + EULER))
ggplot(SIR2_long, aes(x = T, y = log(value), col = key, linetype = Method))+
  geom_line()+
  geom_line(data = SIR_long)+
  xlab( 'Czas [ np . dni ]')+
  ylab ( 'log(Liczba przypadkow ( osob ))')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())

