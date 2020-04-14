`%+%` <- function(a, b) paste(a, b, sep="")

# Parametry , wektory SIR i wartosci poczatkowe
# beta - parametr infekcji, parametr ozdrowieninia
beta <- 0.5
gamma <- 0.1
N <- 1000
Time <- 100



sSIR <- data.frame(N = rep(0, Time*N),
                   T = rep(0, Time*N),
                   S = rep(0, Time*N), 
                   I = rep(0, Time*N), 
                   R = rep(0, Time*N))  

sSIR$I[1] <- 1
sSIR$S[1] <- N - sSIR$I[1]  
sSIR$R[1] <- 0 


# Ewolucja czasowa
u1 <- runif(Time*N ,0, 1) 
u2 <- runif(Time*N ,0, 1)

beta * sSIR$I[1] * sSIR$S[1] / (N^2)
for (t in seq(1, Time)){
  for(n in seq(1, N)){
    i <- (t-1)*N + n  
    if (i < Time*N) {
   
      
  # (S_t ,I_t , R_t) --> (S_t -1 , I_t +1 , R_t ) z prawd . p_1
      sSIR$T[i] <- t
      sSIR$N[i] <- n
      
      if (u1[i] < beta * sSIR$I[i] * sSIR$S[i] / (N^2)) {
       
        sSIR$S[i+1] <- sSIR$S[i] - 1
        sSIR$I[i+1] <- sSIR$I[i] + 1
        }
  # (S_t ,I_t , R_t) --> (S_t ,I_t , R_t ) z prawd . 1- p_1
      else { 
        
        sSIR$S[i+1] <- sSIR$S[i]
        sSIR$I[i+1] <- sSIR$I[i]
        }
  # (S_t ,I_t , R_t) --> (S_t ,I_t -1 , R_t +1) z prawd . p_2

      if(u2[i] < (gamma*sSIR$I[i]/N)) {

        sSIR$I[i+1] <- sSIR$I[i] - 1
        sSIR$R[i+1] <- sSIR$R[i] + 1}
  #(S_t ,I_t , R_t) --> (S_t ,I_t , R_t ) z prawd . p_2
      else{
    #sI(t +1) = sI(t +1); I_t zmienione powyzej
        
        sSIR$R[i+1] <- sSIR$R[i]
      }
    }
  }
}

library(tidyr)
SIR3 <- sSIR[sSIR$N == 1, -1]
SIR3_long <- gather(SIR3, key = 'key', value = 'value', -T)
SIR3_long$Method <- 'Stotastic'


library(ggplot2)
ggplot(SIR3_long, aes(x = T, y = value, col = key, linetype = Method))+
  geom_line()+
  geom_line(data = SIR2_long)+
  geom_line(data = SIR_long)+
  xlab( 'Czas [ np . dni ]')+
  ylab ( ' Liczba przypadkow ( osob ) ')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())

# Wykres lini SIR - skala logarytmiczna ((DOPRI + EULER))
ggplot(SIR3_long, aes(x = T, y = log(value), col = key, linetype = Method))+
  geom_line()+
  geom_line(data = SIR2_long)+
  geom_line(data = SIR_long)+
  xlab( 'Czas [ np . dni ]')+
  ylab ( 'log(Liczba przypadkow ( osob ))')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())

