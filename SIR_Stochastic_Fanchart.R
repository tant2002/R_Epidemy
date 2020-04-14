# Parametry , wektory SIR i wartosci poczatkowe
# beta - parametr infekcji, parametr ozdrowieninia
beta <- 0.5
gamma <- 0.1
N <- 1000 #liczebość populacji
Time <- 100  #ilość okresów 
M <- 1000  #ilość symulacji


sSIR <- data.frame(M = 0,
                   T = 0,
                   S = 0, 
                   I = 0, 
                   R = 0)  





#symulacja M razy 
for (m in seq(1, M)){
  print(m)
  print(Sys.time())
  iSIR <- data.frame(M = rep(m,Time),
                     T = seq(1,Time),
                     S = c(N-1, rep(0, Time-1)), 
                     I = c(1, rep(0, Time-1)), 
                     R = rep(0, Time)) 

  
  # Ewolucja czasowa
  for (t in seq(2, Time)){
    u1 <- runif(N ,0, 1) 
    u2 <- runif(N ,0, 1)
    for(n in seq(1, N)){
      if (n == 1){
        pSIR <- iSIR[t-1,]
        }
      else{
        pSIR <- iSIR[t,]
        }
        # (S_t ,I_t , R_t) --> (S_t -1 , I_t +1 , R_t ) z prawd . p_1
      if (u1[n] < beta * pSIR$I * pSIR$S / (N^2)) {
        iSIR$S[t] <- pSIR$S - 1
        iSIR$I[t] <- pSIR$I + 1
        }
        # (S_t ,I_t , R_t) --> (S_t ,I_t , R_t ) z prawd . 1- p_1
      else { 
        iSIR$S[t] <- pSIR$S
        iSIR$I[t] <- pSIR$I
        }
        # (S_t ,I_t , R_t) --> (S_t ,I_t -1 , R_t +1) z prawd . p_2
      if(u2[n] < (gamma*pSIR$I/N)) {
        iSIR$I[t] <- pSIR$I - 1
        iSIR$R[t] <- pSIR$R + 1
        }
        #(S_t ,I_t , R_t) --> (S_t ,I_t , R_t ) z prawd . p_2
      else{
          #sI(t +1) = sI(t +1); I_t zmienione powyzej
        iSIR$R[t] <- pSIR$R
        }
    }
  }
  sSIR <- rbind(sSIR, iSIR)
}

library(tidyr)
library(dplyr)
SIR3_long <- gather(sSIR, key = 'key', value = 'value', -T, -M)
SIR3_long$Method <- 'Stotastic'


library(ggplot2)
library(ggfan)
iSIR <- sSIR 
iSIR$M <- factor(iSIR$M)
ggplot(iSIR, aes(x = T, y = S))+
  #geom_line()+ 
  geom_interval()+
  xlab( 'Czas [ np . dni ]')+
  ylab ( ' Liczba przypadkow ( osob ) ')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())


ggplot(iSIR, aes(x = T, y = S))+
  #geom_line()+ 
  geom_fan()+
  xlab( 'Czas [ np . dni ]')+
  ylab ( ' Liczba przypadkow ( osob ) ')+
  ggtitle( " Ewolucja SIR dla N =1000 , I_0 =1(= I_1 ), \ beta =0.5 , \ gamma =0.1 ")+
  theme(legend.title = element_blank())

