# Parametry , wektory SIR i wartosci poczatkowe
# beta - parametr infekcji, parametr ozdrowieninia
beta <- 0.5
gamma <- 0.1
N <- 1000
Time <- 100
M <- 100


sSIR <- data.frame(N = rep(0, Time*N*M),
                   T = rep(0, Time*N*M),
                   S = rep(0, Time*N*M), 
                   I = rep(0, Time*N*M), 
                   R = rep(0, Time*N*M))  

#symulacja M razy 
for (m in seq(1, M)){
  sSIR$I[(m-1)*Time*N + 1 ] <- 1
  sSIR$S[(m-1)*Time*N + 1 ] <- N - sSIR$I[1]  
  sSIR$R[(m-1)*Time*N + 1 ] <- 0 
  # Ewolucja czasowa
  u1 <- runif(Time*N ,0, 1) 
  u2 <- runif(Time*N ,0, 1)
  for (t in seq(1, Time)){
    for(n in seq(1, N)){
      i <- (m-1)*Time*N + (t-1)*N + n  
      if (i < (m-1)*Time*N + Time*N) {
        
        
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
}
