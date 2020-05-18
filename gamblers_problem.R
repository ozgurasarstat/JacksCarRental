
rm(list=ls())

ph <- 0.4

states <- 1:99
nstates <- length(states)

Vs <- Vold <- rep(0, 99)

theta <- 10^-10
delta <- 1

iter <- 0

while(delta > theta){
  
  iter <- iter + 1
  print(iter)
  
  for(i in 1:nstates){
    
    Vold[i] <- Vs[i]
    
    n_action_i     <- min(states[i], 100 - states[i]) 
    actions_i      <- 1:n_action_i
    action_value_i <- rep(0, n_action_i)
    
    for(ii in 1:n_action_i){
      
      Stplus_win  <- states[i] + actions_i[ii]
      Stplus_lose <- states[i] - actions_i[ii]
      
      expr1 <- ifelse(Stplus_win == 100, 0, 0) * ph + 0 * (1 - ph)
      expr2 <- ifelse(Stplus_win == 100, 1, Vs[states == Stplus_win]) * ph + 
        ifelse(Stplus_lose == 0, 0, Vs[states == Stplus_lose]) * (1 - ph)
      
      action_value_i[ii] <- expr1 + expr2
      
    }
    
    Vs[i] <- max(action_value_i)
    
  }
  
  delta <- max(abs(Vold - Vs))
  
}

pi_star <- rep(-10, nstates)

for(i in 1:nstates){
  
  n_action_i     <- min(states[i], 100 - states[i]) 
  actions_i      <- 1:n_action_i
  action_value_i <- rep(0, n_action_i)
  
  for(ii in 1:n_action_i){
    
    Stplus_win  <- states[i] + actions_i[ii]
    Stplus_lose <- states[i] - actions_i[ii]
    
    expr1 <- ifelse(Stplus_win == 100, 1, 0) * ph + 0 * (1 - ph)
    expr2 <- ifelse(Stplus_win == 100, 0, Vs[states == Stplus_win]) * ph + 
      ifelse(Stplus_lose == 0, 0, Vs[states == Stplus_lose]) * (1 - ph)
    
    action_value_i[ii] <- expr1 + expr2
    
  }
  
  pi_star[i] <- max(actions_i[which(action_value_i == max(action_value_i))])
  
}

par(mfrow = c(1, 2))
plot(states, Vs, type = "l", lwd = 2)
plot(states, pi_star, type = "p", lwd = 1, pch = 19, cex = 0.6)

