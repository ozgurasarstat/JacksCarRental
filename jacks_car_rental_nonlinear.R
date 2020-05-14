rm(list = ls())

setwd("C:\\Users\\ozgur.asar\\Desktop\\jacks")

suppressMessages(library(tidyverse))

min_cars <- 0
max_cars <- 20

req_lambda_loc1 <- 3
ret_lambda_loc1 <- 3

req_lambda_loc2 <- 4
ret_lambda_loc2 <- 2

pois_upper <- 11

pr_rent <- 10
pr_move <- -2

gamma <- 0.9

states <- c(rep(min_cars:max_cars, each = (max_cars+1)), 
            rep(min_cars:max_cars, (max_cars+1))) %>% 
  matrix(ncol = 2, byrow = FALSE, dimnames=list(NULL, c("loc1", "loc2")))

n_states <- nrow(states)

actions <- -5:5
n_actions <- length(actions)

Vs   <- rep(0, n_states)
Vold <- rep(0, n_states)
pi_s <- matrix(0, nrow = n_states, ncol = n_actions)
pi_s[, 6] <- 1
colnames(pi_s) <- actions

theta <- 1
delta <- 21

finish <- 0

while(finish < 100){
  
  finish <- finish + 1
  
  pi_s_iter <- matrix(apply(pi_s, 1, function(x) actions[which(x == 1)]), ncol = (max_cars + 1), byrow = TRUE)
  colnames(pi_s_iter) <- rownames(pi_s_iter) <- 0:20
  pi_s_iter <- pi_s_iter[order(as.numeric(rownames(pi_s_iter)), decreasing = TRUE), ]
  
  write.table(pi_s_iter, paste0("pi_s", finish, ".txt"))
  
  Vs_iter <- matrix(Vs, ncol = (max_cars + 1), byrow = TRUE) 
  colnames(Vs_iter) <- rownames(Vs_iter) <- 0:20
  Vs_iter <- Vs_iter[order(as.numeric(rownames(Vs_iter)), decreasing = TRUE), ] %>% round(0)
  
  write.table(Vs_iter, paste0("Vs", finish, ".txt"))
  
  
  while(delta > theta){
    
    for(i in 1:n_states){
      
      cat("Finish =", finish, "--- Policy eval ---- i =", i, "\n")
      
      Vold[i] <- Vs[i]
      
      action_i <- actions[which(pi_s[i, ] == 1)]
      
      cars_loc1 <- states[i, 1] - action_i
      cars_loc2 <- states[i, 2] + action_i
      
      add_cost <- ifelse(cars_loc1 > 10, -4, 0) + ifelse(cars_loc2 > 10, -4, 0)
      
      if(action_i > 0){
        rew_move <- (action_i - 1) * pr_move
      }else{
        rew_move <- abs(action_i) * pr_move
      }
      
      expr_left <- expr_right <- 0
      
      for(j in 0:pois_upper){   # request1 
        for(k in 0:pois_upper){ # request2
          
          req_prob <- dpois(j, req_lambda_loc1) * dpois(k, req_lambda_loc2)
          
          act_req1 <- min(cars_loc1, j)
          act_req2 <- min(cars_loc2, k)
          
          rew_rent <- (act_req1 + act_req2) * pr_rent
          
          expr_left <- expr_left + rew_rent * req_prob
          
          for(s in 0:pois_upper){   # return1
            for(t in 0:pois_upper){ # return2
              
              ret_prob <- dpois(s, ret_lambda_loc1) * dpois(t, ret_lambda_loc2)
              prob     <- ret_prob * req_prob
              
              Stplust1 <- c(min((cars_loc1 - act_req1 + s), max_cars), min((cars_loc2 - act_req2 + t), max_cars))
              ind <- which(states[, 1] == Stplust1[1] & states[, 2] == Stplust1[2])
              
              expr_right <- expr_right + gamma * Vs[ind] * prob
              
            }
          }
        }
      }
      
      Vs[i] <- rew_move + expr_left + expr_right + add_cost 
      
    }
    
    delta <- max(abs(Vold - Vs))
    
  }
  ## end of policy evaluation
  
  delta <- theta + 1
  
  ## start of policy improvement      
  old_act <- matrix(0, nrow = nrow(pi_s), ncol = ncol(pi_s))
  
  for(i in 1:n_states){
    
    cat("Finish =", finish, "--- Policy improv ---- i =", i, "\n")
    
    which_pi_s_1 <- which(pi_s[i, ] == 1)
    old_act[i, which_pi_s_1] <- 1
    pi_s[i, which_pi_s_1] <- 0
    
    action_value <- rep(NA, n_actions)
    
    for(ii in 1:n_actions){
      
      action_i <- actions[ii]
      
      cars_loc1 <- states[i, 1] - action_i
      cars_loc2 <- states[i, 2] + action_i
      
      if((cars_loc1 >= min_cars) & (cars_loc1 <= max_cars) &
         (cars_loc2 >= min_cars) & (cars_loc2 <= max_cars)){
        
        add_cost <- ifelse(cars_loc1 > 10, -4, 0) + ifelse(cars_loc2 > 10, -4, 0)
        
        if(action_i > 0){
          rew_move <- (action_i - 1) * pr_move
        }else{
          rew_move <- abs(action_i) * pr_move
        }
        
        expr_left <- expr_right <- 0
        
        for(j in 0:pois_upper){   # request1 
          for(k in 0:pois_upper){ # request2
            
            act_req1 <- min(cars_loc1, j)
            act_req2 <- min(cars_loc2, k)
            rew_rent <- (act_req1 + act_req2) * pr_rent
            
            req_prob <- dpois(j, req_lambda_loc1) * dpois(k, req_lambda_loc2)
            
            expr_left <- expr_left + rew_rent * req_prob
            
            for(s in 0:pois_upper){   # return1
              for(t in 0:pois_upper){ # return2
                
                ret_prob <- dpois(s, ret_lambda_loc1) * dpois(t, ret_lambda_loc2)
                prob     <- req_prob * ret_prob
                
                Stplust1 <- c(min((cars_loc1 - act_req1 + s), max_cars), min((cars_loc2 - act_req2 + t), max_cars))
                ind <- which(states[, 1] == Stplust1[1] & states[, 2] == Stplust1[2])
                
                expr_right <- expr_right + gamma * Vs[ind] * prob
                
              }
            }
          }
        }
        
        action_value[ii] <- rew_move + expr_left + expr_right + add_cost
        
      }
      
    }
    
    if(is.na(action_value) %>% all()){
      pi_s[i, 6] <- 1
    }else{
      pi_s[i, which.max(action_value)] <- 1
    }
    
  }
  
  if(all(apply(pi_s, 1, which.max) == apply(old_act, 1, which.max))){
    
    pi_s_final <- matrix(apply(pi_s, 1, function(x) actions[which(x == 1)]), ncol = (max_cars + 1), byrow = TRUE)
    colnames(pi_s_final) <- rownames(pi_s_final) <- 0:20
    pi_s_final <- pi_s_final[order(as.numeric(rownames(pi_s_final)), decreasing = TRUE), ]
    
    write.table(pi_s_final, "pi_s_final.txt")
    
    Vs_final <- matrix(Vs, ncol = (max_cars + 1), byrow = TRUE) 
    colnames(Vs_final) <- rownames(Vs_final) <- 0:20
    Vs_final <- Vs_final[order(as.numeric(rownames(Vs_final)), decreasing = TRUE), ] %>% round(0)
    
    write.table(Vs_final, "Vs_final.txt")
    
    break
  }
  
}