
rm(list = ls())

ns <- 16
theta <- 2

Vs <- rep(0, ns)
Vold <- rep(0, ns)

pi_s <- matrix(0, ncol = 4, nrow = 16)
for(i in 2:15){
  pi_s[i, sample(1:4, 1)] <- 1  
}

fin <- 0

while(fin < 1000){

  fin <- fin + 1
  
  delta <- 100
  iter <- 0
  while(delta > theta & iter < 1000){
    
    iter <- iter + 1
    
    for(i in 1:ns){
      
      if(i == 1){
        Vold[i] <- Vs[i]
        Vs[i] <- 0
      }else if(i == 2){
        Vold[i] <- Vs[i]

        if(pi_s[2, 1] == 1){
          Vs[i] <- -1 + Vs[1]
        }else if(pi_s[2, 2] == 1){
          Vs[i] <- -1 + Vs[6]
        }else if(pi_s[2, 3] == 1){
          Vs[i] <- -1 + Vs[3]
        }else{
          Vs[i] <- -1 + Vs[2]
        }
        
      }else if(i == 3){
        Vold[i] <- Vs[i]

        if(pi_s[3, 1] == 1){
          Vs[i] <- -1 + Vs[2]
        }else if(pi_s[3, 2] == 1){
          Vs[i] <- -1 + Vs[7]
        }else if(pi_s[3, 3] == 1){
          Vs[i] <- -1 + Vs[4]
        }else{
          Vs[i] <- -1 + Vs[3]
        }
        
      }else if(i == 4){
        Vold[i] <- Vs[i]
        
        if(pi_s[4, 1] == 1){
          Vs[i] <- -1 + Vs[3]
        }else if(pi_s[4, 2] == 1){
          Vs[i] <- -1 + Vs[8]
        }else if(pi_s[4, 3] == 1){
          Vs[i] <- -1 + Vs[4]
        }else{
          Vs[i] <- -1 + Vs[4]
        }
        
      }else if(i == 5){
        Vold[i] <- Vs[i]

        if(pi_s[5, 1] == 1){
          Vs[i] <- -1 + Vs[5]
        }else if(pi_s[5, 2] == 1){
          Vs[i] <- -1 + Vs[9]
        }else if(pi_s[5, 3] == 1){
          Vs[i] <- -1 + Vs[6]
        }else{
          Vs[i] <- -1 + Vs[1]
        }
        
      }else if(i == 6){
        Vold[i] <- Vs[i]

        if(pi_s[6, 1] == 1){
          Vs[i] <- -1 + Vs[5]
        }else if(pi_s[6, 2] == 1){
          Vs[i] <- -1 + Vs[10]
        }else if(pi_s[6, 3] == 1){
          Vs[i] <- -1 + Vs[7]
        }else{
          Vs[i] <- -1 + Vs[2]
        }
        
      }else if(i == 7){
        Vold[i] <- Vs[i]

        if(pi_s[7, 1] == 1){
          Vs[i] <- -1 + Vs[6]
        }else if(pi_s[7, 2] == 1){
          Vs[i] <- -1 + Vs[11]
        }else if(pi_s[7, 3] == 1){
          Vs[i] <- -1 + Vs[8]
        }else{
          Vs[i] <- -1 + Vs[3]
        }
        
      }else if(i == 8){
        Vold[i] <- Vs[i]

        if(pi_s[8, 1] == 1){
          Vs[i] <- -1 + Vs[7]
        }else if(pi_s[8, 2] == 1){
          Vs[i] <- -1 + Vs[12]
        }else if(pi_s[8, 3] == 1){
          Vs[i] <- -1 + Vs[8]
        }else{
          Vs[i] <- -1 + Vs[4]
        }
        
      }else if(i == 9){
        Vold[i] <- Vs[i]

        if(pi_s[9, 1] == 1){
          Vs[i] <- -1 + Vs[9]
        }else if(pi_s[9, 2] == 1){
          Vs[i] <- -1 + Vs[13]
        }else if(pi_s[9, 3] == 1){
          Vs[i] <- -1 + Vs[10]
        }else{
          Vs[i] <- -1 + Vs[5]
        }
        
      }else if(i == 10){
        Vold[i] <- Vs[i]

        if(pi_s[10, 1] == 1){
          Vs[i] <- -1 + Vs[9]
        }else if(pi_s[10, 2] == 1){
          Vs[i] <- -1 + Vs[14]
        }else if(pi_s[10, 3] == 1){
          Vs[i] <- -1 + Vs[11]
        }else{
          Vs[i] <- -1 + Vs[6]
        }
        
      }else if(i == 11){
        Vold[i] <- Vs[i]

        if(pi_s[11, 1] == 1){
          Vs[i] <- -1 + Vs[10]
        }else if(pi_s[11, 2] == 1){
          Vs[i] <- -1 + Vs[15]
        }else if(pi_s[11, 3] == 1){
          Vs[i] <- -1 + Vs[12]
        }else{
          Vs[i] <- -1 + Vs[7]
        }
        
      }else if(i == 12){
        Vold[i] <- Vs[i]

        if(pi_s[12, 1] == 1){
          Vs[i] <- -1 + Vs[11]
        }else if(pi_s[12, 2] == 1){
          Vs[i] <- -1 + Vs[16]
        }else if(pi_s[12, 3] == 1){
          Vs[i] <- -1 + Vs[12]
        }else{
          Vs[i] <- -1 + Vs[8]
        }
        
      }else if(i == 13){
        Vold[i] <- Vs[i]

        if(pi_s[13, 1] == 1){
          Vs[i] <- -1 + Vs[13]
        }else if(pi_s[13, 2] == 1){
          Vs[i] <- -1 + Vs[13]
        }else if(pi_s[13, 3] == 1){
          Vs[i] <- -1 + Vs[14]
        }else{
          Vs[i] <- -1 + Vs[9]
        }
        
      }else if(i == 14){
        Vold[i] <- Vs[i]

        if(pi_s[14, 1] == 1){
          Vs[i] <- -1 + Vs[13]
        }else if(pi_s[14, 2] == 1){
          Vs[i] <- -1 + Vs[14]
        }else if(pi_s[14, 3] == 1){
          Vs[i] <- -1 + Vs[15]
        }else{
          Vs[i] <- -1 + Vs[10]
        }
        
      }else if(i == 15){
        Vold[i] <- Vs[i]

        if(pi_s[15, 1] == 1){
          Vs[i] <- -1 + Vs[14]
        }else if(pi_s[15, 2] == 1){
          Vs[i] <- -1 + Vs[15]
        }else if(pi_s[15, 3] == 1){
          Vs[i] <- -1 + Vs[16]
        }else{
          Vs[i] <- -1 + Vs[11]
        }
        
      }else if(i == 16){
        Vold[i] <- Vs[i]
        Vs[i] <- 0
      }
      
    }#for
    
    delta <- max(abs(Vold - Vs))
    
  }#while
  
  matrix(Vs, 4)
  
  pstable <- TRUE
  
  old_act <- rep(0, 16)
  
  for(i in 2:(ns-1)){
    
    old_act[i] <- which(pi_s[i, ] == 1)
    pi_s[i, ] <- 0
    
    if(i == 2){
      pi_s[i, which.max(-1 + c(Vs[1], Vs[6], Vs[3], Vs[2]))] <- 1
    }else if(i == 3){
      pi_s[i, which.max(-1 + c(Vs[2], Vs[7], Vs[4], Vs[3]))] <- 1
    }else if(i == 4){
      pi_s[i, which.max(-1 + c(Vs[3], Vs[8], Vs[4], Vs[4]))] <- 1
    }else if(i == 5){
      pi_s[i, which.max(-1 + c(Vs[5], Vs[9], Vs[6], Vs[1]))] <- 1
    }else if(i == 6){
      pi_s[i, which.max(-1 + c(Vs[5], Vs[10], Vs[7], Vs[2]))] <- 1
    }else if(i == 7){
      pi_s[i, which.max(-1 + c(Vs[6], Vs[11], Vs[8], Vs[3]))] <- 1
    }else if(i == 8){
      pi_s[i, which.max(-1 + c(Vs[7], Vs[12], Vs[8], Vs[4]))] <- 1
    }else if(i == 9){
      pi_s[i, which.max(-1 + c(Vs[9], Vs[13], Vs[10], Vs[5]))] <- 1
    }else if(i == 10){
      pi_s[i, which.max(-1 + c(Vs[9], Vs[14], Vs[11], Vs[6]))] <- 1
    }else if(i == 11){
      pi_s[i, which.max(-1 + c(Vs[10], Vs[15], Vs[12], Vs[7]))] <- 1
    }else if(i == 12){
      pi_s[i, which.max(-1 + c(Vs[11], Vs[16], Vs[12], Vs[8]))] <- 1
    }else if(i == 13){
      pi_s[i, which.max(-1 + c(Vs[13], Vs[13], Vs[14], Vs[9]))] <- 1
    }else if(i == 14){
      pi_s[i, which.max(-1 + c(Vs[13], Vs[14], Vs[15], Vs[10]))] <- 1
    }else{
      pi_s[i, which.max(-1 + c(Vs[14], Vs[15], Vs[16], Vs[11]))] <- 1
    }
    
  }
  
  if(all(apply(pi_s[2:15, ], 1, which.max) == old_act[2:15])) break
    
}

colnames(pi_s) <- c("left", "down", "right", "up")
pi_s
