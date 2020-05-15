
rm(list = ls())

ns <- 16
Vs <- rep(0, ns)
Vold <- rep(0, ns)
theta <- 1e-100

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
      Vs[i] <- -1 + max(Vs[1], Vs[6], Vs[3], Vs[2])
    }else if(i == 3){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[2], Vs[7], Vs[4], Vs[3])
    }else if(i == 4){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[3], Vs[8], Vs[4], Vs[4])
    }else if(i == 5){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[5], Vs[9], Vs[6], Vs[1])
    }else if(i == 6){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[5], Vs[10], Vs[7], Vs[2])
    }else if(i == 7){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[6], Vs[11], Vs[8], Vs[3])
    }else if(i == 8){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[7], Vs[12], Vs[8], Vs[4])
    }else if(i == 9){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[9], Vs[13], Vs[10], Vs[5])
    }else if(i == 10){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[9], Vs[14], Vs[11], Vs[6])
    }else if(i == 11){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[10], Vs[15], Vs[12], Vs[7])
    }else if(i == 12){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[11], Vs[16], Vs[12], Vs[8])
    }else if(i == 13){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[13], Vs[13], Vs[14], Vs[9])
    }else if(i == 14){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[13], Vs[14], Vs[15], Vs[10])
    }else if(i == 15){
      Vold[i] <- Vs[i]
      Vs[i] <- -1 + max(Vs[14], Vs[15], Vs[16], Vs[11])
    }else if(i == 16){
      Vold[i] <- Vs[i]
      Vs[i] <- 0
    }
    
  }#for
  
  delta <- max(abs(Vold - Vs))
  
}#while

matrix(Vs, 4)

pi_opt <- rep(0, 16)

for(i in 2:15){
  
  if(i == 2){
    pi_opt[i] <- which.max(c(Vs[1], Vs[6], Vs[3], Vs[2]))
  }else if(i == 3){
    pi_opt[i] <- which.max(c(Vs[2], Vs[7], Vs[4], Vs[3]))
  }else if(i == 4){
    pi_opt[i] <- which.max(c(Vs[3], Vs[8], Vs[4], Vs[4]))
  }else if(i == 5){
    pi_opt[i] <- which.max(c(Vs[5], Vs[9], Vs[6], Vs[1]))
  }else if(i == 6){
    pi_opt[i] <- which.max(c(Vs[5], Vs[10], Vs[7], Vs[2]))
  }else if(i == 7){
    pi_opt[i] <- which.max(c(Vs[6], Vs[11], Vs[8], Vs[3]))
  }else if(i == 8){
    pi_opt[i] <- which.max(c(Vs[7], Vs[12], Vs[8], Vs[4]))
  }else if(i == 9){
    pi_opt[i] <- which.max(c(Vs[9], Vs[13], Vs[10], Vs[5]))
  }else if(i == 10){
    pi_opt[i] <- which.max(c(Vs[9], Vs[14], Vs[11], Vs[6]))
  }else if(i == 11){
    pi_opt[i] <- which.max(c(Vs[10], Vs[15], Vs[12], Vs[7]))
  }else if(i == 12){
    pi_opt[i] <- which.max(c(Vs[11], Vs[16], Vs[12], Vs[8]))
  }else if(i == 13){
    pi_opt[i] <- which.max(c(Vs[13], Vs[13], Vs[14], Vs[9]))
  }else if(i == 14){
    pi_opt[i] <- which.max(c(Vs[13], Vs[14], Vs[15], Vs[10]))
  }else if(i == 15){
    pi_opt[i] <- which.max(c(Vs[14], Vs[15], Vs[16], Vs[12]))
  }
  
}

pi_opt <- ifelse(pi_opt == 0, "no_action", 
       ifelse(pi_opt == 1, "left", 
              ifelse(pi_opt == 2, "down", 
                     ifelse(pi_opt == 3, "right", 
                            ifelse(pi_opt == 4, "up", pi_opt)))))

matrix(pi_opt, 4, byrow = TRUE)
