# Implement code for good-turing smoothing 
#
# Author: filqua74

goodturing_phase1 <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  res <- matrix(nrow = n, ncol = 2)
  for (i in 1:n) {
    if (i==1) {
      q <- 0
      t <- x[i+1,1]
    } else if (i==n) {
      q <- x[i-1,1]
      t <- 2*x[i,1]-q
    } else {
      q <- x[i-1,1]
      t <- x[i+1,1]
    }
    res[i,1] <- log(x[i,1])
    res[i,2] <- log(2*x[i,2]/(t-q))
    
  }
  colnames(res) <- c("log_r","log_zr")
  return(res)
}

estProbMon <- function(r) {
  if (r==0) {
    pr <- N1/N
  } else {
    estNR <- exp(predict(monlm, newdata=data.frame(log_r=log(r))))
    estNR_1 <- exp(predict(monlm, newdata=data.frame(log_r=log(r+1))))
    pr <- ((r+1)*estNR_1)/(N*estNR)
  }
  return(pr)
}
 

estProbBig <- function(r) {
  if (r==0) {
    pr <- NB1/NB
  } else {
    estNR <- exp(predict(biglm, newdata=data.frame(log_r=log(r))))
    estNR_1 <- exp(predict(biglm, newdata=data.frame(log_r=log(r+1))))
    pr <- ((r+1)*estNR_1)/(NB*estNR)
  }
  return(pr)
}

estProbTri <- function(r) {
  if (r==0) {
    pr <- NT1/NT
  } else {
    estNR <- exp(predict(trilm, newdata=data.frame(log_r=log(r))))
    estNR_1 <- exp(predict(trilm, newdata=data.frame(log_r=log(r+1))))
    pr <- ((r+1)*estNR_1)/(NT*estNR)
  }
  return(pr)
}

# Compute counts useful for good-turing smoothing
N <- sum(monfreq$count * monfreq$freq)                    #  Total number of monograms
N1 <- monfreq[monfreq$count==1, freq]                     #  Total monograms of 1 count

NB <- sum(bigfreq$count * bigfreq$freq)                   #  Total number of bigrams
NB1 <- bigfreq[bigfreq$count==1, freq]                    #  Total bigrams of 1 count

NT <- sum(trifreq$count * trifreq$freq)                   #  Total number of trigrams
NT1 <- trifreq[trifreq$count==1, freq]                    #  Total tri

monfreq_log <- as.data.frame(goodturing_phase1(monfreq))
bigfreq_log <- as.data.frame(goodturing_phase1(bigfreq))
trifreq_log <- as.data.frame(goodturing_phase1(trifreq))
monlm <- lm(log_zr ~ log_r, monfreq_log) 
biglm <- lm(log_zr ~ log_r, bigfreq_log)
trilm <- lm(log_zr ~ log_r, trifreq_log)
