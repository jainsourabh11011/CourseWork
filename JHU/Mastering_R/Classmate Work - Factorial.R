factorial_loop<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          my.factorial<-1
          for (i in n:1){
               my.factorial<-my.factorial*i
          }
          return(my.factorial)
     }
}

factorial_reduce<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          library(purrr)
          return(reduce(1:n,prod))
     }
}

factorial_func<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          return(factorial_func(n-1)*n)
     }
}

my.factorvector<-rep(NA,100)
factorial_mem<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          if (!is.na(my.factorvector[n])){
               return(my.factorvector[n])
          } else {
               memo<-factorial_mem(n-1)*n
               my.factorvector[n]<-memo
               return(memo)
          }
     }
}

library(microbenchmark)

my.loop <- map(c(15,150,250,500), 
               function(x){microbenchmark(factorial_loop(x),
                                          times = 100)$time})
names(my.loop) <- c(15,150,250,500)

my.data <- as.data.frame(t(map(my.loop,mean)))

my.reduce <- map(c(15,150,250,500), 
                 function(x){microbenchmark(factorial_reduce(x),
                                            times = 100)$time})
names(my.reduce) <- c(15,150,250,500)

my.data[2,] <- as.data.frame(t(map(my.reduce,mean)))

my.recurs <- map(c(15,150,250,500), 
                 function(x){microbenchmark(factorial_func(x),
                                            times = 100)$time})
names(my.recurs) <- c(15,150,250,500)

my.data[3,] <- as.data.frame(t(map(my.recurs,mean)))

my.mem <- map(c(15,150,250,500), 
              function(x){microbenchmark(factorial_mem(x),
                                         times = 100)$time})
names(my.mem) <- c(15,150,250,500)

my.data[4,] <- as.data.frame(t(map(my.mem,mean)))

rownames(my.data) <- c("for_loop","reduce","recursion","memoization")

write.table(t(my.data),"factorial_output.txt",row.names = TRUE)

#########################################

##Assignment part 1: Factorial Functions
##first, loop-based factorial:
factorial_loop <- function(n) {
     
     ##check that n is an integer greater than or equal to 0
     if(n<0) 
          stop("this function is not designed for negative integers")
     
     result<-1
     if(n>1) {
          for(i in 2:n) {
               result <- i*result
          }
     }
     print(result)
}

##second, factorial based on reduce()
factorial_reduce<-function(n) {
     
     ##check that n is an integer greater than or equal to 0
     if(n<0) 
          stop("this function is not designed for negative integers")
     
     result<-1
     if(n>1){
          result<- reduce(1:n, function(x,y){
               x*y
          })
     }
     print(result)
}

##third, factorial based on recursion
factorial_func<-function(n) {
     
     ##check that n is an integer greater than or equal to 0
     if(n<0) 
          stop("this function is not designed for negative integers")
     
     if(n==0){
          1
     } else if(n==1){
          1
     } else {
          factorial_func(n-1)*n
     }
}

##fourth, factorial with memoizatoin
factorial_mem<-function(n) {
     
     ##check that n is an integer greater than or equal to 0
     if(n<0) 
          stop("this function is not designed for negative integers")
     if(n==0){
          0
     } else {
          fac_tab<-c(1, rep(NA,n))
          if(!is.na(fac_tab[n])){
               fac_tab[n]
          } else {
               fac_tab[n-1]<-factorial_mem(n-1)
               fac_tab[n-1]*n
          }
     }
}

microbenchmark(factorial_loop(1), factorial_reduce(1), factorial_func(1), factorial_mem(1))
  # Unit: nanoseconds
  # expr   min    lq     mean  median      uq    max neval
  # factorial_loop(1)   682  1023  1353.26  1023.0  1875.0   3407   100
  # factorial_reduce(1) 57920 59794 63953.98 61327.0 63030.5 236107   100
  # factorial_func(1)  1022  1363  1796.21  1364.0  2386.0   7837   100
  # factorial_mem(1)  3067  3408  4705.75  4429.5  5792.5  16014   100
  
microbenchmark(factorial_loop(5), factorial_reduce(5), factorial_func(5), factorial_mem(5))
  # Unit: microseconds
  # expr    min     lq     mean median      uq     max neval
  # factorial_loop(5)  3.067  3.748  4.58997  4.090  5.1115  17.036   100
  # factorial_reduce(5) 75.977 80.577 90.01769 88.243 90.1160 218.050   100
  # factorial_func(5)  8.859  9.540 10.20142  9.881 11.2430  13.629   100
  # factorial_mem(5) 25.894 26.916 28.85818 28.279 29.9830  64.394   100
  
microbenchmark(factorial_loop(10), factorial_reduce(10), factorial_func(10), factorial_mem(10))
  # Unit: microseconds
  # expr    min     lq      mean median      uq     max neval
  # factorial_loop(10)  5.451  6.133   7.11799  6.814   7.837  29.983   100
  # factorial_reduce(10) 87.902 92.501 104.61676 98.464 102.893 246.328   100
  # factorial_func(10) 19.080 20.102  20.93692 20.443  21.465  33.730   100
  # factorial_mem(10) 53.150 54.854  58.25063 56.557  58.261 166.264   100
  
microbenchmark(factorial_loop(100), factorial_func(100), factorial_mem(100))
  # Unit: microseconds
  # expr     min      lq      mean   median       uq      max neval
  # factorial_loop(100)  44.973  46.336  49.48098  48.0395  49.5730   91.649   100
  # factorial_func(100) 204.422 206.466 227.39228 209.1925 225.3755  912.401   100
  # factorial_mem(100) 582.943 593.845 695.32658 614.4580 659.0900 5781.039   100
  # NOTE: at this point, factorial_reduce produced an integer overflow error
  
microbenchmark(factorial_loop(500), factorial_func(500), factorial_mem(500))
  # Unit: microseconds
  # expr      min        lq      mean   median       uq      max neval
  # factorial_loop(500)  220.095  223.5015  232.1621  230.827  233.382  302.203   100
  # factorial_func(500) 1034.713 1051.9190 1153.0636 1097.403 1167.247 1779.489   100
  # factorial_mem(500) 3862.203 4151.6290 5041.1218 4342.764 4871.193 9302.539   100
  
microbenchmark(factorial_loop(2500))
  # Unit: milliseconds
  # expr      min       lq     mean  median       uq      max neval
  # factorial_loop(2500) 1.099447 1.126874 1.163159 1.14919 1.175423 1.426522   100
  # NOTE: at this point, factorial_func and factorial_mem returned 'evaluation nested too deeply' errors

#########################  

# -----------------------------------------------
#  Factorial Calulation by simple loop
Factorial_loop <- function(x)
{
     if(x==0) 
          return(1)
     
     if ((x==2) | (x==1))
          return(x);
     
     f <- 2;
     for (i in 3:x)
     {
          f <- f*i
     }
     
     return(f);
}

# -----------------------------------------------
#  Factorial Calulation by purrr reduce function
Factorial_reduce <- function(x)
{
     if(x==0)
          return(1)
     
     if ((x==2) | (x==1))
          return(x);
     
     f <- reduce(2:x, function(x,y) {return(as.numeric(x)*as.numeric(y))})
     
     return(f);
}

# -----------------------------------------------
#  Factorial Calulation by recursion  
Factorial_func <- function(x)
{
     if(x==0)
          return(1)
     
     if ((x==2) | (x==1))
          return(x);
     
     return(x*(Factorial_func(x-1)))
}


# ------------------------------------------------
#  Factorial Calulation by recursion with memoization of the calculated values 
#  in the global variable
Factorial_mem <- function(x)
{
     if(x==0)
          return(1)
     
     if ((x==2) | (x==1))
          return(x);
     
     # check if global store for calculated values is already created
     if(!exists("Fact_ready"))
          #and if no -  create it
          Fact_ready <<- vector("numeric", x)
     else 
          #but if yes and necassery factorial available - just return it
          if ((!is.na(Fact_ready[x])) && (Fact_ready[x]!=0))
               return(Fact_ready[x])
     
     #otherwise - start recursive calculation
     Fact_ready[x] <<- x*(Factorial_mem(x-1))
     
     return(Fact_ready[x])
     
}

microbenchmark(Factorial_loop(10), Factorial_reduce(10), Factorial_func(10), Factorial_mem(10))
# Unit: microseconds
# expr  min    lq   mean median    uq   max neval
# Factorial_loop(10)  4.9  5.50  5.970    5.8  6.10  12.6   100
# Factorial_reduce(10) 32.3 34.00 36.014   34.7 35.60  73.2   100
# Factorial_func(10) 21.1 22.85 24.829   24.0 24.65  48.5   100
# Factorial_mem(10)  5.0  5.70  7.340    6.1  6.45 120.8   100


microbenchmark(Factorial_loop(50), Factorial_reduce(50), Factorial_func(50), Factorial_mem(50))
# Unit: microseconds
# expr   min     lq    mean median     uq   max neval
# Factorial_loop(50)  15.7  17.25  21.944  18.00  25.65  59.2   100
# Factorial_reduce(50) 126.6 141.95 156.270 145.55 157.90 247.6   100
# Factorial_func(50) 120.3 125.80 138.664 128.55 136.30 221.0   100
# Factorial_mem(50)   4.8   6.00  11.041   6.65   7.30 377.7   100


microbenchmark(Factorial_loop(100), Factorial_reduce(100), Factorial_func(100), Factorial_mem(100))
# Unit: microseconds
# expr   min     lq    mean median    uq   max neval
# Factorial_loop(100)  30.7  32.85  39.458  40.05  42.0  70.2   100
# Factorial_reduce(100) 271.1 281.70 311.240 296.25 340.0 448.6   100
# Factorial_func(100) 251.2 258.70 282.072 264.55 305.1 445.6   100
# Factorial_mem(100)   4.7   5.90  16.531   7.00   7.7 949.1   100


microbenchmark(Factorial_loop(150), Factorial_reduce(150), Factorial_func(150), Factorial_mem(150))
# Unit: microseconds
# expr   min     lq    mean median     uq    max neval
# Factorial_loop(150)  44.5  51.95  56.890  55.40  57.60  113.2   100
# Factorial_reduce(150) 404.3 423.00 452.934 449.55 476.65  543.1   100
# Factorial_func(150) 379.3 393.45 422.058 407.80 445.90  602.8   100
# Factorial_mem(150)   4.7   6.20  20.148   7.15   7.80 1205.4   100


# The conclusion:
#      1. Factorial_mem() has shortest mean execution time in all four cases, and the longest maximum execution time
# 2. Factorial_reduce() and Factorial_func() are outsiders, it has longest mean execution time
# 3. Whats is surprising to me, Factorial_loop() has a good result too, probably due to internal optimization. it`s a two 
# times longer then Factorial_mem() mean only.

#########################
factorial_loop<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          my.factorial<-1
          for (i in n:1){
               my.factorial<-my.factorial*i
          }
          return(my.factorial)
     }
}

factorial_reduce<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          library(purrr)
          return(reduce(1:n,prod))
     }
}

factorial_func<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          return(factorial_func(n-1)*n)
     }
}

my.factorvector<-rep(NA,100)
factorial_mem<-function(n){
     if (!is.numeric(n)){
          print("n should be a number")
          stop()
     }
     if (n<0){
          print("n should be positive or 0")
          stop()
     }
     if (n%% 1 !=0){
          print("n should be an integer")
          stop()
     }
     if (n == 0){
          return(1)
     } else {
          if (!is.na(my.factorvector[n])){
               return(my.factorvector[n])
          } else {
               memo<-factorial_mem(n-1)*n
               my.factorvector[n]<-memo
               return(memo)
          }
     }
}

library(microbenchmark)

my.loop <- map(c(15,150,250,500), 
               function(x){microbenchmark(factorial_loop(x),
                                          times = 100)$time})
names(my.loop) <- c(15,150,250,500)

my.data <- as.data.frame(t(map(my.loop,mean)))

my.reduce <- map(c(15,150,250,500), 
                 function(x){microbenchmark(factorial_reduce(x),
                                            times = 100)$time})
names(my.reduce) <- c(15,150,250,500)

my.data[2,] <- as.data.frame(t(map(my.reduce,mean)))

my.recurs <- map(c(15,150,250,500), 
                 function(x){microbenchmark(factorial_func(x),
                                            times = 100)$time})
names(my.recurs) <- c(15,150,250,500)

my.data[3,] <- as.data.frame(t(map(my.recurs,mean)))

my.mem <- map(c(15,150,250,500), 
              function(x){microbenchmark(factorial_mem(x),
                                         times = 100)$time})
names(my.mem) <- c(15,150,250,500)

my.data[4,] <- as.data.frame(t(map(my.mem,mean)))

rownames(my.data) <- c("for_loop","reduce","recursion","memoization")

write.table(t(my.data),"factorial_output.txt",row.names = TRUE)

