library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
Test<- c("SS", "ZS","SF", "ZF")
for (t in Test){
  test = t
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  CD1<- c(D1, D2)
  CD2<- c(D2, D1)
  
  x <- vector("list",34)
  x_a<- rep(NA, 34)
  x_b<- rep(NA, 34)
  
  for(participant in 1:77){
    for(threshold in 0:33){
      th = threshold
      
      
      location <- which(DATA[participant,CD1]==th)
      x[[th+1]] <- c(x[[th+1]],as.numeric(DATA[participant,CD2[location]]))
      x_a[th+1]<- quantile(x[[th+1]], 0.05)
      x_b[th+1]<- quantile(x[[th+1]], 0.95)
    }
  }
  
  Bin<- vector("list", 18)
  Bin[[1]]<- x[1]
  Bin[[2]]<- x[c(2:6)]
  Bin[[3]]<- x[c(4:8)]
  Bin[[4]]<- x[c(6:11)]
  Bin[[5]]<- x[c(8:13)]
  Bin[[6]]<- x[c(11:16)]
  Bin[[7]]<- x[c(13:18)]
  Bin[[8]]<- x[c(16:21)]
  Bin[[9]]<- x[c(18:24)]
  Bin[[10]]<- x[c(21:26)]
  Bin[[11]]<- x[27]
  Bin[[12]]<- x[28]
  Bin[[13]]<- x[29]
  Bin[[14]]<- x[30]
  Bin[[15]]<- x[31]
  Bin[[16]]<- x[32]
  Bin[[17]]<- x[33]
  Bin[[18]]<- x[34]
  
  Loc<- c(0, 2.5, 5, 7.5, 9.5, 12.5, 14.5, 17.5, 20, 22.5, c(26:33))
  
  a<- rep(NA, length(Loc))
  b<- rep(NA, length(Loc))
  c<- rep(NA, length(Loc))
  d<- rep(NA, length(Loc))
  e<- rep(NA, length(Loc))
  f<- rep(NA, length(Loc))
  for(i in 1:18){
    Bin[[i]]<- unlist(Bin[i])
    a[i]<- quantile(Bin[[i]], 0.05)
    b[i]<- quantile(Bin[[i]], 0.95)
    c[i]<- quantile(Bin[[i]], 0.1)
    d[i]<- quantile(Bin[[i]], 0.9)
    e[i]<- quantile(Bin[[i]], 0.25)
    f[i]<- quantile(Bin[[i]], 0.75)
    
  }
}

Sample_list<- vector("list", 100)
for (i in 1:100){
  Sample_list[[i]]<- (sample(Bin[[18]], 361))
}


Median<- sapply(Sample_list, median)

unlist(Median)
