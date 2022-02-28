library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
png("C://Users//DELL//OneDrive//Documents//TRV_binned_polygon.png",
    family = "Times", h = 3000, w= 2600, res = 300)
layout(matrix(c(1, 3, 5, 2, 4, 5), nrow = 3, ncol = 2), 
       widths = rep(1, 2), heights = c(rep(1, 2), 0.30))
par(mar = c(5, 7, 5, 2))
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
  
  
  # 5 and 95 th percentile to hist
  plot(c(0:35), c(0: 35), type = "n", lty = 2 , lwd = 1, 
       axes = F,xlim = c(0, 34), ylim = c(1.3, 34),
       cex.main = 2, xlab = " ", 
       ylab = " ", main = ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                          ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))))
  
  
  mtext(ifelse(test == "SS", "Retest sensitivity (dB)                                                                             "," "),
        side = 2, line = 4.5, cex = 1.2)
  
  axis(1,at= seq(0, 35, by = 5),labels= seq(0, 35, by = 5), col = "black",
       cex.axis = 1.5, padj = 1)
  axis(2,at= seq(0, 35, by = 5),labels= seq(0, 35, by = 5), col = "black", 
       cex.axis = 1.5, hadj = 1.5, las = 1)
  
  box()
  polygon(c(Loc[1],Loc, Loc[18:2]), c(b[1], a, b[18:2]),col = "skyblue1", border = "skyblue1")
  polygon(c(Loc[1],Loc, Loc[18:2]), c(d[1], c, d[18:2]),col = "skyblue3", border = "skyblue3")
  polygon(c(Loc[1],Loc, Loc[18:2]), c(f[1], e, f[18:2]),col = "skyblue4", border = "skyblue4")
  points(c(0:35), c(0: 35), type = "l", lty = 5 , lwd = 1, col = "black")
  lines(Loc,  round(sapply(Bin, mean), digits = 2), lty = 1, col = "red",lwd = 1)
  lines(Loc,  round(sapply(Bin, median), digits = 2), lty = 1, col = "green",lwd = 1)
  text(Loc, a+1.5, sapply(Bin, length), cex = 1, col = "red", srt = 90, font = 1)
  
}


mtext("Baseline sensitivity(dB)                                                               ",
      side = 1 ,line = 4, cex= 1.2)
par(mar = c(0, 3, 3, 3))
plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = F)
legend("topleft", c("5 & 95 percentile","10 & 90 percentile", "25 & 75 percentile"),
       pch = rep(15, 3) , col = c("skyblue1", " skyblue3", "skyblue4")
       , bty = "n", cex = 1.5)
legend("topright", legend = c("Median", "Mean"), col = c("green","red"),
       lty = rep(1, 2),lwd = rep(1, 2), bty = "n", cex = 1.5)

dev.off()
