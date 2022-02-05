library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
pdf("C://Users//DELL//OneDrive//Documents//TRV_unbinned.pdf", h = 20, w= 20)
par(mfrow = c(2, 2))
Test<- c("SS", "ZS","SF", "ZF")
for (t in Test){
  test = t
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  D1<- c(D1, D2)
  D2<- c(D2, D1)
  
  x <- vector("list",39)
  
  
  for(participant in 1:77){
    for(threshold in 0:38){
      th = threshold
      
      
      location <- which(DATA[participant,D1]==th)
      x[[th+1]] <- c(x[[th+1]],as.numeric(DATA[participant,D2[location]]))
      
    }
  }
  
  Bin<- vector("list", 19)
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
  Bin[[19]]<- (x[c(35:39)])
  Loc<- c(0, 2.5, 5, 7.5, 9.5, 12.5, 14.5, 17.5, 20, 22.5, c(26:33), 36)
  
  a<- rep(NA, length(Loc))
  b<- rep(NA, length(Loc))
  for(i in 1:19){
    Bin[[i]]<- unlist(Bin[i])
    a[i]<- quantile(Bin[[i]], 0.05)
    b[i]<- quantile(Bin[[i]], 0.95)
  }
  
  plot(c(0:39), c(0: 39), type = "l", lty = 2 , axes = F,xlim = c(0, 40), ylim = c(0, 39),
       xlab = "Baseline sensitivity(dB)",
       ylab = "Retest sensitivity (dB)", main = ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                                                ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))))
  axis(1,at=seq(0, 38, by = 2),labels=seq(0, 38, by = 2), col = "black", cex.axis = 1)
  axis(2,at= seq(0, 38, by = 2),labels=seq(0, 38, by = 2), col = "black", cex.axis = 1)
  
  box()
  points(Loc, a, type = "p", col = "black", pch =16, cex = 1)
  points(Loc, b, type = "p", col = "black", pch =16, cex = 1)
  lines(Loc, a, type = "l", lty = 1, col = "black", cex = 1)
  lines(Loc, b, type = "l", lty = 1, col = "black", cex = 1)
  text(Loc, a+2 , sapply(Bin, length), cex = 1, col = 1, srt = 90)
  points(Loc,  round(sapply(Bin, mean), digits = 2), cex = 1, col = 2, pch = 16)
  points(Loc,  round(sapply(Bin, median), digits = 2), cex = 1, col = 3, pch = 16)
  legend("topleft", c("Length", "Mean"," Median"), pch = c(16, 16, 16), col = c(1, 2, 3))
  
}

dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//TRV_unbinned.pdf", format = "jpeg")

