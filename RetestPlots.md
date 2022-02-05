# TRV_SITA_ZATA
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
  a<- rep(NA, 39)
  b<- rep(NA, 39)
  
  for(participant in 1:77){
    for(threshold in 0:38){
      th = threshold    
      
      location <- which(DATA[participant,D1]==th)
      x[[th+1]] <- c(x[[th+1]],as.numeric(DATA[participant,D2[location]]))
      
    }
  }
  for (i in 3:30){
    x[[i]]<- c(x[[i]], x[[i-1]])
  }
  for(threshold in 1:38){
    th = threshold
    a[th]<- quantile(x[[th]], 0.05)
    b[th]<- quantile(x[[th]], 0.95)
  }
  plot(c(0:39), c(0: 39), type = "n", lty = 2 , axes = F,xlim = c(0, 40), ylim = c(0, 39),
       xlab = "Baseline sensitivity(dB)",
       ylab = "Retest sensitivity (dB)", main = ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                                                ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))))
  axis(1,at=seq(0, 38, by = 2),labels=seq(0, 38, by = 2), col = "black", cex.axis = 1)
  axis(2,at= seq(0, 38, by = 2),labels=seq(0, 38, by = 2), col = "black", cex.axis = 1)
  
  box()
  points(c(0:38), a, type = "p", col = "black", pch =16, cex = 1)
  points(c(0:38), b, type = "p", col = "black", pch =16, cex = 1)
  lines(c(0:38), a, type = "l", lty = 1, col = "black", cex = 1)
  lines(c(0:38), b, type = "l", lty = 1, col = "black", cex = 1)
  text(c(0:38), a+2 , sapply(x, length), cex = 1, col = "black", srt = 90)
  
  
}

dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//TRV_unbinned.pdf", format = "jpeg")

