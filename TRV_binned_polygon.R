library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
pdf("C://Users//DELL//OneDrive//Documents//TRV_binned_polygon.pdf", h = 20, w= 20)
par(mfrow = c(2, 2))
Test<- c("SS", "ZS","SF", "ZF")
for (t in Test){
  test = t
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  D1<- c(D1, D2)
  D2<- c(D2, D1)
  
  x <- vector("list",34)
  x_a<- rep(NA, 34)
  x_b<- rep(NA, 34)
  
  for(participant in 1:77){
    for(threshold in 0:33){
      th = threshold
      
      
      location <- which(DATA[participant,D1]==th)
      x[[th+1]] <- c(x[[th+1]],as.numeric(DATA[participant,D2[location]]))
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
  for(i in 1:18){
    Bin[[i]]<- unlist(Bin[i])
    a[i]<- quantile(Bin[[i]], 0.1)
    b[i]<- quantile(Bin[[i]], 0.9)
    c[i]<- quantile(Bin[[i]], 0.05)
    d[i]<- quantile(Bin[[i]], 0.95)
    
  }
  
  
  # 5 and 95 th percentile to hist
  plot(c(0:35), c(0: 35), type = "n", lty = 2 , lwd = 2, 
       axes = F,xlim = c(0, 34), ylim = c(0, 34),
       xlab = "Baseline sensitivity(dB)",
       ylab = "Retest sensitivity (dB)", main = ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                                                ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))))
  axis(1,at= Loc,labels= Loc, col = "black", cex.axis = 0.7)
  axis(2,at= seq(0, 34, by = 2),labels= seq(0, 34, by = 2), col = "black", cex.axis = 1)
  
  box()
  # points(c(0:33), x_a, type = "p", col = "black", pch =16, cex = 1)
  # points(c(0:33), x_b, type = "p", col = "black", pch =16, cex = 1)
  # lines(Loc, a, type = "l", lty = 1, col = "grey", cex = 1, lwd = 2)
  # lines(Loc, b, type = "l", lty = 1, col = "grey", cex = 1, lwd = 2)
  # lines(Loc, c, type = "l", lty = 1, col = "skyblue", cex = 1, lwd = 2)
  # lines(Loc, d, type = "l", lty = 1, col = "skyblue", cex = 1, lwd = 2)
  
  polygon(Loc, c, col = "grey", border = "grey", lwd = 5)
  polygon(Loc, a, col = "white", border = "white")
  polygon(Loc, d, col = "grey", border = "grey", lwd = 2)
  polygon(Loc, b, col = "white", border = "white")
  points(c(0:35), c(0: 35), type = "l", lty = 5 , lwd = 2, col = "black")
  lines(Loc,  round(sapply(Bin, mean), digits = 2), lty = 1, col = "red",lwd = 2)
  lines(Loc,  round(sapply(Bin, median), digits = 2), lty = 1, col = "yellow", lwd = 2)
  # polygon(Loc, a, col = "grey", border = "grey")
  # polygon(Loc[c(1:11)], a[c(1:11)], col = "grey", border = "grey")
  # polygon(Loc[c(11:18)], a[c(11:18)], col = "grey", border = "grey")
  # polygon(Loc, b, col = "grey", border = "grey")
  # polygon(Loc[c(1:13)], c[c(1:13)], col = "skyblue", border = "skyblue")
  # polygon(Loc[c(13:18)], c[c(13:18)], col = "skyblue", border = "skyblue")
  # polygon(Loc, d, col = "skyblue", border = "skyblue")
  text(Loc, a+2 , sapply(Bin, length), cex = 1, col = 1, srt = 90)
  legend("topleft", c("5:10 & 90:95 %-ile","Mean"," Median"),
         lty = rep(1, 4), col = c("grey","red","yellow"), bty = "n")
  
}

dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//TRV_binned_polygon.pdf", format = "jpeg")

