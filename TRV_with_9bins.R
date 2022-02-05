#Open the library and read the DATA
library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
pdf("C://Users//DELL//OneDrive//Documents//TRV_binned.pdf", h = 20, w= 20)
par(mfrow = c(2, 2))
Label<- c("0", "1-15","16-21", "22-25", 
          "25-29", "30","31", "32", "33-38")
Loc<- c(0, 7.5, 18.5, 23.5, 27, 30, 31, 32, 35.5)
Test<- c("SS", "SF", "ZS", "ZF")
for (t in Test){
  test = t
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  D1<- c(D1, D2)
  D2<- c(D2, D1)
  
  
  x <- vector("list",9)
  a<- rep(NA, 9)
  b<- rep(NA, 9)
  
  
  for(participant in 1:77){
    for(threshold in 0:38){
      th = threshold
      if(th <= 0){
        i = 1
      }else if (th >= 1 & th <=15){
        i = 2
      }else if (th >= 16  & th <= 21){
        i = 3
      }else if (th >= 22  & th <=25){
        i = 4 
      }else if (th >= 25 & th <=29){
        i = 5
      }else if (th == 30){
        i = 6
      }else if (th == 31){
        i = 7 
      }else if (th == 32){
        i = 8
      }else {
        i = 9
      }
      
      location <- which(DATA[participant,D1]==th)
      x[[i]] <- c(x[[i]],as.numeric(DATA[participant,D2[location]]))
      a[i]<- quantile(x[[i]], 0.25) 
      b[i]<- quantile(mean_run(x[[i]]), 0.75) 
      
    }
    
  }
  plot(seq(0, 39, by = 3), seq(0, 39, by = 3), type = "n", lty = 2 , axes = F,xlim = c(0, 39), ylim = c(0, 39),
       xlab = "Baseline sensitivity(dB)",
       ylab = "Retest sensitivity (dB)", main = ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                                                ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))))
  axis(1,at= Loc,labels= Loc, col = "black", cex.axis = 0.8)
  axis(2,at= seq(0, 39, by = 2),labels=seq(0, 39, by = 2), col = "black", cex.axis = 1)
  
  box()
  points(Loc, a, type = "p",  col = "black", pch =16)
  points(Loc, b, type = "p",  col = "black", pch =16)
  lines(Loc, a, type = "l", lty = 1, col = "black", cex = 1)
  lines(Loc, b, type = "l", lty = 1, col = "black", cex = 1)
  text(Loc, a+2, sapply(x, length), cex = 1, col = "black", srt = 90)
  
}
# xx <-  Loc 
# fit<- lm(a~poly(xx,2,raw=TRUE))
# lines(xx, predict(fit, col= 1))
# fit <- lm(b~poly(xx,2,raw=TRUE))
# lines(xx, predict(fit, col= 1))
# xx <-  Loc 
# 
# fit <- loess(a ~ xx, span = 0.25) 
# pred <- predict(fit, xx, se=TRUE)
# lines(xx, pred$fit, lty= 1, col= 1, lwd=2)
# fit <- loess(b ~ xx, span = 0.25) 
# pred <- predict(fit, xx, se=TRUE)
# lines(xx, pred$fit, lty= 1, col= 1, lwd=2)


dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//TRV_binned.pdf", format = "jpeg")

