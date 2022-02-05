library(csvread)
library(pdftools)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

Label<- c("0", "1-15","16-21", "22-25","25-29", "30","31", "32", "33-38")
Loc<- c(0, 7.5, 18.5, 23.5, 27, 30, 31, 32, 35.5)
pdf(paste("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", sep = ""), h = 180, w= 150)

layout(matrix (c(c(41:50), c(1:40)), ncol = 5, byrow = F)
       ,heights = c(0.25,rep(1, 9)), widths = rep(1, 5))

par(mar = c(10, 10, 10, 10))
Test<- c("SS", "SF",  "ZS", "ZF")
for (t in Test){
  test = t
  
  x <- vector("list",9)
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  D1<- c(D1, D2)
  D2<- c(D2, D1)
  for(participant in 1:77){
    for(threshold in 0:38){
      th = threshold
      
      if(th <= 0){
        i = 1
      }else if (th >= 1 & th <=10){
        i = 2
      }else if (th >= 11  & th <= 21){
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
      
    }
  }
  plot(5, 5, type = "n", axes = F, xlab = "", ylab = "")
  mtext(ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "b) SITA Fast",
                                                        ifelse(test == "ZS", "c) ZATA Standard", "d) ZATA Fast"))), side = 3,  line = -10, cex = 15)
  
  
  for(n in 1:9){
    
    
    hist(x[[n]], breaks = seq(min(x[[n]]), max(x[[n]]), by = 1), cex.lab = 8,
         cex.axis = 5,main= " ",
         xlim = c(0, 40),  xlab = " ", ylab = " " , col = which(Test == test))
    
    mtext(paste("Total points = ", length(x[[n]]), "                ", sep = " "), side = 3, 
          line = -15, cex = 7)
  }
  
  
  
}


plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
text(5, 5 , "Bin", cex = 18)

for (n in 1:9){
  plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
  text(5, 5 , paste("Bin",n, "for", Label[n]," dB", sep = " "), cex = 18)
  
}

mtext("--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------Retest threshold (dB)------------------------------------------------------------------>"
      , side = 1, cex = 10, line = 9)
mtext("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------Frequency--------------------------------------------------------------------->"
      , side = 2, cex = 10, line = 1)



dev.off()
pdf_convert(paste("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", sep = ""), format = "jpeg")

