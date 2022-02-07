library(csvread)
library(pdftools)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

Label<- c("0", "1-15","16-21", "22-25", "30-33")
Loc<- c(0, 7.5, 18.5, 23.5, 31.5)
pdf(paste("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", sep = ""), h = 90, w= 75)

layout(matrix (c(c(25:30), c(1:24)), ncol = 5, byrow = F)
       ,heights = c(0.25,rep(1, 5)), widths = rep(1, 5))

par(mar = c(10, 10, 10, 10))
Test<- c("SS", "SF",  "ZS", "ZF")
for (t in Test){
  test = t
  
  x <- vector("list",5)
  Low <- vector("list",5)
  Up <- vector("list",5)
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  D1<- c(D1, D2)
  D2<- c(D2, D1)
  for(participant in 1:77){
    for(threshold in 0:33){
      th = threshold
      
      if(th == 0){
        i = 1
      }else if (th >= 1 & th <=15){
        i = 2
      }else if (th >= 16  & th <= 21){
        i = 3
      }else if (th >= 22  & th <=25){
        i = 4 
      }else {
        i = 5
      }
      
      
      location <- which(DATA[participant,D1]==th)
      x[[i]] <- c(x[[i]],as.numeric(DATA[participant,D2[location]]))
      Low[[i]]<- quantile(x[[i]], 0.05)
      Up[[i]]<- quantile(x[[i]],0.95)
      
    }
  }
  plot(5, 5, type = "n", axes = F, xlab = "", ylab = "")
  mtext(ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "b) SITA Fast",
                                                        ifelse(test == "ZS", "c) ZATA Standard", "d) ZATA Fast"))), side = 3,  line = -10, cex = 10)
  
  
  for(n in 1:5){
    
    
    hist(x[[n]], breaks = seq(min(x[[n]]), max(x[[n]]), by = 1), cex.lab = 8,
         cex.axis = 5,main= " ",
         xlim = c(0, 40),  xlab = " ", ylab = " " , col = which(Test == test))
    points(Loc, Low)
    points(Loc, Up)
    
    mtext(paste("Total points = ", length(x[[n]]), "     ", sep = " "), side = 3, 
          line = -15, cex = 5)
  }
  
  
  
}


plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
text(5, 5 , "Bin", cex = 15)

for (n in 1:5){
  plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
  text(5, 5 , paste("Bin",n, "for", Label[n]," dB", sep = " "), cex = 10)
  
}

mtext("---------------------------------------------------------------------------------------Retest threshold (dB)------------------------------->"
      , side = 1, cex = 10, line = 9)
mtext("----------------------------------------------------------------------------------------------------------------------Frequency------------------------------------->"
      , side = 2, cex = 10, line = 1)



dev.off()
pdf_convert(paste("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", sep = ""), format = "jpeg")

