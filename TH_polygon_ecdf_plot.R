library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

pdf("C://Users//DELL//OneDrive//Documents//diff_plot.pdf", h = 10, w = 10)
par(mfrow = c(1, 1))
par(mar = c(5, 5, 5, 10))
plot(1, 5, main = paste("Distribution of each Th value from 0 to 44 dB for 77 participants across 2 visits", sep = " "), 
     xlim = c(0, 45), ylim = c(0, 2500),
     xlab = "Th(dB)", axes = F,
     ylab= "Frequency", type = "n")
axis(1, at = c(1.5:45.5), labels = c(0:44))
axis(2, at = seq(0, 2400, by = 200), labels = seq(0, 2400, by = 200))
axis(4, at=seq(0, 2100,length.out = 11),
     labels=seq(0, 1, 0.1),
     col = 1, col.axis = 1)
legend(35,2490 , legend = c("SITA Standard", "ZATA Standard", "SITA Fast", "Zata Fast")
       , pch = c(rep(16, 4))
       ,bty = "n" , col =  c(1, 2, 3, 4))
mtext(side = 4, line = 2, "Cumulative Density           ", col = 1, cex = 1.2)
mtext(side = 3, line = -5, "Curves are CDF of respective algorithms ", col = 1, cex = 1.2)

x<- c(1:77)
Test<- c("SS", "SF", "ZS", "ZF")
for (test in Test){
  T<- test
  
  D1_th<- paste(T, "_th_Day1_L" , c(1:54), sep = "")
  D2_th<- paste(T, "_th_Day2_L" , c(1:54), sep = "")
  
  th1= as.numeric(unlist(DATA[x , D1_th]))
  th2= as.numeric(unlist(DATA[x , D2_th]))
  th<- c(th1, th2)
  
  h<- hist(th,plot = F, breaks = seq(0, 44, by = 1))
  points(h$mids + ifelse(T == "ZF", 1, 0) ,h$counts,h$counts,
         type = "l", lwd = 4, lty = 1, col =
           ifelse(T == "SS",1, ifelse(T == "ZS", 2, ifelse(T == "SF", 3, 4))))
  par(new = T)
  
  ec <- ecdf(th)
  points(h$mids, ec(h$mids)*max(h$counts), 
       col = ifelse(T == "SS",1, ifelse(T == "ZS", 2, ifelse(T == "SF", 3, 4))),
       axes=F, xlab=NA, ylab=NA, type =  "l", lwd = 2)
}



dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//diff_plot.pdf", format = "jpeg")
