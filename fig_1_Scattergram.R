library(csvread)
library(pdftools)

DATA<- read.csv("C://Users//DELL//OneDrive//PhD work//DATA//CSV//TRV_DATA.csv")
pdf("C://Users//DELL//OneDrive//Documents//fig1.pdf", h = 10, w = 10)
par(mfrow= c(1, 1))
par(mar = c(7, 7,3, 3))
plot(c(-6,-6, -12, -12), c(-2, 42, 42, -2), type = "n", lty = 5 , lwd = 5,  
     ylim = c(0.7, 19.3 ), xlim = c(-33.5,4),xlab = " ", ylab = " ",
     axes= F)
title(xlab = "Mean Deviation (dB)",cex.lab = 2,line = 5) 
title(ylab = "Pattern Standard Deviation (dB)", cex.lab = 2,
      line = 4)
      
axis(1, at = seq(5,-35,-5), labels = seq(5,-35,-5),cex.axis= 2,
     padj = 1)
axis(2,las = 1, at=seq(0,20,5),seq(0,20,5),cex.axis= 2,
     padj = 0.5)
box()
Defect = c("Normal", "Early Damage", "Moderate Damage", "Advance Damage")

for (D in Defect){
  Stage = D
  
  SS_md_Day1<- DATA[which(DATA$Category == Stage) , paste("SS_Day1_md",sep="")]
  SS_psd_Day1<-DATA[which(DATA$Category == Stage) , paste("SS_Day1_sd",sep="")]
  
  points(SS_md_Day1, SS_psd_Day1, type = "p", pch= ifelse(Stage == Defect[1], 16, 17),
         cex = ifelse(Stage == Defect[1], 3, 3), col = ifelse(Stage == Defect[1], "orangered", "midnightblue"))
  
}
legend("topleft", legend = c(" ", " "),  pch = c(16, 17),
       col = c("orangered", "midnightblue"), cex = 3 ,bty = "n")

text(-25, 18.5, "Normal (n=22)", cex = 2)
text(-23.7, 17, "Glaucoma (n= 55)", cex = 2)
dev.off()
pdf_convert("C://Users//DELL//OneDrive//Documents//fig1.pdf")