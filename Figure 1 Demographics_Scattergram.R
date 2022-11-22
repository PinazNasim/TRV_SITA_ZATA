library(csvread)
DATA<- read.csv("C://Users//DELL//OneDrive//PhD work//DATA//CSV//TRV_DATA.csv")
png("C://Users//DELL//OneDrive//Documents//fig1.png",
    h = 1150, w = 1150, res = 300)
par(mfrow= c(1, 1))
par(mar = c(3, 3,0.5 ,0.5))
plot(c(-6,-6, -12, -12), c(-2, 42, 42, -2), type = "n", lty = 5 , lwd = 5,  
     ylim = c(0.7, 19.3 ), xlim = c(-33.5,4),xlab = " ", ylab = " ",
     axes= F)
title(xlab = "Mean Deviation (dB)",cex.lab = 1,line = 2) 
title(ylab = "Pattern Standard Deviation (dB)", cex.lab = 1,
      line = 2)
axis(1, at = seq(5,-35, by = -5), labels = seq(5,-35, by = -5),cex.axis= 0.8,
     padj = -1, tck = -0.02)
axis(2,las = 1, at=seq(0,20,5),seq(0,20,5),cex.axis= 0.8,
     padj = 0.5, hadj = 0.5, tck = -0.02)
box(col = "black")
Defect = c("Normal", "Early Damage", "Moderate Damage", "Advance Damage")
for (D in Defect){
  Stage = D
  
  SS_md_Day1<- DATA[which(DATA$Category == Stage) , paste("SS_Day1_md",sep="")]
  SS_psd_Day1<-DATA[which(DATA$Category == Stage) , paste("SS_Day1_sd",sep="")]
  
  points(SS_md_Day1, SS_psd_Day1, type = "p", pch= ifelse(Stage == Defect[1], 16, 17),
         cex = 1, col = ifelse(Stage == Defect[1], "#e66101", "#5e3c99"))
  
}
legend("topleft", legend = c("Healthy (n=22)", "Glaucoma (n= 55)"),  pch = c(16, 17),
       col = c("#e66101", "#5e3c99"), cex = 1 ,bty = "n")

dev.off()