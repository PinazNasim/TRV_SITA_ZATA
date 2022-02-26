# Mining required data
library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
paste(T, "_th_Day1_L" , c(1:54), sep = "")
SS<- as.numeric(unlist(DATA[  ,c(paste("SS_th_Day1_L", c(1:54),sep = ""), 
                                 paste("SS_th_Day2_L", c(1:54),sep = ""))]))
ZS<- as.numeric(unlist(DATA[  ,c(paste("ZS_th_Day1_L", c(1:54),sep = ""), 
                                 paste("ZS_th_Day2_L", c(1:54),sep = ""))]))
SF<- as.numeric(unlist(DATA[  ,c(paste("SF_th_Day1_L", c(1:54),sep = ""), 
                                 paste("SF_th_Day2_L", c(1:54),sep = ""))]))
ZF<- as.numeric(unlist(DATA[  ,c(paste("ZF_th_Day1_L", c(1:54),sep = ""), 
                                 paste("ZF_th_Day2_L", c(1:54),sep = ""))]))
Th<- vector("list", 4)
Th[["SS"]]<- SS[which(SS < 36)]
Th[["ZS"]]<- ZS[which(ZS < 36)]
Th[["SF"]]<- SF[which(SF < 36)]
Th[["ZF"]]<- ZF[which(ZF < 36)]


# Data mined

# Plotting data with the mined data
png("C://Users//DELL//OneDrive//Documents//polygon_and_ecdf.png",
    h = 2000, w = 2000, units = "px",  pointsize = 12, res = 300)
layout(matrix(c(1:2), nrow = 2, ncol = 1), heights = c(1,0.75))
par(mar = c(0, 7, 1, 1))
# First plot Frequency distribution

plot(1, 5, main = " ", 
     xlim = c(2.75, 35.5), ylim = c(0, 2120),
     xlab = " ", axes = F,
     ylab= " ", type = "n")
title(ylab= "Frequency Distribution", cex.lab = 1, line = 4)
axis(2, at = seq(0, 2100, by = 210),line = 0.5,labels = seq(0, 2100, by = 210),
     las = 1,cex.axis = 1)

abline (v=seq(1.5, 37.5, 5), col="grey88",lwd = 0.5, lty = 1)
abline (h= 2200 , col="grey88",lwd = 0.5, lty = 1)

Test<- c("ZS", "ZF", "SF","SS")
for (t in Test){
  test = t
  h<- hist(Th[[test]],plot = F, breaks = 36)
  points(h$mids + ifelse(test == "ZF", 1.25, 1) ,h$counts,
         type = "l", lwd = 4, lty = 1, col = ifelse(test == "SS","lightslateblue",ifelse(test == "ZS", "midnightblue",ifelse(test == "SF", "limegreen", "firebrick1"))))
  
}
legend(2.5,1825, legend = c("SITA Standard",  "SITA Fast",
                            "ZATA Standard","ZATA Fast (shifted by 0.5 dB)")
       , lty = c(rep(1, 4)), lwd = 6, yjust = 0.5,
       bty = "n" ,bg = "white", box.col = "white",
       col =  c("lightslateblue", "limegreen", "midnightblue","firebrick1"),
       cex = 1)

# Second plot Cumulative distribution
par(mar = c(7, 7, 0, 1))
plot(1, 5, main = " ", 
     xlim = c(2.75, 35.5), ylim = c(0.02, 1.08),
     xlab = " ", axes = F,
     ylab= " ", type = "n")

title(ylab= "Cumulative Distribution      ", cex.lab = 1,
      line = 4)
title(xlab= "Threshold(dB)", cex.lab = 1, line = 2.5)

axis(2,las = 1, at=seq(0, 1, 0.2),line = 0.5,
     labels=seq(0, 1, 0.2), col = 1, col.axis = 1,
     cex.axis = 1, hadj = 1)

abline (v=seq(1.5, 37.5, 5), col="grey88", lwd = 0.5, lty = 1)

axis(1, at = seq(1.5, 37.5, 5) , labels = seq(0, 36, 5),
     cex.axis = 1, line = 0, padj = 0)

for (t in Test){
  test<- t
  
  h<- hist(Th[[test]],plot = F, breaks = 36)
  ec <- ecdf(Th[[test]])
  points(h$mids+ifelse(test == "ZF", 1.25, 1),ec(h$mids), type = "l",lty = 1,lwd = 4,
         col = ifelse(test == "SS","lightslateblue",ifelse(test  == "ZS", "midnightblue", ifelse(test == "SF", "limegreen", "firebrick1"))))
}
abline (h= 0.5,col="grey88", lwd = 0.5, lty = 1)
axis(2,las = 1, at=0.5,line = 0.5, labels=0.5, col = 1, 
     col.axis = 1, cex.axis = 1, hadj = -0.99, tick = F )


dev.off()
