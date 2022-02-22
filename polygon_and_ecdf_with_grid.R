library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

png("C://Users//DELL//OneDrive//Documents//polygon_and_ecdf.png",
    h = 2500, w = 2500)

layout(matrix(c(1:2), nrow = 2, ncol = 1), heights = c(1,0.75))
par(mar = c(0, 25, 1, 1))

plot(1, 5, main = " ", 
     xlim = c(2.75, 35.5), ylim = c(0, 2120),
     xlab = " ", axes = F,
     ylab= " ", type = "n")
title(ylab= "Frequency Distribution", cex.lab = 7, line = 18)
axis(2, at = seq(0, 2100, by = 210),line = 0.5,labels = seq(0, 2100, by = 210),
     las = 1,cex.axis = 7)
# abline (h=seq(from=0, to=2200, by= 220),v=seq(1.5, 37.5, 5), col="grey88",
#         lwd = 2, lty = 2)
abline (v=seq(1.5, 37.5, 5), col="grey88",lwd = 2, lty = 2)
abline (h= 2200 , col="grey88",lwd = 2, lty = 2)


x<- c(1:77)
Test<- c("ZS", "ZF", "SF","SS")
for (test in Test){
  T<- test
  
  D1_th<- paste(T, "_th_Day1_L" , c(1:54), sep = "")
  D2_th<- paste(T, "_th_Day2_L" , c(1:54), sep = "")
  
  th1= as.numeric(unlist(DATA[x , D1_th]))
  th2= as.numeric(unlist(DATA[x , D2_th]))
  th<- c(th1, th2)
  th<- th[which(th<36)]
  
  h<- hist(th,plot = F, breaks = seq(0, 36, by = 1))
  points(h$mids + ifelse(T == "ZF", 1.25, 1) ,h$counts,h$counts,
         type = "l", lwd = 4, lty = 1, col =
           ifelse(T == "SS","lightslateblue", ifelse(T == "ZS", "midnightblue", ifelse(T == "SF", "limegreen", "firebrick1"))))
}
legend(2.5,1825, legend = c(" ",  " ", "","")
       , lty = c(rep(1, 4)), lwd = 8, yjust = 0.5,
       bty = "n" ,bg = "white", box.col = "white",
       col =  c("lightslateblue", "limegreen", "midnightblue","firebrick1"),
       cex = 7)
text( 11,2070 ,"SITA Standard",cex= 5)
text( 10,1900 ,"SITA Fast",cex= 5)
text( 11.2,1740 ,"ZATA Standard",cex= 5)
text( 14.3,1580 ,"ZATA Fast (shifted by 0.5 dB)",cex= 5)

par(mar = c(20, 25, 0, 1))
plot(1, 5, main = " ", 
     xlim = c(2.75, 36), ylim = c(0.02, 1.08),
     xlab = " ", axes = F,
     ylab= " ", type = "n")

title(ylab= "Cumulative Distribution      ", cex.lab = 7,
      line = 18)
title(xlab= "Threshold(dB)", cex.lab = 7, line = 15)

axis(2,las = 1, at=seq(0, 1, 0.2),line = 0.5,
     labels=seq(0, 1, 0.2), col = 1, col.axis = 1,
     cex.axis = 7, hadj = 1)

abline (v=seq(1.5, 37.5, 5), col="grey88", lwd = 2, lty = 1)
# abline (h=seq(from=0.02, to=1, by= 0.2),v=seq(1.5, 37.5, 5), col="grey88", lwd = 2, lty = 2)
axis(1, at = seq(1.5, 37.5, 5) , labels = seq(0, 36, 5),
     cex.axis = 7, line = -0.5, padj = 1)

for (test in Test){
  T<- test
  
  D1_th<- paste(T, "_th_Day1_L" , c(1:54), sep = "")
  D2_th<- paste(T, "_th_Day2_L" , c(1:54), sep = "")
  
  th1= as.numeric(unlist(DATA[x , D1_th]))
  th2= as.numeric(unlist(DATA[x , D2_th]))
  th<- c(th1, th2)
  th<- th[which(th<36)]
  h<- hist(th,plot = F, breaks = seq(0, 36, by = 1))
  ec <- ecdf(th)
  points(h$mids+ ifelse(T == "ZF", 1.25, 1), ec(h$mids), 
         col = ifelse(T == "SS","lightslateblue", ifelse(T == "ZS", "midnightblue", ifelse(T == "SF", "limegreen", "firebrick1"))),
         type =  "l", lwd = 4)
}
abline (h= 0.5,col="grey88", lwd = 2, lty = 2)
axis(2,las = 1, at=0.5,line = 0.5, labels=0.5, col = 1, 
     col.axis = 1, cex.axis = 7, hadj = -0.5, tick = F )


dev.off()
# library(pdftools)
# pdf_convert("C://Users//DELL//OneDrive//Documents//polygon_and_ecdf.pdf", format = "jpeg")