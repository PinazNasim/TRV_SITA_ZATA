library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

pdf("C://Users//DELL//OneDrive//Documents//polygon_and_ecdf.pdf", h = 10, w = 10)

layout(matrix(c(1:2), nrow = 2, ncol = 1), heights = c(1,0.75))
par(mar = c(0, 7, 0, 1))

plot(1, 5, main = " ", 
     xlim = c(2.75, 36.2), ylim = c(0, 2120),
     xlab = " ", axes = F,
     ylab= "Frequency Distribution", type = "n")

axis(2, at = seq(0, 2100, by = 210),line = 0.5,labels = seq(0, 2100, by = 210),
     las = 1,cex.axis = 0.75)
abline (h=seq(from=0, to=2200, by= 220),v=seq(1.5, 37.5, 4), col="lightgrey",
        lwd = 2)


x<- c(1:77)
Test<- c("SS", "SF", "ZS", "ZF")
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
           ifelse(T == "SS",1, ifelse(T == "ZS", 2, ifelse(T == "SF", 3, 4))))
}
legend(2.5,1600 , legend = c("SITA Standard", "ZATA Standard", "SITA Fast", "Zata Fast (shifted by 0.5 dB)")
       , pch = c(rep(16, 4)), yjust = 0.5, bg = "white", box.col = "white",
                 col =  c(1, 2, 3, 4), cex = 2)

par(mar = c(5, 7, 0, 1))
plot(1, 5, main = " ", 
     xlim = c(2.75, 36.2), ylim = c(0, 1.08),
     xlab = "Th(dB)", axes = F,
     ylab= "Cumulative Frequency Distribution", type = "n")


axis(2,las = 1, at=seq(0, 1, 0.2),line = 0.5, labels=seq(0, 1, 0.2), col = 1, col.axis = 1, cex.axis = 0.75)

abline (h=seq(from=0, to=1, by= 0.2),v=seq(1.5, 37.5, 4), col="lightgrey", lwd = 2)
axis(1, at = seq(1.5, 37.5, 4) , labels = seq(0, 36, 4), cex.axis = 0.75, line = -0.5)

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
         col = ifelse(T == "SS",1, ifelse(T == "ZS", 2, ifelse(T == "SF", 3, 4))),
         type =  "l", lwd = 4)
}



dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//polygon_and_ecdf.pdf", format = "jpeg")