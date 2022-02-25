# Extracting data
library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")

# plots


png(paste("C://Users//DELL//OneDrive//Documents//duration_Vs_md.png", sep = "")
    ,h= 2000,w= 2200, res = 300)
par(mfrow = c(2, 2))
par(mar = c(5, 5,2, 2))
Test_names<- c("SS", "ZS", "SF", "ZF")
for (name in Test_names){
  test = name
  T1<- DATA[paste(test, "_Day1_md", sep = "")]
  T2<- DATA[paste(test, "_Day2_md", sep = "")]
  Test<- rowMeans(data.frame(T1, T2))
  D1<- DATA[paste(test, "_Duration_day1", sep = "")]
  D2<- DATA[paste(test, "_Duration_day2", sep = "")]
  Duration<- rowMeans(data.frame(D1, D2))
  
  plot(1, 600, type = "l", xlab = " ", 
       ylab = " " , main = ifelse(test == "SS", "a) SITA Standard", 
                                  ifelse(test == "SF", "c) SITA Fast",
                                         
                                         ifelse(test == "ZS", "b) ZATA Standard"
                                                , "d) ZATA Fast"))),
       lty = 1, lwd = 2, xlim = c(-35, 5), ylim = c(80,800), 
       cex.main = 1,  axes = F)
  axis(1, at = seq(5,-35,-10), labels = seq(5,-35,-10),cex.axis= 1,
       padj = 0)
  axis(2, at = seq(0,800,100), labels = seq(0,800,100),cex.axis= 1,
       padj = 0, hadj = 1, las = 1)
  mtext(ifelse(test == "SS", " Mean test duration of visit1 & 2 (Secs)                                                                             ", " "),
        side = 2, cex = 1, line =3)
  
  points(Test, Duration ,pch = ifelse(DATA$Category == "Normal", 16, 17),
         cex= 1, col = ifelse(DATA$Category == "Normal", "midnightblue", "brown3"))
  
  xx <-  seq(-35, 0, length=77)
  
  fit <- loess(Duration ~ Test, span = 0.5) 
  pred <- predict(fit, xx, se=TRUE)
  lines(xx, pred$fit, lty= 1, col= 1, lwd = 1)
  lines(xx, pred$fit - qt(0.975,pred$df)*pred$se , lty= 2, col= "red", lwd = 1)
  lines(xx, pred$fit + qt(0.975,pred$df)*pred$se , lty= 2, col= "red", lwd = 1)
  
  
  
  legend("topright", c("95 % CI", "Mean"), col = c("red",1), lty = c(2, 1),
         cex = 1, bty = "n", lwd = 1)
  
  Standard<- cor.test(Test, Duration, method=("spearman"))
  text(-25,103, paste("rho =",round(Standard$estimate, digits = 2), sep = " ")
       , cex = 1)
  legend("topleft", c( "Normal", "Glaucoma"), 
         col = c ("midnightblue", "brown3"),bty = "n" ,
         pch = c(16, 17), cex = 1 )
  box()
}
mtext("Mean MD of visit1 and 2 (dB)                                                                       ",
      side = 1, line = 3, cex = 1 )
dev.off()