library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")

pdf(paste("C://Users//DELL//OneDrive//Documents//duration_Vs_md.pdf", sep = ""),h= 20,w= 22)
par(mfrow = c(2, 2))
par(mar = c(10, 15,5, 5))
Test_names<- c("SS", "ZS", "SF", "ZF")
for (name in Test_names){
  test = name
  
  Test<- (as.numeric(unlist(DATA[paste(test, "_Day1_md", sep = "")]+ DATA[paste(test, "_Day2_md", sep = "")])))/2
  Duration<- (as.numeric(unlist(DATA[paste(test, "_Duration_day1", sep = "")]+ DATA[paste(test, "_Duration_day2", sep = "")])))/2
  
  
  plot(1, 600, type = "l", xlab = " ", 
        ylab = " " , main = ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                                                                    ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
       lty = 1, lwd = 2, xlim = c(-35, 5), ylim = c(80,700), 
       cex.main = 3, cex.lab = 2.5, axes = F)
  axis(1, at = seq(5,-35,-10), labels = seq(5,-35,-10),cex.axis= 3,
       padj = 1)
  axis(2, at = seq(0,700,100), labels = seq(0,700,100),cex.axis= 3,
       padj = 1, hadj = 1, las = 1)
  mtext(ifelse(test == "SS", " Mean test duration of visit1 & 2 (Secs)                                                                            ", " "),
        side = 2, cex = 3, line = 8)
  
  points(Test, Duration ,pch = ifelse(DATA$Category == "Normal", 16, 17), cex= 3, col = ifelse(DATA$Category == "Normal", "orangered", "midnightblue"))
  
  xx <-  seq(-35, 0, length=77)
  
  fit <- loess(Duration ~ Test, span = 0.5) 
  pred <- predict(fit, xx, se=TRUE)
  lines(xx, pred$fit, lty= 1, col= 1, lwd=5)
  lines(xx, pred$fit - qt(0.975,pred$df)*pred$se , lty= 2, col= "red", lwd=5)
  lines(xx, pred$fit + qt(0.975,pred$df)*pred$se , lty= 2, col= "red", lwd=5)
  
  
  
  legend("topright", c("95 % CI", "Mean"), col = c("red",1), lty = c(2, 1), cex = 3, bty = "n", lwd = 5)
  
  Standard<- cor.test(Test, Duration, method=("spearman"))
  text(-25,103, paste("rho =",round(Standard$estimate, digits = 2), sep = " "), cex = 3)
  legend("topleft", c( "Normal", "Glaucoma"), col = c ("orangered", "midnightblue"),bty = "n" , pch = c(16, 17), cex = 3 )
  box()
}
mtext("Mean MD of visit1 and 2 (dB)                                                                              ",
      side = 1, line = 7, cex = 3 )
dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//duration_Vs_md.pdf", format = "jpeg")

