library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")

png(paste("C://Users//DELL//OneDrive//Documents//md_visit1_vs_visit2.png", sep = ""),
    ,h= 3000,w= 3000)

layout(matrix(c(5, 5,5, 1, 3, 6, 2,4, 6), ncol = 3,
              nrow = 3), widths = c(0.18, rep(1, 2)),
       heights = c(rep(1, 2), 0.3))
par(mar = c(5, 5, 30, 25))

Test_names<- c("SS", "ZS", "SF", "ZF")
for (name in Test_names){
  test = name
  
  Day1<- as.numeric(unlist(DATA[paste(test, "_Day1_md", sep = "")]))
  Day2<- as.numeric(unlist(DATA[paste(test, "_Day2_md", sep = "")]))
  plot(c(-37: 7), c(-37: 7), type = "l", 
       xlab =" ", ylab = "",axes = F, 
       main = ifelse(test == "SS", "a) SITA Standard", 
                     ifelse(test == "SF", "c) SITA Fast",
                            ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
       lty = 1, lwd = 2, 
       ylim = c(-33.5,4), xlim = c(-33.5,4), cex.main = 12)
  # title(ylab = " MD visit2 (dB)", line = 8, 
  #       xlab = " MD visit1 (dB)", 
  #       cex.lab = 4)
  axis(1, at = seq(5,-35,-10), labels = seq(5,-35,-10),cex.axis= 10,
       padj = 1, lwd = 1.5, col = "black",lwd.ticks = 2)
  axis(2,las = 1, at = seq(5,-35,-10), labels = seq(5,-35,-10)
       ,cex.axis= 10, padj = 0.5, hadj = 1.3, lwd = 1.5, 
       col = "black", lwd.ticks = 2)
  points(Day1, Day2 ,pch = ifelse(DATA$Category == "Normal", 16, 17), cex= 15, col = ifelse(DATA$Category == "Normal", "midnightblue", "brown3"))
  Standard<- cor.test(Day1, Day2, method="spearman")
  text(-27,2, paste("rho =",round(Standard$estimate, digits = 2), sep = " "), cex = 10)
  box(lwd = 2, col = "black")
  
}

par(mar = c(0, 0, 0, 2))
plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = F)
mtext("         MD visit 2 (dB)", side = 4, cex = 10,
      line = -15)
par(mar = c(0, 0, 0, 0))
plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = F)
mtext("                                            MD visit 1 (dB)                                                   ",
      side = 3, cex = 10, line= -20)
legend(3, 6, bty = "n", legend = c("Normal", "Glaucoma"), col = c ("midnightblue", "brown3"), 
       pch = c(16, 17), cex = 12)

dev.off()
# library(pdftools)
# pdf_convert("C://Users//DELL//OneDrive//Documents//md_visit1_vs_visit2.pdf", format = "jpeg")

