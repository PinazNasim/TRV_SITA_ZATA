library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")

pdf(paste("C://Users//DELL//OneDrive//Documents//md_SITA_vs_ZATA.pdf", sep = ""),h= 10,w= 20)
par(mfrow = c(1, 2))


SS<- (DATA$SS_Day1_md + DATA$SS_Day2_md)/2
ZS<- (DATA$ZS_Day1_md + DATA$ZS_Day2_md)/2
par(mar = c(10, 8, 7,5 ))
plot(c(-38:7), c(-38:7), type = "l",lty = 2 , xlab = " "
     , ylab = " " , main = " a) ZATA Standard vs. SITA Standard ",
     , lwd = 2, xlim = c(-34, 3.5), ylim = c(-34, 3.5), cex.main = 2.5, axes = F)

title(xlab = "Mean MD of SITA Standard (dB)",cex.lab = 2,line = 5)
title(ylab = "Mean MD of ZATA Standard (dB)", cex.lab = 2,line = 5)

axis(1, at = seq(5,-35,-10), labels = seq(5,-35,-10),cex.axis= 2,
     padj = 1)
axis(2,las = 1, at = seq(5,-35,-10), labels = seq(5,-35,-10),
     cex.axis= 2, padj = 0)
box()
points(SS, ZS,pch= ifelse(DATA$Category == "Normal", 16, 17),
       cex = ifelse(DATA$Category == "Normal", 3, 3), 
       col = ifelse(DATA$Category == "Normal", "orangered", "midnightblue"))
Standard<- cor.test(SS, ZS, method=("spearman"))
text(0,-30, paste("rho =",round(Standard$estimate, digits = 2), sep = " "), cex = 1.5)
legend("topleft", c( " ", " "), bty = "n",
       col = c ("orangered", "midnightblue"), pch = c(16, 17), cex = 2.5 )
text ( -29, 2, "Normal", cex = 2)
text (-28, -1, "Glaucoma",cex = 2 )
SF<- (DATA$SF_Day1_md + DATA$SF_Day2_md)/2
ZF<- (DATA$ZF_Day1_md + DATA$ZF_Day2_md)/2
par(mar = c(10, 8, 7,5 ))
plot(c(-38:7), c(-38:7),type = "l", lty = 2,  xlab = " "
     , ylab = " " , main = " b) ZATA Fast vs. SITA Fast ",
     lwd = 2, xlim = c(-34, 3.5), ylim = c(-34, 3.5), 
     cex.main = 2.5, axes = F)

title(xlab = "Mean MD of SITA Fast (dB)",cex.lab = 2,line = 5)
title(ylab = "Mean MD of ZATA Fast (dB)", cex.lab = 2,line = 5)

axis(1, at = seq(5,-35,-10), labels = seq(5,-35,-10),cex.axis= 2,
     padj = 1)
axis(2,las = 1, at = seq(5,-35,-10), labels = seq(5,-35,-10),
     cex.axis= 2, padj = 0)
box()
points(SF, ZF ,pch= ifelse(DATA$Category == "Normal", 16, 17),
       cex = ifelse(DATA$Category == "Normal", 3, 3), 
       col = ifelse(DATA$Category == "Normal", "orangered", "midnightblue"))
Fast<- cor.test(SF, ZF, method=("spearman"))
text(0,-30, paste("rho =", round(Fast$estimate, digits = 2), sep = " "), cex = 1.5)
legend("topleft", c( " ", " "), bty = "n",
       col = c ("orangered", "midnightblue"), pch = c(16, 17), cex = 2.5 )
text ( -29, 2, "Normal", cex = 2)
text (-28, -1, "Glaucoma",cex = 2 )
box()
dev.off()
library(pdftools)
pdf_convert("C://Users//DELL//OneDrive//Documents//md_SITA_vs_ZATA.pdf", format = "jpeg")
