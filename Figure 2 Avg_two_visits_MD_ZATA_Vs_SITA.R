# Calling data
library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")

SS<- (DATA$SS_Day1_md + DATA$SS_Day2_md)/2
ZS<- (DATA$ZS_Day1_md + DATA$ZS_Day2_md)/2

SF<- (DATA$SF_Day1_md + DATA$SF_Day2_md)/2
ZF<- (DATA$ZF_Day1_md + DATA$ZF_Day2_md)/2

# Plotting data
png(paste("C://Users//DELL//OneDrive//Documents//md_SITA_vs_ZATA.png", sep = "")
    ,h= 1150,w= 2350, units = "px", pointsize = 12, res = 300)
par(mfrow = c(1, 2))
par(mar = c(4,4,2,2 ))

# First plot ZS vs SS
plot(c(-38:7), c(-38:7), type = "l",lty = 2 , xlab = " "
     , ylab = " " , main = " a) ZATA Standard vs. SITA Standard "
     , lwd = 2, xlim = c(-34, 3.5), ylim = c(-34, 3.5), cex.main = 1, axes = F)

title(xlab = "Mean MD of SITA Standard (dB)",cex.lab = 1,line = 2.5)
title(ylab = "Mean MD of ZATA Standard (dB)", cex.lab = 1,line = 2.5)

axis(1, at = seq(5,-35,-5), labels = seq(5,-35,-5),cex.axis= 1)
axis(2,las = 1, at = seq(5,-35,-5), labels = seq(5,-35,-5),
     cex.axis= 1, hadj = 1)
box()
points(SS, ZS,pch= ifelse(DATA$Category == "Normal", 16, 17),
       cex = 1, 
       col = ifelse(DATA$Category == "Normal", "midnightblue", "brown3"))
Standard<- cor.test(SS, ZS, method=("spearman"))
text(-5,-30, paste("rho =",round(Standard$estimate, digits = 2), sep = " "), cex = 1)
legend("topleft", c( "Normal", "Glaucoma"), bty = "n",
       col = c ("midnightblue", "brown3"), pch = c(16, 17), 
       cex = 1)

# Second plot ZF vs SF
plot(c(-38:7), c(-38:7),type = "l", lty = 2,  xlab = " "
     , ylab = " " , main = " b) ZATA Fast vs. SITA Fast ",
     lwd = 2, xlim = c(-34, 3.5), ylim = c(-34, 3.5), 
     cex.main = 1, axes = F)

title(xlab = "Mean MD of SITA Fast (dB)",cex.lab = 1, line = 2.5)
title(ylab = "Mean MD of ZATA Fast (dB)", cex.lab = 1,line = 2.5)

axis(1, at = seq(5,-35,-5), labels = seq(5,-35,-5),cex.axis= 1)
axis(2,las = 1, at = seq(5,-35,-5), labels = seq(5,-35,-5),
     cex.axis= 1, padj = 0, hadj = 1)
box()
points(SF, ZF ,pch= ifelse(DATA$Category == "Normal", 16, 17),
       cex = 1, 
       col = ifelse(DATA$Category == "Normal", "midnightblue", "brown3"))
Fast<- cor.test(SF, ZF, method=("spearman"))
text(-5,-30, paste("rho =", round(Fast$estimate, digits = 2), sep = " "), cex = 1)
legend("topleft", c( "Normal", "Glaucoma"), bty = "n",
       col = c ("midnightblue", "brown3"), pch = c(16, 17), 
       cex = 1)

box()
dev.off()
