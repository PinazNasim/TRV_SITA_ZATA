# Calling data
library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")

SS<- (DATA$SS_Day1_md + DATA$SS_Day2_md)/2
ZS<- (DATA$ZS_Day1_md + DATA$ZS_Day2_md)/2

SF<- (DATA$SF_Day1_md + DATA$SF_Day2_md)/2
ZF<- (DATA$ZF_Day1_md + DATA$ZF_Day2_md)/2

# SS<- DATA$SS_Day2_md 
# ZS<- DATA$ZS_Day2_md 
# 
# SF<- DATA$SF_Day2_md 
# ZF<- DATA$ZF_Day2_md 

# Plotting data
png(paste("C://Users//DELL//OneDrive//Documents//md_SITA_vs_ZATA.png", sep = "")
    ,h= 1500,w= 3200, units = "px", pointsize = 12, res = 300)
par(mfrow = c(1, 2))
par(mar = c(4,4.5,2,2 ))

# First plot ZS vs SS
plot(c(-38:7), c(-38:7), type = "l",lty = 2 , xlab = " "
     , ylab = " " , 
     # main = " (a) ",
     lwd = 2, xlim = c(-33.6, 4), ylim = c(-33.6, 4), cex.main = 1, axes = F)
mtext("(a)", side = 3, line = 1)
title(xlab = "Mean Deviation, SITA Standard (dB)",cex.lab = 1.2,line = 2.5)
title(ylab = "Mean Deviation, ZATA Standard (dB)", cex.lab = 1.2,line = 3)

axis(1, at = seq(5,-35, by = -5), labels = seq(5,-35, by = -5),cex.axis= 1)
axis(2,las = 1, at = seq(5,-35, by = -5), labels = seq(5,-35, by = -5),
     cex.axis= 1, hadj = 1)
box()
points(SS, ZS,pch= ifelse(DATA$Category == "Normal", 16, 17),
       cex = 1.5, 
       col = ifelse(DATA$Category == "Normal", "#e66101", "#5e3c99"))
Standard<- cor.test(SS, ZS, method=("spearman"))
text(-5,-30, paste("rho =",round(Standard$estimate, digits = 2), sep = " "), cex = 1)
legend("topleft", c( "Healthy", "Glaucoma"), bty = "n",
       col = c ("#e66101", "#5e3c99"), pch = c(16, 17), 
       cex = 1.5)

# Second plot ZF vs SF
plot(c(-38:7), c(-38:7),type = "l", lty = 2,  xlab = " "
     , ylab = " " 
     # , main = " (b) "
     ,lwd = 2, xlim = c(-33.6, 4), ylim = c(-33.6, 4), 
     cex.main = 1, axes = F)
mtext("(b)", side = 3, line = 1)
title(xlab = "Mean Deviation, SITA Fast (dB)",cex.lab = 1.2, line = 2.5)
title(ylab = "Mean Deviation, ZATA Fast (dB)", cex.lab = 1.2,line = 3)

axis(1, at = seq(5,-35, by = -5), labels = seq(5,-35, by = -5),cex.axis= 1)
axis(2,las = 1, at = seq(5,-35, by = -5), labels = seq(5,-35, by = -5),
     cex.axis= 1, padj = 0.5, hadj = 1)
box()
points(SF, ZF ,pch= ifelse(DATA$Category == "Normal", 16, 17),
       cex = 1.5, 
       col = ifelse(DATA$Category == "Normal", "#e66101", "#5e3c99"))
Fast<- cor.test(SF, ZF, method=("spearman"))
text(-5,-30, paste("rho =", round(Fast$estimate, digits = 2), sep = " "), cex = 1)
legend("topleft", c( "Healthy", "Glaucoma"), bty = "n",
       col = c ("#e66101", "#5e3c99"), pch = c(16, 17), 
       cex = 1.5)

box()
dev.off()
