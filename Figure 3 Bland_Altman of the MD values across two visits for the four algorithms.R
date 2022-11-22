# Calling the required libraries
library(csvread)
# Opening the data file
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
# Mining the required data from the data file
md_D1<- vector("list", 4)
md_D2<- vector("list", 4)
Test_names<- c("SS", "ZS", "SF", "ZF")

for (name in Test_names){
  test = name
  
  md_D1[[test]]<- unlist(DATA[paste(test, "_Day1_md",sep="")])
  md_D2[[test]]<- unlist(DATA[paste(test, "_Day2_md",sep="")])
}

md_D1[["SS"]]<- as.numeric(unlist(md_D1$SS))
md_D1[["ZS"]]<- as.numeric(unlist(md_D1$ZS))
md_D1[["SF"]]<- as.numeric(unlist(md_D1$SF))
md_D1[["ZF"]]<- as.numeric(unlist(md_D1$ZF))
md_D2[["SS"]]<- as.numeric(unlist(md_D2$SS))
md_D2[["ZS"]]<- as.numeric(unlist(md_D2$ZS))
md_D2[["SF"]]<- as.numeric(unlist(md_D2$SF))
md_D2[["ZF"]]<- as.numeric(unlist(md_D2$ZF))

# plotting BlandAltman from the mined data

png("C://Users//DELL//OneDrive//Documents//Bland_Altman_MD _SD.png",
    w= 2300,h=2000, res = 300)
# layout(matrix(c(5, 5, 1, 2, 3, 4), byrow = T, ncol = 2, nrow = 3), heights = c(0.25, 1, 1),
#        widths = c(1, 1))
# par(mar = c(10, 10, 5, 5))
par(mfrow = c(2, 2))
# layout(matrix(c(1, 2, 3, 4), byrow = T, ncol = 2, nrow = 2), heights = c(1, 1),
#        widths = c(1, 1))
par(mar = c(4, 4.5,2, 0.2))

for (name in Test_names){
  test = name
  
  
  Diff<- (md_D1[[test]] - md_D2[[test]])
  Mean<- (md_D1[[test]] + md_D2[[test]])/2
  SD<- 1.96*sd(Diff)
  
  Upper_lm <- (mean(Diff))+SD
  Lower_lm <- (mean(Diff))-SD
  
  
  plot(1,1, xlim=c(-33.6, 3.6), ylim = c(-14, 14), 
       # main= ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
       #                                                       ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
       xlab=" " ,ylab=" ", axes = F, cex.main = 1, type = 'n')
  
  legend("topleft", bty = "n", legend = c("Healthy", "Glaucoma"), 
         col = c ("#e66101", "#5e3c99"),pch = c(16, 17), cex = 1)
  polygon(c(10, -40, -40, 10), 
          c(Upper_lm, Upper_lm, Lower_lm, Lower_lm),
          col = "grey82", border = "grey82")
  
  # text(3, Upper_lm+1, round(Upper_lm, 2), cex = 1.5)
  # text(3, Lower_lm-1, round(Lower_lm, 2), cex = 1.5)
  # text(3, mean(Diff)+1, round(mean(Diff), 2), cex = 1.5)
  # text(4, mean(Diff), round(SD, 2), cex = 1.5)
  mtext(ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                              ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))), side = 3, line = 0.8)
  points(Mean,Diff, xlim=c(-33.6, 3.6), ylim = c(-14, 14),
         panel.last = abline(h = mean(Diff), lty = 1, col = "grey", lwd = 1),
         pch = ifelse(DATA$Category == "Normal", 16, 17),
         main= ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                               ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
         xlab=" " ,ylab=" ", axes = F,
         type = 'p', col = ifelse(DATA$Category == "Normal", "#e66101", "#5e3c99"), cex=1)
  
  axis(1, at =  seq(5, -35, -5), labels =  seq(5, -35, -5),
       cex.axis = 1, padj = 0.8, tck = -0.03, mgp=c(5, .3, 0))
  axis(2, at = seq(15, -15, -5), labels = seq(15, -15, -5),
       cex.axis = 1, hadj = 1, las = 1, tck = -0.03, mgp=c(5, 1, 0))
  mtext(text = ifelse(test == "SS", " MD visit 1 - MD visit 2  (dB)                                                                  ",
                      " "), line = 3, side = 2, cex = 1.2)
  
  mtext(text= ifelse(test == "ZF", "Mean MD of visit 1 & 2 (dB)                                                                 "," "),
        line = 2.5, cex = 1.2, side = 1)
  
  box(col = "black", lwd = 1)
  
}
# par(mar = c(0, 0, 0, 0))
# plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = F)
# legend(3, 6.5, bty = "n", legend = c("Healthy", "Glaucoma"), 
#        col = c ("midnightblue", "brown3"),pch = c(16, 17), cex = 3)
# 

dev.off()

