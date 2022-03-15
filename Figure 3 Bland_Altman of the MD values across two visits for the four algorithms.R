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
    w=3500,h=3500, res = 300)
layout(matrix(c(1, 2, 3, 4, 5, 5), byrow = T, ncol = 2, nrow = 3), heights = c(1, 1, 0.25),
       widths = c(1, 1))
par(mar = c(5, 8, 5, 2))


for (name in Test_names){
  test = name
  
  
  Diff<- (md_D1[[test]] - md_D2[[test]])
  Mean<- (md_D1[[test]] + md_D2[[test]])/2
  SD<- 1.96*sd(Diff)
  
  Upper_lm <- mean(Diff + (SD)) 
  Lower_lm <- mean(Diff - (SD))
  
  plot(1,1, xlim=c(-38, 5), ylim = c(-15, 15), 
       main= ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                             ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
       xlab=" " ,ylab=" ", axes = F, cex.main = 3, type = 'n')
  
  polygon(c(10, -40, -40, 10), 
          c(Upper_lm, Upper_lm, Lower_lm, Lower_lm),
          col = "light blue")
  
  
  points(Mean,Diff, xlim=c(-38, 5), ylim = c(-15, 15),
         panel.last = abline(h = mean(Diff), lty = 1, col = "black", lwd = 1),
         pch = ifelse(DATA$Category == "Normal", 16, 17),
         main= ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                               ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
         xlab=" " ,ylab=" ", axes = F,
         type = 'p', col = ifelse(DATA$Category == "Normal", "midnightblue", "brown3"), cex=3)
  
  axis(1, at =  c(5, 0, -5, -15, -25, -35), labels =  c(5, 0, -5, -15, -25, -35),
       cex.axis = 2, padj = 0.8)
  axis(2, at = seq(15, -15, -5), labels = seq(15, -15, -5),
       cex.axis = 2, hadj = 1, las = 1)
  mtext(text = ifelse(test == "SS", " MD visit 1 - MD visit 2  (dB)                                                    ",
                      " "), line = 6, side = 2, cex = 2)
  
  mtext(text= ifelse(test == "ZF", "Mean MD of visit 1 & 2 (dB)                                                       "," "),
        line = 6, cex = 2, side = 1)
  
  
  
  box(col = "black", lwd = 1)
  
}
par(mar = c(0, 0, 0, 0))
plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = F)
legend(3, 6.5, bty = "n", legend = c("Healthy", "Glaucoma"), 
       col = c ("midnightblue", "brown3"),pch = c(16, 17), cex = 3)


dev.off()
