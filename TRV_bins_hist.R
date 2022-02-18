library(csvread)
library(pdftools)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

Label<- c("0-15","16-21", "22-25", "30-33")
Loc<- c(7.5,18.5, 23.5, 31.5)
pdf("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", h = 45, w= 45)

layout(matrix (c( c(21:25), c(1:5), c(6:10), c(11:15),
                  c(16:20)), ncol = 5, byrow = F)
       ,heights = c(0.35,rep(0.75, 3), 1), widths = c(0.75, rep(1, 4)))


Test<- c("SS", "SF",  "ZS", "ZF")
for (t in Test){
  test = t
  
  x <- vector("list",4)
  Low <- vector("list",4)
  Up <- vector("list",4)
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")): which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  D1<- c(D1, D2)
  D2<- c(D2, D1)
  for(participant in 1:77){
    for(threshold in 0:33){
      th = threshold
      
      
      if (th >= 0 & th <=15){
        i = 1
      }else if (th >= 16  & th <= 21){
        i = 2
      }else if (th >= 22  & th <=25){
        i = 3 
      }else {
        i = 4
      }
      
      
      location <- which(DATA[participant,D1]==th)
      x[[i]] <- c(x[[i]],as.numeric(DATA[participant,D2[location]]))
      Low[[i]]<- quantile(x[[i]], 0.05)
      Up[[i]]<- quantile(x[[i]],0.95)
      
    }
  }
  par(mar = c(5, 5, 5,5))
  plot(5, 5, type = "n", axes = F, xlab = "", ylab = "")
  mtext(ifelse(test == "SS", "a) SITA Standard  ", ifelse(test == "SF", "b) SITA Fast  ",
                                                          ifelse(test == "ZS", "c) ZATA Standard  ", "d) ZATA Fast    "))), side = 3,  line = -10, cex =6)
  library(RColorBrewer)
  
  Colour<- c("cyan4", "cyan3", "orangered3", "orangered")
  
  for(n in 1:4){
    # plot in step of 2 DB
    ifelse(n == 4,par(mar = c(28, 0, 0, 5)) , par(mar = c(5, 0, 0, 5)))
    
    barplot(ifelse(n == 4, 1000,  160),Loc[[n]]/3,  width = 3,
            col = "white", border = "white", xlim = c(0, 36),
            axes = F,
            ylim = c(0, ifelse(n == 4, 1000,  160)), 
            xlab = " ", cex.lab = 5)
    axis(2,at = seq(0, ifelse(n == 4, 1000,  160), ifelse(n == 4, 200,  50)),
         labels = seq(0, ifelse(n == 4, 1000,  160), ifelse(n == 4, 200,  50)),
         cex.axis = 7,las = 1,hadj = 1.3,
         col.axis = ifelse(test != "SS", "white",  "black"),
         col = ifelse(test != "SS", "white",  "black"))
    
    s<- hist(x[[n]], breaks = seq(min(x[[n]]), max(x[[n]]), by = 1), cex.lab = 5,
             ,main= " ", border = "white", add = TRUE, 
             xlim = c(0, 36), ylim = c(0, ifelse(n == 4, 1000,  160)),
             axes = F,
             xlab = " ", ylab = " " ,col = Colour[which(Test == test)])
    
    
    
    axis(1, at= seq(0, 35, by =7), labels = seq(0, 35, by =7),line = 1,
         cex.axis = ifelse(n == 4, 7, 0.001), 
         lwd = ifelse(n == 4, 2, 0.001), 
         col= ifelse(n == 4, "black", "white") 
         ,padj = 2)
    points(ifelse(n == 1, which(s$counts > 160)- 0.35, 1),150, pch = 17,
           col = ifelse(n == 1, "black","white" ),cex = 15)
    
    text(ifelse(n == 1, which(s$counts > 160)+7, 1),150,
         ifelse(n == 1, s$counts[which(s$counts > 160)]," " ),
         col = "black", cex =6.5)
    points(ifelse(n == 4 & test == "ZS", 
                  which(s$counts > 1000)- 0.35, 1),925, pch = 17,
           col = ifelse(n == 4 & test == "ZS", "black","white" ),cex = 15)
    text(ifelse(n == 4 & test == "ZS", 
                which(s$counts > 1000)- 8, 1),925,
         ifelse(n == 4 & test == "ZS", s$counts[which(s$counts > 1000)]," " ),
         col = "black", cex =6.5)
    points(ifelse(n == 4 & test == "ZF", 
                  which(s$counts > 1000)- 0.35, 1),925, pch = 17,
           col = ifelse(n == 4 & test == "ZF", "black","white" ),cex = 15)
    text(ifelse(n == 4 & test == "ZF", 
                which(s$counts > 1000)- 8, 1),925,
         ifelse(n == 4 & test == "ZF", s$counts[which(s$counts > 1000)]," " ),
         col = "black", cex =6.5)
    
    abline (h=seq(from=0, to=1000, by= 50), col="grey94", lwd = 1)
    
    mtext(paste("n = ", length(x[[n]]), "     ", sep = " "), side = 3, 
          line = -24, cex = 6)
  }
  
}
mtext("Threshold (dB)                                                                                         ", 
      side = 1, cex = 6.5, line = 23)


par(mar = c(0, 5, 5,  0))
plot(5,5 , type = "n", xlab = " ", ylab = " ", axes = F)
text(5, 6, "Bin", cex = 9)


for (n in 1:4){
  par(mar = c(15, 15, 5, 15))
  plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
  text(5, 5 , paste( Label[n]," dB", sep = " "), cex = 9, srt = 90)
  

}
mtext("                                                                                   Frequency Distribution",
      side = 2, cex = 8, line = 5)


dev.off()
pdf_convert(paste("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", sep = ""),
            format = "jpeg")

