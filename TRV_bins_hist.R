library(csvread)
library(pdftools)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")

Label<- c("0-15","16-21", "22-25", "30-33")
Loc<- c(7.5,18.5, 23.5, 31.5)
pdf("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", h = 45, w= 45)

layout(matrix (c( c(21:25), c(1:5), c(6:10), c(11:15),
                  c(16:20)), ncol = 5, byrow = F)
       ,heights = c(0.35,rep(1, 4)), widths = c(0.40, rep(1, 4)))

par(mar = c(15, 10, 2, 2))
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
  plot(5, 5, type = "n", axes = F, xlab = "", ylab = "")
  mtext(ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "b) SITA Fast",
                                                        ifelse(test == "ZS", "c) ZATA Standard", "d) ZATA Fast"))), side = 3,  line = -10, cex =5)
  library(RColorBrewer)
  # Colour<- c("#e66101", "#fbd863", "#5e3c99", "#b2abd2")
  Colour<- c("cyan4", "cyan3", "brown4", "brown3")
  for(n in 1:4){
    # plot in step of 2 DB
    barplot(ifelse(n == 4, 1000,  160),Loc[[n]]/3,  width = 3,
            col = "white", border = "white",
            xlim = c(0, 36),axes = ifelse(test != "SS", F,  T),
            ylim = c(0, ifelse(n == 4, 1000,  160)), cex.axis = 5,
            xlab = " ", cex.lab = 5)
    
    s<- hist(x[[n]], breaks = seq(min(x[[n]]), max(x[[n]]), by = 1), cex.lab = 5,
             ,main= " ", border = "white", add = T, 
             xlim = c(0, 36), ylim = c(0, ifelse(n == 4, 1000,  160)),
             axes = F,
             xlab = " ", ylab = " " ,col = Colour[which(Test == test)])
    
    par(mgp=c(4,4,0))
    # axis(2, at= seq(0, ifelse(n !=4, 160,  1000), by = ifelse(n !=4, 50,  200)),
    #      labels =seq(0, ifelse(n !=4, 160,  1000), by = ifelse(n !=4, 50,  200)),
    #      cex.axis = 2.5, las = 2)
    # axis(2, at= seq(0, ifelse(n !=4 & test == "SS", 160,
    #                           ifelse(n ==4 & test == "SS", 1000, 0)),
    #                 by = ifelse(n !=4, 10,
    #                             ifelse(n ==4 & test == "SS",100, 0))),
    #      labels =seq(0, ifelse(n !=4 & test == "SS", 160,
    #                            ifelse(n ==4 & test == "SS", 1000, 0)),
    #                  by = ifelse(n !=4, 100,
    #                              ifelse(n ==4 & test == "SS",100), 0)),
    #      cex.axis = 2.5, las = 2)
    axis(1, at= seq(0, 35, by =5), labels = seq(0, 35, by =5),line = 1,
         cex.axis = ifelse(n == 4, 5, 0.01), lwd = 1)
    
    # text(which(s$counts > 160),150,s$counts[which(s$counts > 160)],cex =5)
    # box()
    
    abline (h=seq(from=10, to=1000, by= 10), col="white", lwd = 1)
    points(which(s$counts > ifelse(n == 4, 1000,  160))-0.5,
           rep(ifelse(n == 4, 940,  151), 
               length(which(s$counts > ifelse(n == 4, 1000,  160)))), 
           pch = 17, cex = 17)
    
    mtext(paste("n = ", length(x[[n]]), "     ", sep = " "), side = 3, 
          line = -15, cex = 5)
  }
  
  
  
}

plot(5,5 , type = "n", xlab = " ", ylab = " ", axes = F)
text(5, 4 , "Bin", cex = 7)


for (n in 1:4){
  plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
  text(5, 5 , paste( Label[n]," dB", sep = " "), cex = 5, srt = 90)
  
  
  
}
mtext("                                                                                                                                         Frequency",
      side = 2, cex = 5)
mtext("                                                                                                                                                                                                   Threshold (dB)", 
      side = 1, cex = 5, line = 12)


dev.off()
pdf_convert(paste("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.pdf", sep = ""), format = "jpeg")

