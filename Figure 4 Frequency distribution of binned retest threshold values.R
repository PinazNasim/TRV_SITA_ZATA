library(csvread)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//blindspots_removing_codes.R")
# Mining and preparing DATA to plot
Test<- c("SS","SF","ZS","ZF")
x <- vector("list",16)
for (t in Test){
  test = t
  D1<- ((which(colnames(DATA) == paste(test, "_th_Day1_L1", sep = "")): 
           which(colnames(DATA) == paste(test, "_th_Day1_L54", sep = ""))))
  D2<- ((which(colnames(DATA) == paste(test, "_th_Day2_L1", sep = "")):
           which(colnames(DATA) == paste(test, "_th_Day2_L54", sep = ""))))
  CD1<- c(D1, D2)
  CD2<- c(D2, D1)
  for(participant in 1:77){
    for(threshold in 0:33){
      th = threshold
      # Breaking the data in bins of 4
      if (th >= 0 & th <=15){
        i = 1
      }else if (th >= 16  & th <= 21){
        i = 2
      }else if (th >= 22  & th <=25){
        i = 3 
      }else {
        i = 4
      }
      
      
      location <- which(DATA[participant,CD1]==th)
      x[[paste(test, i, sep = "_")]] <- c(x[[paste(test, i, sep = "_")]],DATA[participant,CD2[location]])
      
    }
  }
}
x[["SS_1"]]<- as.numeric(unlist(x$SS_1))
x[["SS_2"]]<- as.numeric(unlist(x$SS_2))
x[["SS_3"]]<- as.numeric(unlist(x$SS_3))
x[["SS_4"]]<- as.numeric(unlist(x$SS_4))
x[["SF_1"]]<- as.numeric(unlist(x$SF_1))
x[["SF_2"]]<- as.numeric(unlist(x$SF_2))
x[["SF_3"]]<- as.numeric(unlist(x$SF_3))
x[["SF_4"]]<- as.numeric(unlist(x$SF_4))
x[["ZS_1"]]<- as.numeric(unlist(x$ZS_1))
x[["ZS_2"]]<- as.numeric(unlist(x$ZS_2))
x[["ZS_3"]]<- as.numeric(unlist(x$ZS_3))
x[["ZS_4"]]<- as.numeric(unlist(x$ZS_4))
x[["ZF_1"]]<- as.numeric(unlist(x$ZF_1))
x[["ZF_2"]]<- as.numeric(unlist(x$ZF_2))
x[["ZF_3"]]<- as.numeric(unlist(x$ZF_3))
x[["ZF_4"]]<- as.numeric(unlist(x$ZF_4))

# Data mined and now plotting the data

png("C://Users//DELL//OneDrive//Documents//TRV_bins_hist.png",
    h = 2200, w= 2700, units = "px", pointsize = 12, res = 300)

layout(matrix (c(c(21:26), c(1:5), 26, c(6:10),26 , c(11:15),26,
                 c(16:20), 26), ncol = 5, byrow = F)
       ,heights = c(0.35,rep(1, 3), 1.1, 0.40), widths = c(0.35, rep(0.5, 4)))


Label<- c("0-15","16-21", "22-25", "30-33")
Loc<- c(7.5,18.5, 23.5, 31.5)

for (t in Test){
  test = t  
  # Plotting the Frequency distribution of the retest thresholds
  par(mar = c(2,1,2,0))
  plot(5, 5, type = "n", axes = F, xlab = "", ylab = "")
  mtext(ifelse(test == "SS", "a) SITA Standard  ", ifelse(test == "SF", "b) SITA Fast  ",
                                                          ifelse(test == "ZS", "c) ZATA Standard  ", "d) ZATA Fast    "))),
        side = 3,  line = 0, cex =1)
  
  Colour<- c("cyan4", "cyan3", "orangered3", "orangered")
  par(mar = c(2,0,2,0))
  for(n in 1:4){
    
    
    barplot(ifelse(n == 4, 1000,  160),Loc[[n]]/3,  width = 3,
            col = "white", border = "white", xlim = c(0, 35),
            axes = F,
            ylim = c(0, ifelse(n == 4, 1000,  160)), 
            xlab = " ", cex.lab = 1, cex.axis = 1)
    axis(2,at = seq(0, ifelse(n == 4, 1000,  160), ifelse(n == 4, 200,  50)),
         labels = seq(0, ifelse(n == 4, 1000,  160), ifelse(n == 4, 200,  50)),
         cex.axis = 1,las = 1,hadj = ifelse(test != "SS", 100,  1),
         col.axis = ifelse(test != "SS", "white",  "black"),
         col = ifelse(test != "SS", "white",  "black"), lwd = 1)
    
    s<- hist(x[[paste(test, n, sep = "_")]], 
             breaks = seq(min(x[[paste(test, n, sep = "_")]]), 
                          max(x[[paste(test, n, sep = "_")]]), by = 1), cex.lab = 1,
             ,main= " ", border = "white", add = TRUE, 
             xlim = c(0, 35), ylim = c(0, ifelse(n == 4, 1000,  160)),
             axes = F,
             xlab = " ", ylab = " " ,col = Colour[which(Test == test)])
    
    
    
    axis(1, at= seq(0, 35, by =5), labels = seq(0, 35, by =5),line = 1,
         cex.axis = ifelse(n == 4, 1, 0.001), col.axis = "black", 
         lwd = ifelse(n == 4, 1, 0.001), 
         col= ifelse(n == 4, "black", "white"))
    abline (h=seq(from=0, to=1000, by= 50), col="grey98")
    
    
    text(ifelse(n == 1, which(s$counts > 160)+3, 1),150,
         ifelse(n == 1, s$counts[which(s$counts > 160)]," " ),
         col = "black", cex = 1.2, font = 2)
    
    text(ifelse(n == 4 & test == "ZS", 
                which(s$counts > 1000), 1)-4,950,
         ifelse(n == 4 & test == "ZS", s$counts[which(s$counts > 1000)]," " ),
         col = "black", cex = 1.2, font = 2)
    
    text(ifelse(n == 4 & test == "ZF", 
                which(s$counts > 1000)-4, 1),950,
         ifelse(n == 4 & test == "ZF", s$counts[which(s$counts > 1000)]," " ),
         col = "black", cex = 1.2, font = 2)
    
    
    mtext(paste("n = ", length(x[[paste(test, i, sep = "_")]]), "       ", sep = " "), side = 3,
          line = -5, cex = 1)
  }
  
}

# plot to add the heading "Bin" in the figure
par(mar = c(0, 0, 0,  0))
plot(5,5 , type = "n", xlab = " ", ylab = " ", axes = F)
text(5, 5.5, "Bin", cex = 1.5)

# Adding a plot to add Bin values
for (n in 1:4){
  par(mar = c(2, 0, 2, 0))
  plot(5, 5, type = "n", xlab = " ", ylab = " ", axes = F)
  text(5, 5 , paste( Label[n]," dB", sep = " "), cex = 1.5, srt = 90)
  
  
}
mtext("                                                                                                           Frequency Distribution",
      side = 2, cex = 1, line = -2)


# Adding a plot to put x-axis
par(mar = c(2, 2, 2, 2))
plot(5,5 , type = "n", xlab = " ", ylab = " ", axes = F)

mtext("                           Threshold (dB)", 
      side = 3, cex = 1, line = -2)

dev.off()

