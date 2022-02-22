library(BlandAltmanLeh)
DATA<- read.csv("C://Users//DELL//Documents//DATA//TRV_DATA.csv")
# source("C://Users//DELL//Documents//CODES//SITA_ZATA_TRV_codes//Table_Data.R")
png("C://Users//DELL//OneDrive//Documents//Bland_Altman_MD _SD.png",w=3500,h=3500)
layout(matrix(c(1, 2, 3, 4, 5, 5), byrow = T, ncol = 2, nrow = 3), heights = c(1, 1, 0.25),
       widths = c(1, 1))
par(mar = c(25, 35, 20, 10))

Test_names<- c("SS", "ZS", "SF", "ZF")

for (name in Test_names){
        test = name
        
        md_D1<- unlist(DATA[paste(test, "_Day1_md",sep="")])
        md_D2<- unlist(DATA[paste(test, "_Day2_md",sep="")])
        ba<- bland.altman.stats(md_D1, md_D2)
        plot(ba$means, ba$diffs, xlim=c(-38, 5), ylim = c(-15, 15), 
             panel.last = abline(h = ba$lines, lty = 3, col = 1),
             pch = ifelse(DATA$Category == "Normal", 16, 17),
             main= ifelse(test == "SS", "a) SITA Standard", ifelse(test == "SF", "c) SITA Fast",
                                                                   ifelse(test == "ZS", "b) ZATA Standard", "d) ZATA Fast"))),
             xlab=" " ,ylab=" ", axes = F, 
             type = 'p', col = ifelse(DATA$Category == "Normal", "midnightblue", "brown3"), cex=15,
              cex.main = 15)
        text(-36, (ba$mean.diffs-1), paste(round(ba$mean.diffs, digits = 2)),
             font = 2, col = "black", cex = 10 )
        text(-36, (ba$upper.limit-1), paste(round(ba$upper.limit, digits = 2)), 
             font = 2, col = "black", cex = 10 )
        text(-36, (ba$lower.limit-1), paste(round(ba$lower.limit, digits = 2)), 
             font = 2, col = "black", cex = 10 )
        
        axis(1, at = seq(5, -35, -10), labels = seq(5, -35, -10),
             cex.axis = 10, padj = 1.5)
        axis(2, at = seq(15, -15, -5), labels = seq(15, -15, -5),
             cex.axis = 10, hadj = 1.5, las = 1)
        mtext(text = ifelse(test == "SS", "Difference btw visit 1 & 2 MD (dB)                                                    ",
                            " "), line = 22, side = 2, cex = 10)
        
        mtext(text= ifelse(test == "ZF", "Mean of MD of visit 1 & 2 (dB)                                                "," "),
              line = 22, cex = 10, side = 1)
        
        
            
        box(col = "black")
}
par(mar = c(0, 0, 0, 0))
plot(5, 5, type = "n", xlab = " ", ylab = " ", bty = "n", axes = F)
legend(3, 6.5, bty = "n", legend = c("Normal", "Glaucoma"), col = c ("midnightblue", "brown3"),
       pch = c(16, 17), cex = 15)


dev.off()
# library(pdftools)
# pdf_convert("C://Users//DELL//OneDrive//Documents//Bland_Altman_MD _SD.pdf", format = "jpeg")
