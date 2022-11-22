library(magick)


Test<- choose.files()
Test<- image_read(Test)




pdf(paste("C://Users//DELL//OneDrive//Documents//image.pdf"), h = 45, w= 100)
# par(mfrow = c(2, 5))
layout(matrix(c((1:4),9, (5:8), 9), nrow = 2, ncol = 5, byrow = T), 
       widths = c(rep(1, 4), 2))
Main <- rep(c("SSD1", "SSD2", "SFD1", "SFD2", "ZSD1", "ZSD2", "ZFD1", "ZFD2", "OCT"), 77)

for (t in 1:length(Test)){
  
  plot(NA,bty = "n" , xlim=c(-100, 100),ylim=c(-100,100),xaxt="n",yaxt="n")
  rasterImage(Test[t],-100,-100, 100,100)
  mtext(Main[t], side = 3, line = -5, cex = 5)
  
  
}
dev.off()
