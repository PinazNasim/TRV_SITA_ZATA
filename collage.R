library(pdftools)
library(magick)

SSD1<- choose.files()
SSD2<- choose.files()
SFD1<- choose.files()
SFD2<- choose.files()
ZSD1<- choose.files()
ZSD2<- choose.files()
ZFD1<- choose.files()
ZFD2<- choose.files()
SSD1<- image_read(SSD1)
SSD2<- image_read(SSD2)
SFD1<- image_read(SFD1)
SFD2<- image_read(SFD2)
ZSD1<- image_read(ZSD1)
ZSD2<- image_read(ZSD2)
ZFD1<- image_read(ZFD1)
ZFD2<- image_read(ZFD2)


Test<- c(SSD1,SSD2,SFD1,SFD2,ZSD1,ZSD2,ZFD1,ZFD2)
Main<- c("SSD1","SSD2","SFD1","SFD2","ZSD1","ZSD2","ZFD1","ZFD2")
pdf("C://Users//DELL//OneDrive//Documents//image.pdf", h = 45, w= 60)
par(mfrow = c(2, 4))
for (t in 1:8){
  
  # Fig<- image_crop(Test[t], geometry="0x-0+140")
  Fig<- Test[t]
  
  
  plot(NA,bty = "n" , xlim=c(-100, 100),ylim=c(-100,100),xaxt="n",yaxt="n")
  rasterImage(Fig,-100,-100, 100,100)
  
  mtext(Main[t] , side = 3,  line = -6, cex =5)
  
  
}
dev.off()

