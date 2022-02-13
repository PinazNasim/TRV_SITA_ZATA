library(magick)


Test<- choose.files()
Test<- image_read(Test)
pdf(paste("C://Users//DELL//OneDrive//Documents//image.pdf"), h = 45, w= 60)
par(mfrow = c(2, 4))

for (t in 1:length(Test)){
  
  
  plot(NA,bty = "n" , xlim=c(-100, 100),ylim=c(-100,100),xaxt="n",yaxt="n")
  rasterImage(Test[t],-100,-100, 100,100)
  
  
}
dev.off()
