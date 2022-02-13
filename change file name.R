# choose files to change the file name
File<- choose.files()
library(pdftools)



#Assign names to the files
for (i in 1: length(File)){
  
  a<- pdf_data(File[i])
  a<- as.data.frame(a)
  ID<- a[which(a[, 6] == "ID:")+1, 6]
  Test<- paste(a[which(a[, 6] == "SITA"), 6], a[which(a[, 6] == "SITA")+1, 6], sep = "")
  b<- (paste("C:\\Users\\DELL\\OneDrive\\PhD work\\HFA_pdfs\\",ID, Test,i,".png", sep = ""))
  File[i]<- pdf_convert(File[i], format = "png")
  file.copy(from = File[i], to = b)
}
