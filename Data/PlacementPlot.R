library(readxl)
data <- data.frame(read_excel("/Users/bomeara/Documents/MyDocuments/GitClones/EEB_NRT/Data/Placement.xlsx"))
setwd("/Users/bomeara/Documents/MyDocuments/GitClones/EEB_NRT/Data/")
rownames(data) <- data[,1]
data <- data[,-1]
GetProportion <- function(x) {
  return(x/sum(x)) 
}
proportions <- apply(data, 2, GetProportion)
my.palette <- RColorBrewer::brewer.pal(dim(data)[2], "Dark2")
pdf(file="Placement.pdf", width=5, height=5)
par(mar=c(5,9,4,2)+.1)
barplot(t(proportions), beside=TRUE, horiz=TRUE, xlab="Proportion of graduated students", las=1, col=my.palette)
legend("topright", legend=colnames(proportions), col=my.palette, fill=my.palette)
# par(mfcol=c(1,dim(data)[2]))
# for (i in sequence(dim(data)[2])) {
#   barplot(proportions[,i], xlim=c(0, 0.5), main=colnames(proportions)[i], col=my.palette, horiz=TRUE, las=1, mar=c(5,7,4,2)+.1)
# }
dev.off()
system("open Placement.pdf")

pdf(file="Placement2.pdf", width=12, height=5)
par(mar=c(5,5,4,14)+.1)
par(xpd=TRUE)
my.palette <- c(colorRampPalette(c("red","darkred"))(3),"darkgoldenrod1", colorRampPalette(c("cadetblue1","cadetblue3"))(3), "gray") 
#colnames(proportions) <- gsub('.', ' ', colnames(proportions))
barplot(proportions, col=my.palette, ylab="Proportion of graduated students")
legend(6.5,1, legend=rev(rownames(proportions)), col=rev(my.palette), fill=rev(my.palette))
dev.off()
system("open Placement2.pdf")
