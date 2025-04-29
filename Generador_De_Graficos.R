install.packages("readxl")
install.packages("mirt")
library(readxl)
library(mirt)
B_I <- read_excel("Base_Items_Depurada.xlsx")
View(B_I)
gr<-mirt(B_I, 1, itemtype = "2PL")
#un conjunto de graficos 1-definan-continuos
plot(gr, type = "trace",which.items = 1:10)
#un conjunto de graficos de los items en especifo
plot(gr, type = "trace",which.items = c(5,45))
#un item solo
plot(gr, type = "trace",which.items = 5)
