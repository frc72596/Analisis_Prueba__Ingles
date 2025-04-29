library(readxl)
library(dplyr)
library(Amelia)
library(mirt)
library(flextable)
library(writexl)
BF_PI <- read_excel("C:/Users/felip/Downloads/BD proyecto ingles.xlsx", 
                    na = "99")
mean(BF_PI$`Calificación/50,00`)
#Base de los items
Items <- BF_PI %>% 
  select(starts_with("P"))
#Imputación multiple para depurar datos perdidos
Items_De<- amelia(
  Items,
  noms = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", 
           "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35", "P36", "P37", "P38", 
           "P39", "P40", "P41", "P42", "P43", "P44", "P45", "P46", "P47", "P48", "P49", "P50", "P51", "P52", "P53", "P54", "P55")
)
BITEMS<-Items_De$imputations[[4]]
summary(BITEMS)
#Parte analisis de los items 
Ing<-mirt(BITEMS, 1, itemtype = "2PL")
summary(Ing) 
coefs <- coef(Ing, IRTpars = TRUE, simplify = TRUE)
coefs_items <- coefs[!names(coefs) %in% "GroupPars"]
coefs_df_list <- lapply(coefs_items, function(x) {
  df <- as.data.frame(t(x))
  df$Item <- rownames(df)
  df
})
all_params <- unique(unlist(lapply(coefs_df_list, names)))
all_params <- setdiff(all_params, "Item")
coefs_df_list <- lapply(coefs_df_list, function(df) {
  missing <- setdiff(all_params, names(df))
  for (m in missing) df[[m]] <- NA
  df[, c("Item", all_params)]  # ordenar columnas
})
coefs_df <- do.call(rbind, coefs_df_list)
rownames(coefs_df) <- NULL
write_xlsx(coefs_df, path = "Parametros_Modelo_ingles.xlsx")
write_xlsx(BITEMS, path = "Base_Items_Depurada.xlsx")






flextable(Paramodel)
plot(Ing, type = "trace")
plot(Ing, type = "trace",which.items = 1:10)
plot(Ing, type = "trace",which.items = 1:5)
plot(Ing, type = "trace",which.items = c(1,43))
plot(Ing, type = "infotrace")
plot(Ing, type = "infotrace",which.items = c(1,43))
plot(Ing, type = "infotrace",which.items = c(1,25))
plot(Ing)
