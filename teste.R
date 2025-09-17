library(readxl)
caminho <- paste0(getwd(), "/dados.xlsx")

dados <- read_xlsx(caminho, sheet= 1)
df2 <- read_xlsx(caminho, sheet =2)
