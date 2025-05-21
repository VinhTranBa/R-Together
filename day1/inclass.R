df <- life_exp
?read.table()
df <-read.table(file="data/life-exp.csv", header = T, sep =",")
head(df)

library(readxl)
library(haven)
?readxl
df2 <- read_csv("data/life-exp.csv", sheet=1)
df3 <- read_dta("data/linelist-raw.dta")
