df <- life_exp
?read.table()
df <-read.table(file="data/life-exp.csv", header = T, sep =",")
head(df)
# data import
library(readxl)
library(haven)
?readxl
df2 <- read_csv("data/life-exp.csv", sheet=1)
df3 <- read_dta("data/linelist-raw.dta")

# data clean
a <- 2 # Giá trị
a <- c(1:4) # Vector
df <- data.frame(a = c(1:4),
                 b = c(2:5)) # Matrix
df <- readxl::read_xlsx("data/vaccine_data.xlsx")
class(df)