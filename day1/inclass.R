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

# bài tập
# tải dữ liệu
vaccine <- readxl::read_xlsx("data/vaccine_data.xlsx")
#1
library(skimr)
is.na(vaccine)
skimr::skim(vaccine)
#2
library(janitor)
library(tidyverse)

vaccine <- vaccine %>% 
  clean_names() %>% # Tự động làm sạch tên cột
  rename( 
    vgb_truoc_24 = vgb_24, # Tên mới = Tên cũ
    vgb_sau_24 = vgb_24_2
  )
#3
str(vaccine)

date_cols <- c("ngaysinh", "vgb_truoc_24","vgb_sau_24","vgb_1","vgb_2","vgb_3","vgb_4","hg_1","hg_2","hg_3","hg_4","uv_1","uv_2","uv_3","uv_4")

vaccine <- vaccine %>% 
  mutate( # Each column
    gioitinh = as.factor(gioitinh),
    tinhtrang = ifelse(tinhtrang == "theo dõi", TRUE, FALSE)
  ) %>% 
  mutate_at( # Multiple column
    date_cols, 
    dmy
  )

str(vaccine)
#4
vaccine %>% filter(huyen == "Quận 2")
vaccine %>% filter(vgb_1<as.Date("2024-02-20"))
vaccine %>% filter(huyen == "Quận 2" | vgb_1<as.Date("2024-02-20"))
vaccine %>% filter(huyen == "Quận 2" & vgb_1<as.Date("2024-02-20"))

#5
?replace_na()
library(dplyr)
library(lubridate)
is.na(vaccine$vgb_truoc_24)

vaccine <-  vaccine %>% 
  mutate(vgb_truoc_24= replace_na(vgb_truoc_24, today()))

vaccine <- vaccine %>%
  mutate(vgb_truoc_24 = coalesce(vgb_truoc_24, today()))
table(vaccine$vgb_truoc_24==today())
#6
# data import
covid_cases <- readRDS("data/covid_cases.rds")
# example
case_col <- "cases_chn"

basic_stats <- c(
  min =  min(covid_cases[[case_col]]),
  q1 = quantile(covid_cases[[case_col]], 0.25, names=FALSE),
  median = median(covid_cases[[case_col]]),
  q3 = quantile(covid_cases[[case_col]], 0.75, names=FALSE),
  max = max(covid_cases[[case_col]])
)
# tạo function cho các quốc gia

func_national <- function(data,colum) {
  return(
    c(
    min = min(data[[colum]], na.rm = TRUE),
    q1 = quantile(data[[colum]], 0.25, names = FALSE, na.rm = TRUE),
    median = median(data[[colum]], na.rm = TRUE),
    q3 = quantile(data[[colum]], 0.75, names = FALSE, na.rm = TRUE),
    max = max(data[[colum]], na.rm = TRUE)
  )
)
}
#chạy kết quả
func_national(covid_cases, "cases_jpn")
