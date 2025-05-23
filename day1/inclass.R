## Read binary file
library(readxl) ## Excel
library(haven) ## STATA

df <- read_excel(path = "data/vaccine_data.xlsx", 
                 sheet = 1)

## 1. Có bao nhiêu biến có missing, gồm những loại biến nào?
library(skimr)
skim(df)

## 2. Làm sạch tên cột và đổi tên vgb_24 thành vgb_truoc_24
## và vgb_24_2 thành vgb_sau_24
library(janitor)
library(tidyverse)
df <- df %>% clean_names() #%>% colnames()

df <- df %>% 
  clean_names() %>% # Tự động làm sạch tên cột
  rename( 
    vgb_truoc_24 = vgb_24, # Tên mới = Tên cũ
    vgb_sau_24 = vgb_24_2
  )

## 3. Đổi kiểu dữ liệu của các biến về đúng loại dữ liệu

date_cols <- c("ngaysinh", "vgb_truoc_24","vgb_sau_24","vgb_1","vgb_2","vgb_3","vgb_4","hg_1","hg_2","hg_3","hg_4","uv_1","uv_2","uv_3","uv_4")

df <- df %>% 
  mutate( # Each column
    gioitinh = as.factor(gioitinh),
    tinhtrang = ifelse(tinhtrang == "theo dõi", T, F)
  ) %>% 
  mutate_at( # Multiple column
    date_cols, 
    dmy
  ) %>% 
  rename(
    theodoi = tinhtrang
  )

## 4. Lọc dữ liệu
## * Những người có Quận 2.
## * Những người có vgb_1 trước ngày 20 tháng 2 năm 2024.
## * Những người có Quận 2 hoặc vgb_1 trước ngày 20 tháng 2 năm 2024.
## * Những người có Quận 2 và vgb_1 trước ngày 20 tháng 2 năm 2024.

df %>% filter(huyen == "Quận 2") %>% select(id, huyen)

df %>% filter(vgb_1 < as.Date("2024-02-20")) %>% select(id, vgb_1)

df %>% filter(huyen == "Quận 2" | vgb_1 < as.Date("2024-02-20")) %>% select(id, huyen, vgb_1)

df %>% filter(huyen == "Quận 2" & vgb_1 < as.Date("2024-02-20")) %>% select(id, huyen, vgb_1)

## 5. Thay những giá trị NA của vgb_truoc_24 thành ngày hiện tại
library(lubridate)
df <- df %>%
  mutate(vgb_truoc_24 = replace_na(vgb_truoc_24, today()))

table(df$vgb_truoc_24==today())

## 6. Tạo function để tính cho các quốc gia khác 

covid_cases <- readRDS("data/covid_cases.rds")

compute_stats <- function(data, colname){
  return(
    c(
      min = min(data[[colname]]),
      q1 = quantile(data[[colname]], 0.25, names=FALSE),
      median = median(data[[colname]]),
      q3 = quantile(data[[colname]], 0.75, names=FALSE),
      max = max(data[[colname]])
    )
  )
}

compute_stats(covid_cases, "cases_jpn")
