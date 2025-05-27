# ====== R Scipt for take home exercise solution === 
# Gọi packages
library(dplyr)
library(tidyverse)
# Nhiệm vụ 1: Nhập dữ liệu
covid_cases <- readRDS("~/GitHub/R-Together/day1/data/covid_cases.rds")
skimr::skim(covid_cases)
# Nhiệm vụ 2: Tính toán đơn giản

# Ngày báo cáo dữ liệu sớm nhất và muộn nhất
covid_cases <- covid_cases %>% 
  mutate(date = as.Date(date),
         first_report_date = min(date, na.rm = TRUE),
         last_report_date = max(date, na.rm = TRUE))
# tổng số trường hợp trên mọi quốc gia mỗi ngày báo cáo.
?rowsum
covid_cases <- covid_cases %>%
  mutate(
    case_global = rowSums(across(starts_with("cases_")), na.rm = TRUE)
  )
# Tỷ lệ phần trăm các trường hợp toàn cầu mà các trường hợp Trung Quốc báo cáo trong ngày
covid_cases <- covid_cases %>%
  mutate(
    percent_chn = (cases_CHN / case_global) * 100
  )
# Nhiệm vụ 3
compute_percent <- function(data, country_code) {
  country_col <- paste0("cases_", country_code)
  
  if (!(country_col %in% names(data))) {
    stop(paste("Không tìm thấy cột", country_col, "trong dữ liệu."))
  }
  
  data %>%
    rowwise() %>%
    mutate(
      total_cases = sum(c_across(starts_with("cases_")), na.rm = TRUE),
      percent = (get(country_col) / total_cases) * 100
    ) %>%
    ungroup() %>%
    select(date, !!country_col, total_cases, percent)
}
# ví dụ
compute_percent(covid_cases, "vnm")
