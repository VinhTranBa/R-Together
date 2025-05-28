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
    percent_chn = (cases_chn / case_global) * 100
  )
# Nhiệm vụ 3
#
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


# Tạo 3 bảng phần trăm riêng biệt cho từng quốc gia
percent_vnm <- compute_percent(covid_cases, "vnm") %>%
  select(date, percent_vnm = percent)

percent_usa <- compute_percent(covid_cases, "usa") %>%
  select(date, percent_usa = percent)

percent_sgp <- compute_percent(covid_cases, "sgp") %>%
  select(date, percent_sgp = percent)

# Gộp các cột phần trăm trở lại bảng gốc
covid_cases_with_percent <- covid_cases %>%
  left_join(percent_vnm, by = "date") %>%
  left_join(percent_usa, by = "date") %>%
  left_join(percent_sgp, by = "date")

#In bảng covid_cases cuối cùng
covid_case_final <- covid_cases_with_percent %>% 
  select(date, percent_chn, percent_vnm, percent_usa, percent_sgp)
head(covid_case_final)

# Nhiệm vụ 4
skimr::skim(covid_case_final)
# Nhận xét: Trung quốc có trung bình số ca cao nhất với 38.6 (sd:45.6), tiếp theo đến USA 12.3 (16.2), Việt Nam và Singapore có trung bình số ca thấp
# tất cả đều missing 1 case tại ngày 20-1-2020