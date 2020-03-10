library(httr)
library(dplyr)
library(purrr)
library(plyr)

tar <- "https://kreditjob.com/api/dashboard/filter"

body <- list("data_ym"= "1911",
             "bjd_code"= F,
             "industry"= F,
             "min_sales"= F,
             "max_sales"= F,
             "min_salary"= F,
             "max_salary"= F,
             "min_prsn"= F,
             "max_prsn"= F,
             "min_prsn_ratio"= F,
             "max_prsn_ratio"= F,
             "only_good_cmpn"= F,
             "sort"= "SALES_VALUE",
             "order"= "DESC",
             "page"= 1,
             "size"= 100)

dat <- POST(tar, body = body, encode = "json") %>%
  content()

dat <- dat$list


result <- tibble(
  PK_NM_HASH = dat %>% map_chr("PK_NM_HASH"),
  COMPANY_INFO_NO = dat %>% map_chr("COMPANY_INFO_NO"),
  CMPN_NM = dat %>% map_chr("CMPN_NM"),
  WKP_ADRS = dat %>% map_chr("WKP_ADRS"),
  WKP_ADRS_CODE = dat %>% map_chr("WKP_ADRS_CODE"),
  PRSN_BASE = dat %>% map_chr("PRSN_BASE"),
  AVG_SALARY_FINAL = dat %>% map_chr("AVG_SALARY_FINAL"),
  PRSN_DIF_RATIO =dat %>% map_chr("PRSN_DIF_RATIO"),
  SALES_VALUE = dat %>% map_chr("SALES_VALUE"),
  OPEN_YN = dat %>% map_chr("OPEN_YN"),
  CMPN_GRADE = dat %>% map_chr("CMPN_GRADE"),
  GOOD_CMPN_YN = dat %>% map_chr("GOOD_CMPN_YN"),
  JOBDOM_COUNT = dat %>% map_chr("JOBDOM_COUNT"),
)



dat %>% transpose() %>% as_tibble() %>% map_df(~unlist(.x))
  
ldply(dat, data.frame)

