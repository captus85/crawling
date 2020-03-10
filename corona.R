library(httr)
library(rvest)
library(tidyverse)

tar <- "http://ncov.mohw.go.kr/bdBoardList_Real.do?brdId=1&brdGubun=13&ncvContSeq=&contSeq=&board_id=&gubun="

read_html(tar) %>%
  html_nodes("table.num") %>%
  html_table(fill = T)

Sys.setlocale("LC_ALL", "C") 

