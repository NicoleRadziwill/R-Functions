library(janitor)
library(readr)
library(dplyr)
library(ExPanDaR)

rawurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS3A8UyOQdJJiDqFhOAadnAzOWaBaS9tLts5bB_f9ce_7QcfXlvpLizEdkYZ8WqsMnqzo7Ybe45Jyd8/pub?gid=1246114051&single=true&output=csv" 

data <- readr::read_csv(rawurl) %>% janitor::clean_names()
   xdata <- data %>% select(-product_pricecat) 
   names(xdata) <- c("date","region","order_type","customer_id","email",
                  "product_type","billed","orders","name","continent","type","unit_cost")
   xdata %>% mutate(total_cost=orders*unit_cost, total_profit=billed-total_cost) -> xdata

ExPanD(xdata)
