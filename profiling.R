date_hist_by_year <- function (col_data, start_year, end_year) {
    # Requires lubridate and ggplot
    col_data %>% 
       lubridate::ymd(col_data) %>% as.Date() %>% lubridate::year() %>% as.data.frame() %>% 
       ggplot(aes(.)) + geom_bar() + scale_x_continuous(limits=c(start_year, end_year)) +
       ggtitle(paste0("Records by ", col_data)) }
