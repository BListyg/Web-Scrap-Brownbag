#Install and load “rvest” package

Install.packages(“rvest”)
library(rvest)

riasec.scrape = function(x,y) {read_html(x) %>% 
		     html_nodes(y) %>% 
         html_text()}

cbind(
riasec.scrape(x = "https://www.onetonline.org/link/details/11-9013.01", y = "#wrapper_Interests .report2a b"), 
riasec.scrape(x = "https://www.onetonline.org/link/details/11-9013.01", y = "#wrapper_Interests .moreinfo b")
)
