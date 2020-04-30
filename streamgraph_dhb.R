# requires covid.df

library(streamgraph)
library(tidyr)
library(dplyr)
library(magrittr)
library(zoo)

# j_covid.df <- covid.df %>% group_by(DHB) %>% rename(Date = `Report Date`)
# join_dates <- data.frame(Date = seq(min(covid.df$`Report Date`),
#                                     max(covid.df$`Report Date`),
#                                     by = "days"))
# 
# full_join(j_covid.df,join_dates,by = "Date")

na.locf2 <- function(x) na.locf(x, na.rm = FALSE)

covid_cumulative_dhb.df <- covid.df %>%
  rename(Date = `Report Date`) %>%
  group_by(DHB,Date) %>%
  tally() %>%
  mutate(c_n = cumsum(n)) %>%
  ungroup() %>%
  complete(Date = seq(min(covid.df$`Report Date`),
                      max(covid.df$`Report Date`),
                      by = "days")
           ,DHB) %>%
  group_by(DHB) %>%
  arrange(Date,DHB) %>%
  do(na.locf2(.))
  #fill(c_n,.direction = "up")

# write.csv(covid_cumulative_dhb.df,file = "cum_dhb.csv",quote = F,row.names = F)
# read.csv()

# covid_cumulative_dhb.df$c_n <- na.locf(covid_cumulative_dhb.df$c_n,fromLast = F,na.rm = F)
# covid_cumulative_dhb.df$n[is.na(covid_cumulative_dhb.df$n)] <- 0
# covid_cumulative_dhb.df <- na.omit(covid_cumulative_dhb.df)

pp <- streamgraph(covid_cumulative_dhb.df, 
                  key = "DHB", 
                  value = "c_n", 
                  date = "Date", 
                  height = "800px", 
                  width = "1000px")

pp

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/streamgraphBasic.html"))