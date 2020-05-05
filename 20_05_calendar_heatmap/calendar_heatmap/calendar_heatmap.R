library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)

covid19 <- read_csv("~/Desktop/covid19.csv")

covid19 <- covid19 %>% 
  janitor::clean_names()


covid19 %>% 
  arrange(date)

skimr::skim(covid19)


poland <- covid19 %>% 
  filter(country_region == 'Poland')

poland <- poland %>% 
  inner_join(mweek_tbl)

ggplot(poland) + 
  geom_tile(aes(mweek, wday, fill = confirmed)) + 
  scale_fill_viridis_c('confirmed',
                       breaks = c(2500, 5000, 7500, 10000, 12500)) +
  theme_minimal() + 
  facet_wrap(~month)






australia <- covid19 %>% 
  filter(country_region == 'Australia')

ggplot(data = australia, aes(x = date, y = confirmed, 
                             fill = as.factor(province_state))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~province_state) +
  scale_fill_discrete(name = "Prowincja", 
                    label = LETTERS[1:8])


australia_2 <- australia %>% filter(date == '2020-01-31' | date == '2020-02-29' |
                     date == '2020-03-31' | date == '2020-04-30')

ggplot(data = australia_2, aes(x = date, y = confirmed, 
                             fill = as.factor(province_state))) +
  geom_bar(stat='identity', position = 'dodge')



tmp_tbl <- tibble(date = seq.Date(date('2019-01-01'), 
                                  date('2020-12-31'), 
                                  by = 1),
                  rok = year(date),
                  miesiac = month(date),
                  tydzien = week(date),
                  dzien_t = wday(date, label = T))

koniec_mies <- tmp_tbl %>% 
  group_by(rok,miesiac) %>% 
  summarise(date = max(date))
  

australia %>% 
  inner_join(koniec_mies)

mweek_tbl <- tibble(date = seq.Date(date('2019-01-01'), 
                              date('2020-12-31'), 
                              by = 1),
              month = month(date, label = T),
              week = isoweek(date),
              wday = wday(date, week_start = 1, label = T)) %>% 
  group_by(month) %>% 
  mutate(mweek = week - min(week)+1) %>% 
  ungroup()





