library(tidyverse)
library(ncdf4)
library(lubridate)


  ## chilling days
feldberg <- read.table("data/vgdcnGME00120934.dat")
st_bernard <- read.table("data/vgdcnSZ000006717.dat") 
lugano <- read.table("data/vgdcnSZ000009480.dat") 
zurich <- read.table("data/vgdcnSZ000003700.dat") 

climate <- bind_rows(feldberg = feldberg, lugano = lugano, zurich = zurich, .id = "station") %>%
  rename(year = V1, month = V2, day = V3, temperature = V4) %>% 
  mutate(year2 = if_else(month > 10, 2015, 2016),
         date2 = ymd(paste(year2, month, day)), 
         station = factor(station, levels = c("lugano", "zurich", "feldberg"))) %>% 
  as_tibble()



climate %>% ggplot(aes(x = date2, y = temperature)) +
  geom_point(alpha = 0.1) + 
  geom_rect(xmin = -Inf, xmax = ymd("2016-04-22"), ymin = 0, ymax = 8, fill = muted("blue"), alpha = 0.1, inherit.aes = FALSE) +
  facet_wrap(~station)

climate2 <- climate %>% 
  filter(between(year, 1960, 1969)|between(year, 2000, 2009)) %>% 
#  filter(year >= 1960) %>% 
  mutate(decade = floor(year/10) * 10, decade = factor(decade))#, levels = c(2000, 1960)))

ggplot(climate2, aes(x = date2, y = temperature, colour = decade)) +
  geom_point(alpha = 0.1) +  
  geom_rect(xmin = -Inf, xmax = ymd("2016-04-22"), ymin = 0, ymax = 8, colour = muted("blue"),  inherit.aes = FALSE, fill = NA) + 
  facet_wrap(~station) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  labs(x = "", y = "Temperature °C", colour = "Decade")

ggplot(climate2, aes(x = date2, y = temperature, colour = decade)) +
  #stat_summary(fun.y = "mean", geom = "line") +  
  geom_smooth(se = FALSE) + 
  geom_rect(xmin = -Inf, xmax = ymd("2016-04-22"), ymin = 0, ymax = 8, colour = muted("blue"),  inherit.aes = FALSE, fill = NA) + 
  facet_wrap(~station) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  labs(x = "", y = "Temperature °C", colour = "Decade")


climate2 %>% filter(month %in% c(11, 12, 1:4)) %>% 
  mutate(chilling = case_when(temperature > 8 ~ "Too warm", 
                                         temperature > 0 ~ "Chill", 
                                         TRUE ~ "Too cold")) %>% 
  mutate(chilling = factor(chilling, levels = c("Too warm", "Chill", "Too cold")), 
         decade = factor(decade, levels = c(1960, 2000))) %>% 
  ggplot(aes(x = decade, fill = chilling)) +
  geom_bar() +facet_wrap(~station)


climate %>% mutate(month = plyr::mapvalues(month, from = 1:12, to = month.abb), 
                   month = factor(month, levels = month.abb)) %>% 
  group_by(station, month, year) %>%
  filter(year >= 1960) %>% 
  summarise(temperature = mean(temperature)) %>% 
  do({tidy(lm(temperature ~ year, data = .))}) %>%
  filter(term == "year") %>% 
  ggplot(aes(x = month, y = estimate, ymax = estimate +std.error, ymin = estimate - std.error)) +
   geom_pointrange()+  
  facet_wrap(~station)
  
climate %>% mutate(month = plyr::mapvalues(month, from = 1:12, to = month.abb), 
                   month = factor(month, levels = month.abb)) %>% 
  group_by(station, month, year) %>%
  filter(year >= 1960) %>%
  ggplot(aes(x = year, y = temperature, colour = month)) +
 # geom_line() +
  geom_smooth(method = "lm")+
  facet_wrap(~station)


  summarise(temperature = mean(temperature)) %>% 
  do({tidy(lm(temperature ~ year, data = .))}) %>%
  filter(term == "year") %>% 
  ggplot(aes(x = month, y = estimate, ymax = estimate +std.error, ymin = estimate - std.error)) +
  geom_pointrange()+  
  facet_wrap(~station)

