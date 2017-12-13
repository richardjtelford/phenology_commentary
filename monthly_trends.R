library(tidyverse)
library(ncdf4)
library(lubridate)
library(data.table)
library(broom)
nc <- nc_open("data/air.mon.mean.nc")
ncatt_get(nc, varid = "time")

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
dates <- as.Date(ncvar_get(nc, "time")/24, origin = "1800-01-01 00:00:0.0")
start <- "1960-01-01"

global <- ncvar_get(nc)
month(dates)

global2 <- expand.grid(lon = lon, lat = lat, date = dates) %>% 
  mutate(temp = as.vector(global))
global2 <- setDT(global2)

global3 <- global2 %>% 
  filter(date > ymd(start), !is.na(temp), lat > 30, lat < 60, lon > -15, lon < 45) %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(lon, lat, month) %>% 
  do({
    mod <- lm(temp ~ year, data = .)
    tidy(mod) %>% slice(2)
  })

library(scales)
mp <- map_data(map = "world")
ggplot(global3, aes(x = lon, y = lat, fill = estimate)) +
  geom_raster() +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = NA, colour = "grey60") +
  scale_fill_gradient2(low =  muted("blue"), high = muted("red")) +
  facet_wrap(~month) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 
  
