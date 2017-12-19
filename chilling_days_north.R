library(tidyverse)
library(ncdf4)
library(lubridate)
library(readxl)
library(scales)


#data from 
#https://climexp.knmi.nl/selectdailyseries.cgi?id=someone@somewhere

## chilling days
feldberg <- read.table("data/vgdcnGME00120934.dat")
st_bernard <- read.table("data/vgdcnSZ000006717.dat") 
lugano <- read.table("data/vgdcnSZ000009480.dat") 
zurich <- read.table("data/vgdcnSZ000003700.dat") 

feldberg <- read_excel("data/vgdcnGME00120934.xlsx")
lugano <- read_excel("data/vgdcnSZ000009480.xlsx") 
nesbyen <- read_excel("data/vgdcnNOE00105482.xlsx")
bergen <- read_excel("data/vgdcnNO000050540.xlsx")
vernes <- read_excel("data/vgdcnNOE00112071.xlsx")
forse <- read_excel("data/vgdcnSWE00139962.xlsx")
tromso <- read_excel("data/vgdcnNO000001026.xlsx")
utsjoki <- read_excel("data/vgdcnFIE00146698.xlsx")


climate <- bind_rows(feldberg = feldberg, lugano = lugano, nesbyen = nesbyen, bergen = bergen, utsjoki = utsjoki, tromso = tromso, .id = "station") %>%
  rename(year = V1, month = V2, day = V3, temperature = V4) %>% 
  mutate(year2 = if_else(month > 10, 2015, 2016),
         date2 = ymd(paste(year2, month, day)), 
         station = factor(station, levels = c("lugano", "feldberg", "bergen", "nesbyen", "tromso", "utsjoki"))) %>% 
  as_tibble()


climate %>% ggplot(aes(x = date2, y = temperature)) +
  geom_point(alpha = 0.1) + 
  geom_hline(yintercept = c(0, 8), color = "blue") +
  #geom_rect(xmin = -Inf, xmax = ymd("2016-04-22"), ymin = 0, ymax = 8, fill = muted("blue"), alpha = 0.1, inherit.aes = FALSE) + 
  facet_wrap(~station)



climate2 <- climate %>% 
  filter(between(year, 1960, 1969)|between(year, 2000, 2009)) %>% 
  #  filter(year >= 1960) %>% 
  mutate(decade = floor(year/10) * 10, decade = factor(decade))#, levels = c(2000, 1960)))

# Mean leaf-out date
box <- data_frame(station = c("lugano", "feldberg", "bergen", "nesbyen", "tromso", "utsjoki"),
  xmin = rep(-Inf, 6),
xmax = c(ymd("2016-04-23"), ymd("2016-05-11"), ymd("2016-05-15"), ymd("2016-05-15"), ymd("2016-06-21"), ymd("2016-06-21")),
ymin = rep(0, 6),
ymax = rep(8, 6))

climateplot <- climate2 %>% 
  left_join(box, by = "station") %>% 
  mutate(station = factor(station, levels = c("lugano", "feldberg", "bergen", "nesbyen", "tromso", "utsjoki"))) %>% 
  mutate(station = plyr::mapvalues(station, c("lugano", "feldberg", "bergen", "nesbyen", "tromso", "utsjoki"), c("a) Lugano (CH) 46.0°N 9.0°E 276.0m", "b) Feldberg (DE) 47.9°N 8.0°E 1490.0m", "c) Bergen (NO) 60.4°N 5.3°E 12.0m", "d) Nesbyen (NO) 60.6°N 9.1°E 167.0m", "e) Tromso (NO) 69.7°N 18.9°E 100.0m", "f) Utsjoki (SF) 69.8°N 27.0°E 107.0m"))) %>% 
  mutate(decade = plyr::mapvalues(decade, c("1960", "2000"), c("1960-1969", "2000-2009"))) %>% 
  ggplot(aes(x = date2, y = temperature, colour = decade, fill = decade)) +
  stat_summary(fun.data ="mean_sdl", geom = "smooth") + 
  #geom_point(alpha = 0.1) +  
  #geom_smooth() +
  geom_rect(xmin = -Inf, xmax = ymd("2016-04-23"), ymin = 0, ymax = 8, colour = muted("blue"), fill = NA) +
  facet_wrap(~station, nrow = 3) + 
  scale_color_manual(values = c("steelblue2", "tomato")) +
  #scale_fill_manual(values = c("steelblue2", "tomato")) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  labs(x = "", y = "Mean daily air temperature °C", colour = "Decade", fill = "Decade") +
  theme_bw()

climateplot +
  #geom_rect(xmin = -Inf, xmax = ymd("2016-04-23"), ymin = 0, ymax = 8, colour = muted("blue"), fill = NA)
  #geom_rect(xmin = -Inf, xmax = xmax, ymin = 0, ymax = 8, colour = muted("blue"), fill = NA)
  geom_rect(box, aes(x = NULL, y = NULL, xmin = -Inf, xmax = xmax, ymin = 0, ymax = 8, colour = muted("blue"), fill = NA))



ggplot(climate2, aes(x = date2, y = temperature, colour = decade)) +
  #stat_summary(fun.y = "mean", geom = "line") +  
  geom_smooth() + 
  geom_rect(xmin = -Inf, xmax = ymd("2016-04-22"), ymin = 0, ymax = 8, colour = muted("blue"),  inherit.aes = FALSE, fill = NA) + 
  facet_wrap(~station) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  labs(x = "", y = "Temperature °C", colour = "Decade")




# quantile regression for gam
library(qgam)

dd <- climate2 %>% filter(station == "feldberg", decade == "1960")

# Fit for quantile 0.5 using the best sigma
set.seed(6436)
fit <- qgam(temperature ~ s(date2), data = dd, qu = 0.5)
#fit <- qgam(temperature ~ s(date2, k=20, bs="ad"), data = dd, err = 0.05, qu = 0.5)

# Plot the fit
xSeq <- data_frame("date2" = unique(ymd(date2)))
pred <- predict(fit, newdata = xSeq, se=TRUE)
plot(dd$date2, dd$temperature, ylab = "Temperature")
lines(xSeq$date2, pred$fit, lwd = 1)
lines(xSeq$date2, pred$fit + 2*pred$se.fit, lwd = 1, col = 2)
lines(xSeq$date2, pred$fit - 2*pred$se.fit, lwd = 1, col = 2)

