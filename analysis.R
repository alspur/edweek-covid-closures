# state_dist_closing.R

# 2020-03-30

# load ------------

library(tidyverse)
library(readxl)
library(googlesheets4)
library(sf)
library(urbnmapr)
library(lubridate)
library(scales)
library(gganimate)
library(janitor)

# load edweek spreadsheet of closure info circa 2020-03-30
edweek_raw <- read_excel("data/coronavirus-school-closures-data.xlsx",
                         skip = 1)

# cleaning edweek ---------

# tidy up the column names
colnames(edweek_raw) <- tolower(str_replace_all(colnames(edweek_raw), " ", "_"))

# rename cols & organize data
edweek_clean <- edweek_raw %>%
  rename(state_abbv = state_abbreviation) %>%
  rename(date_closed = state_closure_start_date) %>%
  select(state_abbv, date_closed) %>%
  mutate(date_closed = excel_numeric_to_date(as.numeric(date_closed)))

# join edweek data with state data from urbnmapr
edweek_states <- states %>%
  left_join(edweek_clean) 

# plot ---------------

# check the data
ggplot()+
  geom_polygon(data = edweek_states,
               mapping = aes(x = long, y = lat, group = group, 
                             fill = ),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# prep data for gif-ing
edweek_long <- edweek_states %>% 
  # create status for each state from 3-15 to 3-25
  mutate(march_15 = ifelse(date_closed <= ymd("2020-03-15"), "Closed", "Open"),
         march_16 = ifelse(date_closed <= ymd("2020-03-16"), "Closed", "Open"),
         march_17 = ifelse(date_closed <= ymd("2020-03-17"), "Closed", "Open"),
         march_18 = ifelse(date_closed <= ymd("2020-03-18"), "Closed", "Open"),
         march_19 = ifelse(date_closed <= ymd("2020-03-19"), "Closed", "Open"),
         march_20 = ifelse(date_closed <= ymd("2020-03-20"), "Closed", "Open"),
         march_21 = ifelse(date_closed <= ymd("2020-03-21"), "Closed", "Open"),
         march_22 = ifelse(date_closed <= ymd("2020-03-22"), "Closed", "Open"),
         march_23 = ifelse(date_closed <= ymd("2020-03-23"), "Closed", "Open"),
         march_24 = ifelse(date_closed <= ymd("2020-03-24"), "Closed", "Open"),
         march_25 = ifelse(date_closed <= ymd("2020-03-25"), "Closed", "Open")) %>%
  # reshape from wide to long
  pivot_longer(cols = starts_with("march"),
               names_to = "time", values_to = "status") %>%
  # convert character date to POSIXct format
  mutate(time = str_replace(time, "march_", "2020-03-"))%>%
  mutate(time = ymd(time)) %>%
  # create special status cases for states w/o blanket closures
  mutate(status = ifelse(state_abbv == "IA", "Open", status)) %>%
  mutate(status = ifelse(state_abbv == "NE", "Open", status)) %>%
  mutate(status = ifelse(state_abbv == "ME", "Open", status)) %>%
  mutate(status = factor(status, levels = c("Open", "Closed"))) %>%
  # sort by date
  arrange(time)



# edweek gif ------------------

# create map gif
anim <- ggplot() + 
  # plot the state data
  geom_polygon(data = edweek_long,
               mapping = aes(x = long, y = lat,
                             group = group, fill = status),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = c("#4b7e9a", "#953545")) +
  labs(title = "Covid-19 School Closures", 
       fill = "Status",
       size = "Enrollment",
       subtitle = "Date: {format(ymd(current_frame), \"%B %d\")}",
       caption = "Data via Education Week | gif by @alspur")+
  theme_void()+
  theme(legend.position = c(.9,.4))+
  transition_manual(time)

# animate gif
animate(anim, fps = 6)

# safe gif
anim_save("covid_state_closures.gif")
