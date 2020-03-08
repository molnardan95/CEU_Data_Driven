
# Load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(janitor)
library(corrplot)

# Read Data ---------------------------------------------------------------

turnover <- read_excel("data.xlsx", sheet = "Turnover")
storeData <- read_excel("data.xlsx", sheet = "Store data")
costs <- read_excel("data.xlsx", sheet = "Costs")

turnover <- turnover %>% clean_names()
storeData <- storeData %>% clean_names()
costs <- costs %>% clean_names()

# Clean Data ---------------------------------------------------------------

unique(turnover$store_name)
storeData <- storeData %>% 
  filter(store_name != "#MISSING") %>% 
  mutate(store_name = as.numeric(store_name))

costs <- costs %>% 
  filter(store_name != "#MISSING") %>% 
  mutate(store_name = as.numeric(store_name))


storeData <- storeData %>% 
  mutate(sunday_opening_hours = ifelse(sunday_opening_hours == "#MISSING", 0, as.numeric(sunday_opening_hours)),
         real_estate_price_sqm = ifelse(real_estate_price_sqm == "#MISSING", 0, as.numeric(real_estate_price_sqm)),
         x0_50m_avg = ifelse(x0_50m_avg == "#MISSING", 0, as.numeric(x0_50m_avg)),
         x100_plusm_avg = ifelse(x100_plusm_avg == "#MISSING", 0, as.numeric(x100_plusm_avg)),
         newspaper_shelves = ifelse(newspaper_shelves == "#MISSING", 0, as.numeric(newspaper_shelves)),
         non_newspaper_shelves = ifelse(non_newspaper_shelves == "#MISSING", 0, as.numeric(non_newspaper_shelves)),
         x50_100m_avg = ifelse(x50_100m_avg == "#MISSING", 0, as.numeric(x50_100m_avg)))


# Join Data ---------------------------------------------------------------

data <- turnover %>% 
  left_join(storeData, by = "store_name") %>% 
  left_join(costs, by = c("store_name", "year"))

rm(costs, storeData, turnover)

# Data Manipulation -------------------------------------------------------

data <- data %>% 
  mutate(year_num = ifelse(year == "This year", 2019, 2018),
         month_chr = ifelse(month < 10, paste(0, month, sep = ""), month),
         date = paste(year_num, month_chr, sep = "-"),
         costs = -costs) 
  

# Data Analysis -----------------------------------------------------------

#unique(data$product_category)
gm_by_date <- data %>% 
  group_by(date) %>% 
  summarise(gm = sum(gm))

ggplot(gm_by_date, aes(date, gm)) +
  geom_bar(stat = 'identity')

# We can see an overally increasing trend in Gross Margin

gm_by_prod <- data %>% 
  group_by(date, product_category) %>% 
  summarise(gm = sum(gm))

ggplot(gm_by_prod, aes(date, gm)) +
  geom_bar(stat = 'identity') +
  facet_grid(cols = vars(product_category)) 
# This trend seems to be true for basically all the product categories as well

# YoY Trend
gm_by_store <- data %>% 
  group_by(year_num, store_name) %>% 
  summarise(gm = sum(gm)) %>% 
  spread(year_num, gm) %>% 
  mutate(change = `2019` - `2018`)

gm_change <- sum(gm_by_store$change)

costs_by_store <- data %>% 
  select(store_name, year_num, costs) %>% distinct() %>% 
  spread(year_num, costs) %>% 
  mutate(change = `2019` - `2018`)

cost_change <- sum(costs_by_store$change)

gm_change - cost_change

# Let's check the cost side of things

M <- cor(data %>% select_if(is.numeric))
corrplot(M, method = "shade")

test <- as.data.frame(M)
test <- test %>% filter(gm == 1)
test <- test %>% gather()


this_year_gm_by_prod <- data %>% 
  filter(year == "This year") %>% 
  group_by(month, product_category) %>% 
  summarise(gm = sum(gm))

ggplot(this_year_gm_by_prod, aes(month, gm)) +
  geom_line(stat = 'identity') +
  ylim(0, 12000000) +
  facet_grid(cols = vars(product_category)) +
  ggtitle("This Year's GM by Product")

last_year_gm_by_prod <- data %>% 
  filter(year == "Last year") %>% 
  group_by(month, product_category) %>% 
  summarise(gm = sum(gm))

ggplot(last_year_gm_by_prod, aes(month, gm)) +
  geom_line(stat = 'identity') +
  ylim(0, 12000000) +
  facet_grid(cols = vars(product_category)) +
  ggtitle("Last Year's GM by Product")

# 