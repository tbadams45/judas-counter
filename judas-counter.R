library(tidyverse)

files <- dir("./data/raw")
files <- tibble(names = files, year = as.double(stringr::str_sub(files, start = 4, end = 7)))

data <- map2_df(.x = files$names, .y = files$year, .f = function(fp, year) {
  my_df <- read_csv(paste0("./data/raw/", fp), col_names = c("name", "sex", "number"), col_types = "ccd")
  my_df$year <- year
  my_df <- my_df %>%
    mutate(rank_in_year = row_number())
})

data_judas <- data %>%
  filter(name == "Judas")

min(data$year)
max(data$year)

popularity_over_all_time <- data %>%
  group_by(name) %>%
  summarize(number = sum(number)) %>%
  arrange(desc(number)) %>%
  mutate(rank = row_number())

popularity_over_all_time %>% filter(name == "Judas")

# % of babies named Judas
429 / sum(popularity_over_all_time$number) * 100

# number of babies named Judas per million babies 
429 / sum(popularity_over_all_time$number) * 1e6

ggplot(data_judas, aes(x = year, y = number)) + 
  geom_point()
