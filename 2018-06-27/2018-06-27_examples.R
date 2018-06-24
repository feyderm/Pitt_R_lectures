# 2018-06-20 R Learners examples
#
# Topics:
#   - coding / re-coding
#   - cleaning
#   - reshaping
#   - merging / joining
#
# Cheatsheets at: https://www.rstudio.com/resources/cheatsheets/

library(stringr)
library(tidyverse)

experiment <- read_csv('http://www.maple-lab.org/r-sample-data.csv')

# coding / re-coding ---------------------------------------------------------------

experiment %>%
  mutate(
    is_plausible = recode(Condition, 'Implausible' = FALSE, 'Plausible' = TRUE )
  )

experiment %>%
  ggplot(aes(RT)) +
  geom_histogram(bins = 100)

experiment %>%
  mutate(RT_group = case_when(
      RT  > 1000 ~ 'High',
      RT <= 1000 ~ 'Normal')
  )

# cleaning ----------------------------------------------------------------

str_extract('xxx_123', '123')
str_extract('xxx_123', '[:digit:]')
str_extract('xxx_123', '[:digit:]+')
str_extract('xxx_123', '[:alpha:]')
str_extract('xxx_123', '[:alpha:]+')

df <- tibble(
  id = c('subject_1', 'subject_2', 'subject_3'),
  col_to_extract = c('xxx_123', 'xxx_456', 'xxx_789')
)

df %>% mutate(
    nums = str_extract(col_to_extract, '[:digit:]+'),
    chars = str_extract(col_to_extract, '[:alpha:]+') 
  )

df %>% mutate(nums = str_extract(col_to_extract, '1[:digit:]+'))

df %>% mutate(nums = str_extract(col_to_extract, '[14][:digit:]+'))

df %>% mutate(
    nums = str_remove(col_to_extract, 'xxx_'),
    chars = str_remove(col_to_extract, '_[:digit:]+')
  )

# reshaping ---------------------------------------------------------------

df <- tibble(
  country = c('A', 'B', 'C'),
  `1999` = c('0.7K', '37K', '212K'),
  `2000` = c('2K', '80K', '213K')
)

df_gathered <- df %>% gather(`1999`, `2000`, key = "year", value = "cases")

df_gathered %>% spread(key = "year", value = "cases") 

# merging / joining -------------------------------------------------------

# add variables
experiment_vars <- tibble(obs_id = 1:nrow(experiment))
experiment %>% bind_cols(experiment_vars)

# add observations
experiment_obs <- tibble(
  Subject = 'S37',
  Age = '44',
  ItemName = 'Teacher',
  Condition = 'Plausible',
  TestingRoom = 3,
  SerialPosition = 1,
  RT = 654
)

experiment %>% bind_rows(experiment_obs)

# joins
X <- tibble(
  id = c('A', 'B', 'C'),
  var_1 = c(1, 2, 3)
)

Y <- tibble(
  id = c('A', 'B', 'D'),
  var_2 = c(TRUE, TRUE, FALSE)
)

X %>% full_join(Y, by = 'id')
X %>% left_join(Y, by = 'id')
X %>% right_join(Y, by = 'id')
X %>% inner_join(Y, by = 'id')
