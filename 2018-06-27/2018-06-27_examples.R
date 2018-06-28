# 2018-06-20 R Learners examples
#
# Topics:
#   - re-coding / coding
#   - cleaning / regex
#   - reshaping
#   - merging / joining
#
# Cheatsheets at: https://www.rstudio.com/resources/cheatsheets/

library(stringr)
library(tidyverse)

experiment <- read_csv('http://www.maple-lab.org/r-sample-data.csv')

# review
filter(experiment, Age == 35)
experiment %>% filter(Age == 35) %>% select(ItemName)

experiment %>% mutate(RT_plus_10 = RT + 10)

# re-coding ---------------------------------------------------------------

experiment %>% mutate(
    is_plausible = recode(Condition, 'Implausible' = FALSE, 'Plausible' = TRUE)
  )

# coding ------------------------------------------------------------------

experiment %>%
  ggplot(aes(RT)) +
  geom_histogram(bins = 100)

experiment %>%
  mutate(RT_group = case_when(
      RT  > 1000 ~ 'High',
      RT <= 1000 ~ 'Normal')
  ) %>%
  select(RT, RT_group)

# cleaning ----------------------------------------------------------------

# digits
str_extract('abc_123', '[:digit:]')
str_extract('abc_123', '[:digit:]+')
str_extract('abc_123_def', '[:digit:]+')
str_extract('abc_123_def', '\\d+')        # '\\d' is shorthand for [:digit;]

# alphabet (leters A-Z, a-z)
str_extract('abc_123', '[:alpha:]')
str_extract('abc_123', '[:alpha:]+')
str_extract('ABC_123', '[:alpha:]+')
str_extract('abc_123_def', '[:alpha:]+')
str_extract_all('abc_123_def', '[:alpha:]+')

# anchors
str_extract("abc", "^[:alpha:]")
str_extract("abc", "[:alpha:]$")

# clean dataframe/tibble variable 
df <- tibble(
  id = c('subject_1', 'subject_2', 'subject_3'),
  col_to_extract = c('abc_123', 'abc_456', 'abc_789')
)

df %>% mutate(
    nums = str_extract(col_to_extract, '[:digit:]+'),
    chars = str_extract(col_to_extract, '[:alpha:]+') 
  )

df %>% mutate(nums = str_extract(col_to_extract, '1[:digit:]+'))

df %>% mutate(nums = str_extract(col_to_extract, '[14][:digit:]+'))

# look arounds
# (positive) look-behind: '_' must match, but not included in output 
str_extract("123_456", "(?<=_)[:digit:]+")
str_extract("123456", "(?<=_)[:digit:]+")

# (positive) look-ahead: '_' must match, but not included in output 
str_extract("123_456", "[:digit:]+(?=_)")
str_extract("123456", "[:digit:]+(?=_)")

# remove a pattern instead of extracting one
df %>% mutate(
    nums = str_remove(col_to_extract, '[:alpha:]+_'),
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
experiment_vars <- tibble(obs_id = 1:nrow(experiment))   # tibble/df
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
