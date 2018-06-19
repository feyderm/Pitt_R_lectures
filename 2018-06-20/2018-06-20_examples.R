# 2018-06-20 R Learners examples
#
# Preview of the dplyr and ggplot2 packages, both part of the tidyvserse.
# Cheatsheets at:
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

library(tidyverse)

experiment <- read_csv('http://www.maple-lab.org/r-sample-data.csv')

# subsetting observations / logical indexing ------------------------------

# base R
subset(experiment, TestingRoom == 3)

# tidyverse
experiment %>% filter(TestingRoom == 3)

# subsetting variables ----------------------------------------------------

experiment %>% select(Subject, Age, RT)

# piping ------------------------------------------------------------------

experiment %>%
  filter(TestingRoom == 3) %>%
  select(Subject, Age, RT)

# creating new variables + assignment -------------------------------------

# base R
experiment$log_RT_baseR <- log(experiment$RT)

# tidyverse
experiment <- experiment %>% mutate(log_RT_tidy = log(RT))

View(experiment)

# summary stats (global) --------------------------------------------------

# base R
mean(experiment$RT)

# tidyverse
experiment %>% summarize(mean_RT = mean(RT))

# summary stats (grouped) -------------------------------------------------

# base R
tapply(experiment$RT, experiment$SerialPosition, mean)

# tidyverse
experiment %>%
  group_by(SerialPosition) %>%
  summarize(mean_RT = mean(RT))

# tidyverse (nested groups)
experiment %>%
  group_by(SerialPosition, Condition) %>%
  summarize(mean_RT = mean(RT))

# summary stats (multiple) ------------------------------------------------

# tidyverse
experiment %>%
  group_by(SerialPosition) %>%
  summarize(
    n = n(),
    min_RT = min(RT),
    max_RT = max(RT),
    mean_RT = mean(RT),
    median_RT = median(RT),
    mode_RT = mode(RT),
    sd_RT = sd(RT),
    var_RT = var(RT),
    has_NA = any(is.na(RT))
  )

# rename column(s) --------------------------------------------------------

experiment <- experiment %>% rename(item_name = ItemName)

# sorting -----------------------------------------------------------------

# ascending order
experiment %>% arrange(RT)

# decending order
experiment %>% arrange(desc(RT))

# histogram ---------------------------------------------------------------

# base R
hist(experiment$RT)

# ggplot
ggplot(data = experiment, mapping = aes(x = RT)) +
  geom_histogram()

# scatterplot -------------------------------------------------------------

# base R
plot(experiment$Age, experiment$RT)

# ggplot
ggplot(data = experiment, mapping = aes(x = Age, y = RT)) +
  geom_point()

# bar chart ---------------------------------------------------------------
# NOTE: geom_bar (n. observations) vs geom_col (variable values)

experiment %>%
  group_by(Age) %>%
  mutate(mean_RT = mean(RT)) %>%
  ggplot(mapping = aes(x = Age, y = mean_RT)) +
  geom_col()

ggplot(data = experiment, mapping = aes(x = Age)) +
  geom_bar()

# ggplot geoms -----------------------------------------------------------

ggplot(data = experiment, mapping = aes(Age, RT)) +
  geom_point()

ggplot(data = experiment, mapping = aes(Age, RT)) +
  geom_boxplot()

ggplot(data = experiment, mapping = aes(Age, RT)) +
  geom_violin()

# ggplot facets -----------------------------------------------------------

ggplot(data = experiment, mapping = aes(Age, RT)) +
  geom_point() + 
  facet_wrap(~TestingRoom)
