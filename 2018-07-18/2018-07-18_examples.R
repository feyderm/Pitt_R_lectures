# 2018-06-20 R Learners examples
#
# Topics:
#   - joins
#
# Cheatsheets at: https://www.rstudio.com/resources/cheatsheets/

library(tidyverse)

# Joins -------------------------------------------------------------------

X <- tibble(
  id = c('A', 'B', 'C'),
  var_1 = c(1, 2, 3)
)

Y <- tibble(
  id = c('A', 'B', 'D'),
  var_2 = c(TRUE, TRUE, FALSE)
)

left_join(X, Y, by = 'id')
right_join(X, Y, by = 'id')
inner_join(X, Y, by = 'id')
full_join(X, Y, by = 'id')


# statistical tests -------------------------------------------------------

library(broom)
set.seed(10)

# t-test
df_ttest <- tibble(
  obs_1 = rnorm(50, mean = 0, sd = 1),
  obs_2 = rnorm(50, mean = 3, sd = 1)
)

ggplot(data = df_ttest) +
  geom_histogram(aes(obs_1), color = 'purple', alpha = 0.4) +
  geom_histogram(aes(obs_2), color = 'gold', alpha = 0.4) +
  theme_minimal()

ttest_default <- t.test(df_ttest$obs_1, df_ttest$obs_2)
ttest_default

str(ttest_default)
ttest_default$statistic
ttest_default$p.value

ttest_results <- broom::tidy(ttest_default)

ttest_var_equal <- t.test(df_ttest$obs_1, df_ttest$obs_2, var.equal = TRUE)
ttest_var_equal

ttest_results <- broom::tidy(ttest_var_equal) %>% bind_rows(ttest_results)

# one-way ANOVA
mtcars

ggplot(data = mtcars) +
  geom_point(aes(x = gear, y = mpg))

aov_1 <- aov(mpg ~ as.factor(gear), data = mtcars)
summary(aov_1)

pairwise.t.test(x = mtcars$mpg, g = mtcars$gear, p.adjust.method = 'bonferroni')
pairwise.t.test(x = mtcars$mpg, g = mtcars$gear, p.adjust.method = 'fdr')
TukeyHSD(aov_1)

# two-way ANOVA 
experiment <- read_csv('http://www.maple-lab.org/r-sample-data.csv')

ggplot(data = experiment) +
  geom_jitter(aes(x = TestingRoom, y = RT), alpha = 0.5) +
  facet_grid(~Condition)

aov_2 <- aov(
  RT ~ as.factor(TestingRoom) * as.factor(Condition),
  data = experiment
)

summary(aov_2)
TukeyHSD(aov_2)

# linear regression
ggplot(data = mtcars) +
  geom_point(aes(x = disp, y = mpg)) +
  geom_smooth(aes(x = disp, y = mpg))

lm_mtcars <- lm(mpg ~ disp, data = mtcars)
summary(lm_mtcars)
