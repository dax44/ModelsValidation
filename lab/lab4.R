library(titanic)
library(infer)
library(dplyr)

# Zad1

data <- titanic_train %>%
  select(Survived) %>% 
  mutate(Survived = as.factor(Survived))

observed_stat <- data %>%
  specify(response = Survived, success = "1") %>%
  calculate(stat = "prop")

set.seed(44)  
bootstrap_distribution <- data %>%
  specify(response = Survived, success = "1") %>%
  hypothesize(null = "point", p = 0.35) %>%
  generate(reps = 1000, type = "draw") %>%
  calculate(stat = "prop")

p_value <- bootstrap_distribution %>%
  get_p_value(obs_stat = observed_stat, direction = "greater")

observed_stat
p_value

# Zad2

data("ToothGrowth")

observed_diff <- ToothGrowth %>%
  specify(len ~ supp) %>%
  calculate(stat = "diff in means", order = c("VC", "OJ"))

set.seed(44)
perm_distribution <- ToothGrowth %>%
  specify(len ~ supp) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("VC", "OJ"))

p_value <- perm_distribution %>%
  get_p_value(obs_stat = observed_diff, direction = "two-sided")

observed_diff
p_value

# Zad3

data("mtcars")

mtcars <- mtcars %>% 
  mutate(am = as.factor(am))

# Obserwowana różnica median
observed_diff_median <- mtcars %>%
  specify(mpg ~ am) %>%
  calculate(stat = "diff in medians", order = c("0", "1"))

set.seed(44)

# Wycentrowanie danych zgodnie z H0
mtcars_centered <- mtcars %>%
  group_by(am) %>%
  mutate(mpg_centered = mpg - median(mpg) + median(mtcars$mpg))

# Generowanie bootstrapowych prób zgodnych z hipotezą zerową
bootstrap_distribution <- mtcars_centered %>%
  ungroup() %>%
  specify(mpg_centered ~ am) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in medians", order = c("0", "1"))

p_value <- bootstrap_distribution %>%
  get_p_value(obs_stat = observed_diff_median, direction = "less")

# Wyświetlenie poprawnych wyników
observed_diff_median
p_value

# Zad4

observed_cor <- iris %>%
  specify(Sepal.Length ~ Sepal.Width) %>%
  calculate(stat = "correlation")

set.seed(44)
perm_distribution <- iris %>%
  specify(Sepal.Length ~ Sepal.Width) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "correlation")

p_value <- perm_distribution %>%
  get_p_value(obs_stat = observed_cor, direction = "greater")

observed_cor
p_value
