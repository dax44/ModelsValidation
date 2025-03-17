library(tidymodels)

set.seed(44)
x1 <- rchisq(20, 2)
x2 <- -rchisq(15, 2)+10

dt <- tibble(x = c(x1,x2)) |> 
  mutate(gr = rep(c("A", "B"), times = c(20,15)))

dt |> 
  ggplot(aes(x, fill = gr))+
  geom_density(alpha = 0.6)+
  xlim(c(-2, 12))+
  scale_fill_nord(palette = "victory_bonds")+
  theme_minimal()

sample_diff <- dt |> 
  specify(x~gr) |> 
  calculate(stat = "diff in medians", order = c("A","B"))
sample_diff

set.seed(44)
null_diff <- dt |> 
  specify(x~gr) |> 
  hypothesise(null = "independence") |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "diff in medians", order = c("A","B"))

null_diff

ci <- null_diff |> 
  get_confidence_interval(level = .95, type = "percentile")

null_diff |> 
  visualise()+
  shade_ci(endpoints = ci)+
  geom_vline(xintercept = 0, 
             linewidth = 2,
             color = "red")

obs_diff <- dt |> 
  specify(x ~ gr) |> 
  calculate(stat = "diff in medians", order = c("A", "B")) |> 
  pull()

# Mediana globalna
global_median <- median(dt$x)

# Mediana każdej grupy
median_A <- median(dt$x[dt$gr == "A"])
median_B <- median(dt$x[dt$gr == "B"])

# Skorygowanie danych przez przesunięcie median do globalnej mediany
dt_null <- dt |>
  mutate(x_null = case_when(
    gr == "A" ~ x - median_A + global_median,
    gr == "B" ~ x - median_B + global_median
  ))

null_bootstrap <- dt_null |> 
  specify(x_null ~ gr) |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "diff in medians", order = c("A", "B"))

null_bootstrap


p_value <- null_bootstrap |> 
  get_p_value(obs_stat = obs_diff, direction = "two-sided")

p_value

null_bootstrap |> 
  visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")
