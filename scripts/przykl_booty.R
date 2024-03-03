library(tidymodels)
dt <- data.frame(x = rnorm(50, 1, 2),
                 y = rnorm(50, 0, 2))
dt2 <- dt |> 
  mutate(id = 1:nrow(dt)) |> 
  pivot_longer(cols = c(x,y))

diff_boot <- dt2 |> 
  pivot_wider(names_from = name,
              values_from = value) |> 
  mutate(diff = x-y) |> 
  specify(response = diff) |> 
  hypothesise(null = "point", mu = 0) |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "mean")

diff_boot |> 
  visualise()

diff_obs <- dt2 |> 
  pivot_wider(names_from = name,
              values_from = value) |> 
  mutate(diff = x-y) |> 
  specify(response = diff) |> 
  hypothesise(null = "point", mu = 0) |>
  calculate(stat = "mean")

diff_boot |> 
  get_p_value(obs_stat = diff_obs, direction = "two-sided")

t.test(dt$x, dt$y)