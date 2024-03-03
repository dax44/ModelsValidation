library(reshape2)
library(tidyverse)
library(tidymodels)
library(plotly)
library(kernlab)
library(pracma) #For meshgrid()
data(iris)

mesh_size <- .02
margin <- 0
X <- iris %>% select(Sepal.Width, Sepal.Length)
y <- iris %>% select(Petal.Width)

model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression") %>% 
  fit(Petal.Width ~ Sepal.Width + Sepal.Length+ I(Sepal.Width^2) + I(Sepal.Length^2)+
        I(Sepal.Width^3) + I(Sepal.Length^3), data = iris)

x_min <- min(X$Sepal.Width) - margin
x_max <- max(X$Sepal.Width) - margin
y_min <- min(X$Sepal.Length) - margin
y_max <- max(X$Sepal.Length) - margin
xrange <- seq(x_min, x_max, mesh_size)
yrange <- seq(y_min, y_max, mesh_size)
xy <- meshgrid(x = xrange, y = yrange)
xx <- xy$X
yy <- xy$Y
dim_val <- dim(xx)
xx1 <- matrix(xx, length(xx), 1)
yy1 <- matrix(yy, length(yy), 1)
final <- bind_cols(Sepal.Width = xx1[,1], Sepal.Length = yy1[,1])
pred <- model %>%
  predict(final)

pred <- pred$.pred
pred <- matrix(pred, dim_val[1], dim_val[2])

fig <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length, z = ~Petal.Width ) %>% 
  add_markers(size = 5) %>% 
  add_surface(x=xrange, y=yrange, z=pred, alpha = 0.65, type = 'mesh3d', name = 'pred_surface')
fig
