
library(tidyverse)
library(here)
library(purrr)
message <- read_csv(here::here("Week #8", "mystery_animal.csv")
)

message_lm <- message |>
  lm(weight_after ~ weight_before, 
     data = _)

message_lm |>
  broom::augment() |> 
  ggplot(mapping = aes(y = .resid, x = .fitted)) +
  geom_point()

qunif(.95, min = 1.5, max = 3.5)
qnorm(.10, mean = 4.6, sd = .8)
pnorm(5, mean = 4.6, sd = 0.8, lower.tail = FALSE)



set.seed(1957)

music_man <- function(n_tromb, n_cor, n_reed){
  
  trombones <- rnorm(n_tromb, mean = 4.6, sd = .8)
  cornets <- runif(n_cor, min = 1.5, max = 3.5)
  reeds <- rchisq(n_reed, df = 4)
  
  return(sum(trombones, cornets, reeds))
  
}

my_weights <- map_dbl(.x = 1:1000, 
                      .f = ~ music_man(n_tromb = 76, n_cor = 110, n_reed = 1035)
) 

filter_func <- function(x) {
  x < 4532 
}

filtered_list <- keep(my_weights, filter_func)
