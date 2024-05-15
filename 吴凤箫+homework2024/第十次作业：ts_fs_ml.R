# 04- machine learning for time series 
# https://www.r-bloggers.com/2022/01/time-series-forecasting-lab-part-3-machine-learning-with-workflows/
# A) load packages and data

library(tidyverse)  
library(timetk) 
library(tidymodels)
library(modeltime)
library(timetk)

mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI")

biomtk_ts <- mydata |> # Convert to tibble
  tk_tbl() |> 
  select(index, DATE, BIOMASS) # keep date and target

# biomtk_ts |>
#   plot_time_series(DATE, BIOMASS,
#                    .facet_ncol  = NULL,
#                    .smooth      = FALSE,
#                    .interactive = TRUE,
#                    .title = "Biomass timeseries")

library(tidyquant)
ggplot(biomtk_ts, aes(x = DATE, y = BIOMASS)) +
  geom_line() +
  ggtitle("Biomass of Fishes in Doubs")

##---------------------------------------------------
# B) Train/Test Splitting and creating features

# splits <- biomtk_ts |>
#   time_series_split(DATE,
#                     assess = "3 year", 
#                     cumulative = TRUE)
# 
# splits

n_rows <- nrow(biomtk_ts)
train_rows <- round(0.8 * n_rows)

train_data <- biomtk_ts |>
  slice(1:train_rows) # slice() from dplyr
test_data <- biomtk_ts |>
  slice((train_rows):n_rows)

ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

# creating features with recipes

library(recipes)
library(tidymodels)

recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>
  step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(recipe_spec_final))

