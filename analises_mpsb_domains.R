# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(DataExplorer)
library(tidyplots)

# Ler a base e selecionar idosos --------------------------------------------------------------
df_leitura <- read_rds("df_para_analise.rds")

df <- 
  df_leitura  |> 
  filter(idade >= 60)

colnames(df)

# Checking outcomes ---------------------------------------------------------------------------
# ajustado
df <- 
  df |> 
  mutate(
    hgs_max = if_else(hgs_max > 99, hgs_max/100, hgs_max),
    hgs_max = if_else(hgs_max < 5, mean(hgs_max), hgs_max)
  ) |>
  mutate(
    imc = imc/10,
    imc = if_else(imc < 20, mean(imc), imc)
  ) |> 
  filter(ts_max > 3 & ts_max < 30) |> 
  select(-tug_max)

# Missing  ----------------------------------------------------------------------
plot_missing(df)

# plots of each MPSB and depression -----------------------------------------------------------
# Fazer os plot de cada dominio de SB (categorizar pela mediana)
df <- 
  df |> 
  mutate(
    napping_mediana = ntile(total_soneca_minday,n = 2),
    napping_mediana = factor(
      napping_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    
  ) |> 
  mutate(
    
  ) |> 
  mutate(
    
  ) |> 
  mutate(
    
  ) |> 
  mutate(
    
  )

# Napping
napping <- 
  df |> 
  tidyplot(x = napping_mediana, 
           y = depressao_score, 
           color = napping_mediana) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()|> 
  add_mean_dot() +
  labs(
    y = "Beck Depression Inventory Score (a.u.)",
  ) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 30, 10)) +
  coord_cartesian(ylim = c(0, 30))

napping

testt <- t.test(depressao_score ~ napping_mediana, df)
testt
# Listening to music
# Watching television
# Talking using or not a smartphone when sited
# Sit in car, bus or train
# Sit in the church or the theater





