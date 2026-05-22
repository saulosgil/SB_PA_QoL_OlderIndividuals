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

# variáveis que irão no eixo Y
variaveis <- c("hgs_max", 
               "ts_max", 
               "ansiedade_score",
               "depressao_score",
               "whoqol_fisico_escore_100",
               "whoqol_psicol_escore_100",
               "whoqol_social_escore_100",
               "whoqol_ambiente_escore_100")


for (y_var in variaveis) {
  
  # fórmula da regressão
  formula <- as.formula(paste(y_var, "~ total_sb_hday"))
  
  # gráfico
  plot(df$total_sb_hday,
       df[[y_var]],
       main = paste("Regressão Linear -", y_var),
       xlab = "total_sb_hday",
       ylab = y_var,
       pch = 19)
  
  # modelo
  modelo <- lm(formula, data = df)
  
  # linha de regressão
  abline(modelo,
         col = "red",
         lwd = 2)
}

# SB categories -------------------------------------------------------------------------------
df <- 
  df |> 
  mutate(
    sb_higher_6h = if_else(total_sb_hday >= 6, "highersb", "lowersb")
  )

# count each category -------------------------------------------------------------------------
# Total
df |> 
  count(sb_higher_6h)

# analyses ------------------------------------------------------------------------------------
# HGS -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = hgs_max, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(hgs_max ~ sb_higher_6h, df)
testt

# TS -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = ts_max, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()


testt <- t.test(ts_max ~ sb_higher_6h, df)
testt

# depression -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = depressao_score, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(depressao_score ~ sb_higher_6h, df)
testt

# anxiety -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = ansiedade_score, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(ansiedade_score ~ sb_higher_6h, df)
testt

# WHO - fisico -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = whoqol_fisico_escore_100, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(whoqol_fisico_escore_100 ~ sb_higher_6h, df)
testt

# WHO - mental -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = whoqol_psicol_escore_100, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(whoqol_psicol_escore_100 ~ sb_higher_6h, df)
testt

# WHO - social -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = whoqol_social_escore_100, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(whoqol_social_escore_100 ~ sb_higher_6h, df)
testt

# WHO - ambiente -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = whoqol_ambiente_escore_100, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()

testt <- t.test(whoqol_ambiente_escore_100 ~ sb_higher_6h, df)
testt

# Regression with significant outcome -------------------------------------------
# ----------
# Total SB 
# ----------
summary(lm(depressao_score ~ df$total_sb_hday,data = df))

# ----------
# MPSB 
# ----------
# Napping
# Listening to music
# Watching television
# Talking using or not a smartphone when sited
# Sit in car, bus or train
# Sit in the church or the theater

summary(lm(depressao_score ~ df$total_soneca_minday,data = df))

summary(lm(depressao_score ~ df$total_musica_minday,data = df))

summary(lm(depressao_score ~ df$total_tv_minday,data = df))

summary(lm(depressao_score ~ df$total_telefone_minday,data = df))

summary(lm(depressao_score ~ df$total_transporte_minday,data = df))

summary(lm(depressao_score ~ df$total_atvculturais_minday,data = df))





