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
variaveis <- c("ansiedade_score",
               "depressao_score")


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
    sb_higher_6h = if_else(total_sb_hday >= 6, "Higher SB", "Lower SB")
  )

# count each category -------------------------------------------------------------------------
# Total
df |> 
  count(sb_higher_6h)

# analyses ------------------------------------------------------------------------------------
# depression -----------------------------------------------------------------------------------------
testt_dep <- t.test(depressao_score ~ sb_higher_6h, df)

dep <- 
  df |> 
  tidyplot(x = sb_higher_6h, 
           y = depressao_score, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot() +
  labs(
    y = "Beck Depression Inventory Score (a.u.)",
  ) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 30, 10)) +
  coord_cartesian(ylim = c(0, 30)) +
  geom_hline(yintercept = 28, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 1.5, y = 29, label = paste("p =", format(testt_dep$p.value, digits = 3)), 
           size = 3.5, color = "black")

dep

testt_dep

# anxiety -----------------------------------------------------------------------------------------
testt_anx <- t.test(ansiedade_score ~ sb_higher_6h, df)

anx <- 
  df |> 
  tidyplot(x = sb_higher_6h, 
           y = ansiedade_score, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot()|> 
  add_mean_dot() +
  labs(
    y = "Beck Anxiety Inventory Score (a.u.)",
  ) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 30, 10)) +
  coord_cartesian(ylim = c(0, 30)) +
  geom_hline(yintercept = 28, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 1.5, y = 29, label = paste("p =", format(testt_anx$p.value, digits = 3)), 
           size = 3.5, color = "black")

anx

testt_anx

# Layout ------------------------------------------------------------------
library(patchwork)
dep/anx

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





# Regression with significant outcome -------------------------------------------
# ----------
# Total SB 
# ----------
summary(lm(depressao_score ~ df$total_sb_hday,data = df))

# ----------
# MPSB 
# ----------
# Napping
summary(lm(depressao_score ~ df$total_soneca_minday,data = df))

# Listening to music
summary(lm(depressao_score ~ df$total_musica_minday,data = df))

# Watching television
summary(lm(depressao_score ~ df$total_tv_minday,data = df))

# Talking using or not a smartphone when sited
summary(lm(depressao_score ~ df$total_telefone_minday,data = df))

# Sit in car, bus or train
summary(lm(depressao_score ~ df$total_transporte_minday,data = df))

# Sit in the church or the theater
summary(lm(depressao_score ~ df$total_atvculturais_minday,data = df))











