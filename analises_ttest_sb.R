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
pvalue_dep <- format(testt_dep$p.value, digits = 1, scientific = FALSE)

if (pvalue_dep < 0.001) {
  pvalue_dep <- "< 0.0001"
}

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
  annotate("segment", x = 1.0, xend = 2.0, y = 28, yend = 28, color = "gray50", linewidth = 0.5, linetype = "solid") +
  annotate("text", x = 1.5, y = 30, label = paste("P =", pvalue_dep)) # nolint: line_length_linter.

dep

# anxiety -----------------------------------------------------------------------------------------
testt_anx <- t.test(ansiedade_score ~ sb_higher_6h, df)
pvalue_anx <- format(testt_anx$p.value, digits = 3, scientific = FALSE)

if (pvalue_anx < 0.001) {
  pvalue_anx <- "< 0.0001"
}

anx <- 
  df |> 
  tidyplot(x = sb_higher_6h, 
           y = ansiedade_score, 
           color = sb_higher_6h) |> 
  add_boxplot(show_outliers = FALSE) |> 
  add_mean_dot() +
  labs(
    y = "Beck Anxiety Inventory Score (a.u.)",
  ) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 30, 10)) +
  coord_cartesian(ylim = c(0, 30)) +
  annotate("segment", x = 1.0, xend = 2.0, y = 28, yend = 28, color = "gray50", linewidth = 0.5, linetype = "solid") +
  annotate("text", x = 1.5, y = 30, label = paste("P =", pvalue_anx)) # nolint: line_length_linter.

anx

testt_anx

# Layout ------------------------------------------------------------------
library(patchwork)
dep/anx
