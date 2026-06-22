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
# MPSB
df <- 
  df |> 
  mutate(
    napping_mediana = ntile(total_soneca_minday,n = 2), # napping
    napping_mediana = factor(
      napping_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    musica_mediana = ntile(total_musica_minday,n = 2), # listening to music
    musica_mediana = factor(
      musica_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    tv_mediana = ntile(total_tv_minday,n = 2), # Watching television
    tv_mediana = factor(
      tv_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    telefone_mediana = ntile(total_telefone_minday,n = 2), # Talking using or not a smartphone when sited
    telefone_mediana = factor(
      telefone_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    transporte_mediana = ntile(total_transporte_minday,n = 2), # Sit in car, bus or train
    transporte_mediana = factor(
      transporte_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    atvculturais_mediana = ntile(total_atvculturais_minday,n = 2), # Sit in the church or the theater
    atvculturais_mediana = factor(
      atvculturais_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  )

# MASB
df <- 
  df |> 
  mutate(
    leitura_mediana = ntile(total_leitura_minday,n = 2), # Reading
    leitura_mediana = factor(
      leitura_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    jogos_mediana = ntile(total_jogos_minday,n = 2), # Perform a hobby while being seated
    jogos_mediana = factor(
      jogos_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    pc_mediana = ntile(total_pc_minday,n = 2), # Use the computer
    pc_mediana = factor(
      pc_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  ) |> 
  mutate(
    atvdomesticas_mediana = ntile(total_atvdomesticas_minday,n = 2), # Administrative activities
    atvdomesticas_mediana = factor(
      atvdomesticas_mediana,
      levels = 1:2,
      labels = c("Lower", "Higher")
    )
  )

# Depressão  
# Função com teste t independente -------------------------------------------------------------
plot_sb_ttest <- function(data, var_mediana, y_var = depressao_score,
                          x_label = NULL, y_label = NULL) {
  
  var_mediana_sym <- rlang::ensym(var_mediana)
  y_var_sym       <- rlang::ensym(y_var)
  
  x_label <- x_label %||% rlang::as_label(var_mediana_sym)
  y_label <- y_label %||% "Beck Depression Inventory Score (a.u.)"
  
  # Teste t independente
  grupo  <- data[[rlang::as_label(var_mediana_sym)]]
  outcome <- data[[rlang::as_label(y_var_sym)]]
  
  ttest  <- t.test(outcome ~ grupo, data = data)
  p_val  <- ttest$p.value
  
  # Formatar p-value igual ao exemplo
  p_label <- if (p_val < 0.0001) {
    "P = < 0.0001"
  } else {
    paste0("P = ", formatC(p_val, digits = 4, format = "f"))
  }
  
  data |> 
    tidyplot(
      x     = !!var_mediana_sym,
      y     = !!y_var_sym,
      color = !!var_mediana_sym
    ) |>
    add_boxplot(show_outliers = FALSE) |>
    add_mean_dot() +
    labs(
      # title = p_label,
      x     = x_label,
      y     = y_label
    ) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 13, face = "bold"),
      legend.title = element_blank()
    ) +
    scale_y_continuous(breaks = seq(0, 30, 10)) +
    coord_cartesian(ylim = c(0, 30)) +
    annotate("segment", x = 1.0, xend = 2.0, y = 28, yend = 28, color = "gray50", linewidth = 0.5, linetype = "solid") +
    annotate("text", x = 1.5, y = 30, label = paste("", p_label)) # nolint: line_length_linter.
}

# Domínios ------------------------------------------------------------------------------------
dominios <- list(
  # MPSB
  list(var = "napping_mediana",       label = "Napping"),
  list(var = "musica_mediana",        label = "Listening to Music"),
  list(var = "tv_mediana",            label = "Watching Television"),
  list(var = "telefone_mediana",      label = "Talking on Phone"),
  list(var = "transporte_mediana",    label = "Sitting in Transport"),
  list(var = "atvculturais_mediana",  label = "Cultural Activities"),
  # MASB
  list(var = "leitura_mediana",       label = "Reading"),
  list(var = "jogos_mediana",         label = "Hobbies (Seated)"),
  list(var = "pc_mediana",            label = "Computer Use"),
  list(var = "atvdomesticas_mediana", label = "Administrative Activities")
)

# Gerar plots ---------------------------------------------------------------------------------
plots_sb <- map(dominios, \(d) {
  plot_sb_ttest(
    data        = df,
    var_mediana = !!rlang::sym(d$var),
    y_var       = depressao_score,
    x_label     = d$label
  )
})

names(plots_sb) <- map_chr(dominios, "var")

# Painéis -------------------------------------------------------------------------------------
painel_mpsb <- 
  plots_sb$napping_mediana + plots_sb$musica_mediana +
  plots_sb$tv_mediana      + plots_sb$telefone_mediana +
  plots_sb$transporte_mediana + plots_sb$atvculturais_mediana +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Mentally-Passive Sedentary Behaviors",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

painel_masb <- 
  plots_sb$leitura_mediana + plots_sb$jogos_mediana +
  plots_sb$pc_mediana      + plots_sb$atvdomesticas_mediana +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Mentally-Active Sedentary Behaviors",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

painel_mpsb
painel_masb