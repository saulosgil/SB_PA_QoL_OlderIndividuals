# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(sjPlot)
library(broom)
library(gtsummary)

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

# Ajuste nas covariaveis para colocar como fator --------------------------
df <- df |> 
  mutate(
    raca  = as.factor(raca),
    genero = as.factor(genero),
    estado_civil  = as.factor(estado_civil),
    obesidade = as.factor(obesidade),
    has = as.factor(has),
    dm2 = as.factor(dm2)
  )

# Função: regressão linear por domínio --------------------------------------------------------
run_sb_regression <- function(data, var_sb, outcome = "ansiedade_score",
                              covariates = c("idade",
                                             "raca",
                                             "genero",
                                             "estado_civil",
                                             "obesidade",
                                             "has",
                                             "dm2")) {
  formula_obj <- as.formula(
    paste(outcome, "~", var_sb, "+", paste(covariates, collapse = " + "))
  )
  lm(formula_obj, data = data)
}

# Domínios ------------------------------------------------------------------------------------
dominios_vars <- list(
  napping       = "napping_mediana",
  musica        = "musica_mediana",
  tv            = "tv_mediana",
  telefone      = "telefone_mediana",
  transporte    = "transporte_mediana",
  atvculturais  = "atvculturais_mediana",
  leitura       = "leitura_mediana",
  jogos         = "jogos_mediana",
  pc            = "pc_mediana",
  atvdomesticas = "atvdomesticas_mediana"
)

dominios_labels <- list(
  napping       = "Napping",
  musica        = "Listening to Music",
  tv            = "Watching Television",
  telefone      = "Talking on Phone",
  transporte    = "Sitting in Transport",
  atvculturais  = "Cultural Activities",
  leitura       = "Reading",
  jogos         = "Hobbies (Seated)",
  pc            = "Computer Use",
  atvdomesticas = "Administrative Activities"
)

mpsb_vars <- c("napping", "musica", "tv", "telefone", "transporte", "atvculturais")
masb_vars <- c("leitura", "jogos", "pc", "atvdomesticas")

# Rodar todos os modelos ----------------------------------------------------------------------
modelos_sb <- imap(dominios_vars, \(var, nome) {
  run_sb_regression(data = df, var_sb = var)
})

# Verificar se todos rodaram ------------------------------------------------------------------
walk2(modelos_sb, names(modelos_sb), \(m, nome) {
  if (!inherits(m, "lm")) stop(paste("Modelo falhou:", nome))
  cat("OK:", nome, "| n =", nobs(m), "| R2 =", round(summary(m)$r.squared, 3), "\n")
})

# Loop para apresentar tab_model de cada modelo -----------------------------------------------
# Salvar cada tab_model em uma lista ----------------------------------------------------------
tabs_sb <- vector("list", length(modelos_sb))
names(tabs_sb) <- names(modelos_sb)

Sys.setlocale("LC_ALL", "C")  # forçar o locale para UTF-8 antes do loop para resolver o erro de encoding/locale do sjPlot::tab_model

for (i in seq_along(modelos_sb)) {
  nome  <- names(modelos_sb)[i]
  label <- dominios_labels[[nome]]
  
  tabs_sb[[nome]] <- tab_model(
    modelos_sb[[i]],
    dv.labels = label
  )
}

# Visualizar individualmente ------------------------------------------------------------------
tabs_sb$napping
tabs_sb$musica
tabs_sb$tv
tabs_sb$telefone
tabs_sb$transporte
tabs_sb$atvculturais
tabs_sb$leitura
tabs_sb$jogos
tabs_sb$pc
tabs_sb$atvdomesticas












