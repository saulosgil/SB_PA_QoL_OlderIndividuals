# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Ler a base e selecionar idosos --------------------------------------------------------------
df_leitura <- read_rds("df_para_analise.rds")

df <- 
  df_leitura  |> 
  filter(idade >= 60)

# analises exploratorias ----------------------------------------------------------------------

figura <- function(df, x_var, y_var, diretorio = "."){
  plot <- ggplot(df, aes(x = .data[[x_var]],
                         y = .data[[y_var]])) +
    geom_point() +
    geom_smooth(method = "lm")

nome_arquivo <- file.path(diretorio, paste0("figura_", x_var, "_", y_var, ".png"))
ggsave(nome_arquivo, plot = plot, width = 8, height = 6, dpi = 300)

  plot
}

variaveis <- c("ansiedade_score", "depressao_score")

for (v in variaveis) {
  print(figura(df, "mvpa_minday", v))
}

colnames(df)

# teste de github