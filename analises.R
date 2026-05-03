# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Ler a base e selecionar idosos --------------------------------------------------------------
df_leitura <- read_rds("df_para_analise.rds")

df <- 
  df_leitura  |> 
  filter(idade >= 60)

# analises exploratorias ----------------------------------------------------------------------

figura <- function(df, x_var, y_var){
  ggplot(df, aes(x = .data[[x_var]],
                 y = .data[[y_var]])) +
    geom_point() +
    geom_smooth(method = "lm")
}


variaveis <- c("ansiedade_score", "depressao_score")

for (v in variaveis) {
  print(figura(df, "mvpa_minday", v))
}



colnames(df)

# teste de github