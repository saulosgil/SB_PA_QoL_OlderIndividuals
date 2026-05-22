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
readr::write_csv(df, "data_exploratory.csv")

# Missing  ----------------------------------------------------------------------
plot_missing(df)

# SB categories -------------------------------------------------------------------------------
df <- 
  df |> 
  mutate(
    mpsb_higher_6h = if_else(total_sb_mp_hday >= 6, "highermpsb", "lowermpsb"),
    masb_higher_6h = if_else(total_sb_ma_hday >= 6, "highermpsb", "lowermpsb"),
    sb_higher_6h = if_else(total_sb_hday >= 6, "highersb", "lowersb")
  )

# count each category -------------------------------------------------------------------------
# Total
df |> 
  count(sb_higher_6h)

# MPSB
df |> 
  count(mpsb_higher_6h)

# MSSB
df |> 
  count(masb_higher_6h)

# analyses ------------------------------------------------------------------------------------
# HGS -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = mpsb_higher_6h, 
           y = hgs_max, 
           color = mpsb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(hgs_max ~ mpsb_higher_6h, df)
testt

# TUG -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = quartil_sedentario, 
           y = tug_max, 
           color = quartil_sedentario) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(tug_max ~ quartil_sedentario, df)
testt

# TS -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = mpsb_higher_6h, 
           y = ts_max, 
           color = mpsb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(ts_max ~ mpsb_higher_6h, df)
testt

# depression -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = mpsb_higher_6h, 
           y = depressao_score, 
           color = mpsb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(depressao_score ~ mpsb_higher_6h, df)
testt

# anxiety -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = mpsb_higher_6h, 
           y = ansiedade_score, 
           color = mpsb_higher_6h) |> 
  add_boxplot()

testt <- t.test(ansiedade_score ~ mpsb_higher_6h, df)
testt

# WHO - fisico -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = whoqol_fisico_escore_100, 
           color = sb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(whoqol_fisico_escore_100 ~ sb_higher_6h, df)
testt

# WHO - mental -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = sb_higher_6h, 
           y = whoqol_psicol_escore_100, 
           color = sb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(whoqol_psicol_escore_100 ~ sb_higher_6h, df)
testt

# WHO - social -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = mpsb_higher_6h, 
           y = whoqol_social_escore_100, 
           color = mpsb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(whoqol_social_escore_100 ~ mpsb_higher_6h, df)
testt

# WHO - ambiente -----------------------------------------------------------------------------------------
df |> 
  tidyplot(x = mpsb_higher_6h, 
           y = whoqol_ambiente_escore_100, 
           color = mpsb_higher_6h) |> 
  add_boxplot() |> 
  add_median_dot()

testt <- t.test(whoqol_ambiente_escore_100 ~ mpsb_higher_6h, df)
testt

df <- df |> 
  mutate(
    quartil_sedentario = ntile(total_sb_hday, 3),
    quartil_sedentario = factor(
      quartil_sedentario,
      levels = 1:3,
      labels = c("Q1 - Menor tempo",
                 "Q2",
                 "Q3")
    )
  )

df$quartil_sedentario
