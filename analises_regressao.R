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











