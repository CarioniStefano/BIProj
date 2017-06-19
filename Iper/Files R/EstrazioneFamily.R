library(readr)
EstrazioneFamily <- read_csv("~/Downloads/Estrazione.csv","|")
View(EstrazioneFamily)

library(readr)
EstrazioneFamily <- read_delim("~/Downloads/Estrazione.csv", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
View(EstrazioneFamily)

table(EstrazioneFamily$ENTE)
table(EstrazioneFamily$PROMO_FL)
table(EstrazioneFamily$SETTORE)
table(EstrazioneFamily$REPARTO)
table(EstrazioneFamily$`REPARTO DESCRIZIONE`)
table(EstrazioneFamily$`SETTORE DESCRIZIONE`)
