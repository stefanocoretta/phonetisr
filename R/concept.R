library(tidyverse)
library(Unicode)

db <- c("ɑʃʊ̃ɲtʃuɪ", "ɡom̥u")

ph <- c("ʊ̃", "tʃ", "m̥")

ph_length <- length(ph)

pua <- intToUtf8(as.u_char_seq(u_blocks("Private Use Area")[[1]])[[1]][1:ph_length], multiple = T)

names(pua) <- ph

db_pua <- lapply(db, function(.x) str_replace_all(.x, pua))

db_pua_token <- str_split(db_pua, "")

ipa <- ph
names(ipa) <- pua

db_ipa <- lapply(db_pua_token, function(.x) str_replace_all(.x, ipa))
db_ipa
