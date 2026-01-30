# one_time_make_credentials.R
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, bcrypt, googledrive, googlesheets4)

# Step 1. Your plain-text roster (temporary)

read_csv('final_question_reveal/roster.csv') %>%
    rowwise() %>%
    mutate(pw_hash=bcrypt::hashpw(password)) %>%
    select(-password) %>%
    ungroup() %>%
    googlesheets4::sheet_write(sheet_id, sheet = "credentials")

# txt <- readChar("final_question_reveal/credentials.csv", file.info("final_question_reveal/credentials.csv")$size, useBytes = TRUE)
# txt_b64 <- base64enc::base64encode(charToRaw(txt))
# # add to Renviron
# Sys.setenv(CRED_B64=txt_b64)
# cred_b64 <- Sys.getenv("CRED_B64")
# cred_txt <- rawToChar(base64enc::base64decode(cred_b64))
# cred_df <- read_csv(cred_txt)