## Parse des données CORE millésimées

# Librairies
library(tidyverse)
library(htm2txt)

# Import
core_millesime <- purrr::map(
        .x = (as.data.frame(rep(1:2303, each = 1)) %>% rename(page = `rep(1:2303, each = 1)`))$page,
        .y = data.frame(matrix(ncol = 1, nrow = 1)),
        possibly(.f = ~ as.data.frame(gettxt(paste0('http://portal.core.edu.au/conf-ranks/', .x))) %>% #import pag par page jà 2303
    rename(text = 1) %>% 
    mutate(text = strsplit(as.character(text), "\n")) %>% unnest(text) %>% #split les éléments séparés par des "\n"
    filter(row_number() == 10 | #nom de conférence
               grepl("Acronym:", text) == TRUE | 
               grepl("Source:", text) == TRUE | 
               grepl("Rank:", text) == TRUE, #champs que l'on garde
           grepl("DBLP", text) == FALSE) %>% #retrait de la ligne contenant ce string
    mutate(text = case_when(row_number() == 1 ~ paste("Title:", text), TRUE ~ text), #ajout du préfixe "titre:"
           champ = str_extract(text, "^[a-zA-Z0-9_]*"), #dans une nouvelle colonne ce qui est avant ":"
           value = str_extract(text, "(?<=: )[^\n]*")) %>% #dans une nouvelle colonne ce qui est après ": "
    select(-text) %>% t() %>% row_to_names(row_number = 1) %>% data.frame() %>% #transpose puis 1ère ligne en nom de colonnes
    pivot_longer(cols = -c(Title, Acronym), names_to = "number", values_to = "value", names_prefix = "Source|Rank") %>% # format long pour rank et source quand multiples
    mutate(col = case_when(row_number() %% 2 == 0 ~ "rank",
                           row_number() %% 2 == 1 ~ "source")) %>% #
    pivot_wider(names_from = col, values_from = value) %>% select(-number) %>% mutate(core_id = .x), otherwise = NA_character_),
        .default = NA)

# Gestion des Na et mise au format tabulaire
core_histo <- core_millesime[core_millesime != "NA"] # replace NA by NULL
core_histo <- rrapply::rrapply(core_histo, condition = Negate(is.null), how = "prune") #remove NULL
core_histo <- core_histo %>% bind_rows()

# Export
rio::export(core_histo, "data/core_millesime.csv")

