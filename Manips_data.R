# DESEMBIGUISATION DES INTITULES DE CONFERENCES


# Librairies
library(tidyverse)
#remotes::install_github("skgrange/threadr")
library(threadr) # import bibTex
library(rvest) # read_html
library(fuzzyjoin)
reticulate::py_config()
library(fuzzywuzzyR) #reticulate::py_config() à runer avant d'appeler la library


# Import données 
    # HAL (bibTex)
hal1 <- read_bibtex("https://api.archives-ouvertes.fr/search/?q=+publicationDateY_i%3A[2018+TO+2020]+AND++(docType_s%3ACOMM)&fq=collCode_s%3AINRIA2&rows=100000&wt=bibtex")
hal2 <- read_bibtex("https://api.archives-ouvertes.fr/search/?q=+publicationDateY_i%3A[2021+TO+2022]+AND++(docType_s%3ACOMM)&fq=collCode_s%3AINRIA2&rows=100000&wt=bibtex")
hal <- rbind(hal1 %>% select(bibtex_key, article_type, title, author, url, editor, series, volume, number, pages, year, month, keywords, pdf, hal_id, hal_version, publisher, doi, organization, booktitle, address ), 
              hal2 %>% select(bibtex_key, article_type, title, author, url, editor, series, volume, number, pages, year, month, keywords, pdf, hal_id, hal_version, publisher, doi, organization, booktitle, address )) %>% 
    distinct() %>% 
    filter(year >= 2018 & year <= 2022) # 10.052 articles

    # CORE (CSV)
core <- purrr::map(
        .x = (as.data.frame(rep(1:45, each = 1)) %>% rename(page = `rep(1:45, each = 1)`))$page,
        .y = data.frame(matrix(ncol = 1, nrow = 1)),
        .f = ~read_html(paste0("http://portal.core.edu.au/conf-ranks/?search=&by=all&source=all&sort=atitle&page=", .x)) %>% html_nodes('body')  %>% html_nodes('table') %>% html_table(dec = ","), 
        .default = NA)
core <- bind_rows(core) %>% rename(title = Title) %>% 
    mutate_all(na_if,"") %>% 
    mutate(core_id = row_number()) # 2.212 conférences
    

# Nettoyage des données
    # HAL
hal_manip <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(hal))) %>%  # retrait de tous les caractères spéciaux
    mutate(hal_title = tolower(title), # minuscules
           hal_title = str_replace(hal_title, "  ", " "), # retrait des doubles espaces
           booktitle = tolower(booktitle), 
           booktitle = str_replace(booktitle, "  ", " "))
    # CORE
core_manip <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(core))) %>%  # retrait de tous les caractères spéciaux
    mutate(core_title = tolower(title), # minuscules
           core_title = str_replace(core_title, "  ", " "), # retrait des doubles espaces
           core_acronym = tolower(Acronym), # même chose pour l'acronyme qui servira de colonne de jointure dans un second temps
           core_acronym = str_replace(core_acronym, "  ", " ")) 


# Match inexacte avec distance
    # colonne de jointure : TITLE
enriched_title <- stringdist_left_join(hal_manip %>% select(hal_title, year, hal_id, booktitle) %>% filter(!is.na(hal_title)),
                                    core_manip %>% select(core_title, core_acronym, core_id, Source, Rank, `Primary FoR`),
                                    by = c("hal_title" = "core_title"),
                                    max_dist = 50, 
                                    distance_col = "distance") %>% 
    mutate(colHAL_jointure = "title") %>% 
    arrange(distance) %>% 
    group_by(hal_id) %>% 
    slice_min(order_by = distance, n = 1) # ne garder que le match le plus proche #rename(Acronym_title = Acronym, Source_title = Source, Rank_title = Rank, FoR_title = `Primary FoR`)
    # colonne de jointure : BOOKTITLE
enriched_booktitle <- stringdist_left_join(hal_manip %>% select(hal_title, year, hal_id, booktitle) %>% filter(!is.na(booktitle)),
                                    core_manip %>% select(core_title, core_acronym, core_id, Source, Rank, `Primary FoR`),
                                    by = c("booktitle" = "core_title"),
                                    max_dist = 50, 
                                    distance_col = "distance") %>% 
    mutate(colHAL_jointure = "booktitle") %>% 
    arrange(distance) %>% 
    group_by(hal_id) %>% 
    slice_min(order_by = distance, n = 1) # ne garder que le match le plus proche
    # on remet les 2 jointures ensemble
enriched_hal_score <- rbind(enriched_title, enriched_booktitle) %>% 
    group_by(hal_id) %>% slice_min(order_by = distance, n = 1) %>% # ne garder que le match le plus proche entre title et booktitle
    mutate(method = "score")





# Match sur les tokens communs
    # colonne de jointure : TITLE
enriched_title_token <- hal_manip %>% 
    mutate(split = strsplit(as.character(hal_title), " ")) %>% unnest(split) %>% 
    filter(!is.na(split)) %>% 
    left_join(., 
              core_manip %>% select(core_title, core_acronym, Acronym, core_id, Source, Rank, `Primary FoR`), 
              by = c("split" = "core_acronym")) %>% 
    filter(!is.na(Acronym)) %>% 
    mutate(colHAL_jointure = "title")
    # colonne de jointure : BOOKTITLE
enriched_booktitle_token <- hal_manip %>% 
    mutate(split = strsplit(as.character(booktitle), " ")) %>% unnest(split) %>% 
    filter(!is.na(split)) %>% 
    left_join(., 
              core_manip %>% select(core_title, core_acronym, Acronym, core_id, Source, Rank, `Primary FoR`), 
              by = c("split" = "core_acronym")) %>% 
    filter(!is.na(Acronym)) %>% 
    mutate(colHAL_jointure = "booktitle")
    # on remet les 2 jointures ensemble
enriched_hal_token <- rbind(enriched_title_token, enriched_booktitle_token) %>% distinct() %>% 
    mutate(method = "token") %>% 
    select("hal_title","year","hal_id","booktitle", "split","core_title","Acronym","Source","Rank","Primary FoR","split","colHAL_jointure","method","core_id")

# On remet les noms initiaux (avec caractères spéciaux et majuscules)
enriched_hal_token <- enriched_hal_token %>% 
    # Noms des articles et conférences dans HAL
    select(-c(hal_title, booktitle)) %>% 
    left_join(., hal %>% select(title, booktitle, hal_id) %>% mutate(hal_id = str_replace_all(hal_id, "-", "")), by = "hal_id") %>% 
    rename(hal_title = title) %>% 
    # Noms des conférences et acronymes dans CORE
    select(-c(core_title)) %>% mutate(core_id = as.numeric(core_id)) %>% 
    left_join(., core %>% select(title, core_id), by = "core_id") %>% 
    rename(core_title = title) %>% 
    # Mise en forme finale
    select("hal_title","year","hal_id","booktitle","core_title","Acronym","Source","Rank","Primary FoR","colHAL_jointure","method")


# séparer les mots des titres
# acronym CORE minimisés
# jointure
# ne garder que les HAL quand jointure
# répeter à partir de booktitle et howpublished










#---- BROUILLON
init_proc = FuzzUtils$new()
PROC = init_proc$Full_process
PROC1 = tolower 
init_scor = FuzzMatcher$new()
SCOR = init_scor$WRATIO
init <- FuzzExtract$new()

word = "new york jets"
choices = c("Atlanta Falcons", "New York Jets", "New York Giants", "Dallas Cowboys")

init$Extract(string = word, sequence_strings = choices, processor = PROC, scorer = SCOR)
init$ExtractBests(string = core_manip$core_title, sequence_strings = hal_manip$hal_title)

