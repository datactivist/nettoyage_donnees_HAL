## BROUILLON POUR COMMANDES IMPORT XML-TEI


# Librairies
library(tidyverse)
library(rvest)
library(XML)
library(httr)
library(xml2)
#https://api.archives-ouvertes.fr/search/?q=+(docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS)+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=bibTex&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s

# Données CORE (CSV)
core_data <- purrr::map(
        .x = (as.data.frame(rep(1:5, each = 1)) %>% rename(page = `rep(1:5, each = 1)`))$page,
        .y = data.frame(matrix(ncol = 1, nrow = 1)),
        .f = ~read_html(paste0("http://portal.core.edu.au/conf-ranks/?search=&by=all&source=all&sort=atitle&page=", .x)) %>% html_nodes('body')  %>% html_nodes('table') %>% html_table(dec = ","), 
        .default = NA)
core_data <- bind_rows(core_data)



 # Données TEI-XML
    # import
data <- xmlParse(content(GET("https://api.archives-ouvertes.fr/search/?q=+%28docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS%29+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=xml-tei&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s", user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36")), "text"), 
                      encoding = "UTF-8")

# Parse, test
## 1
xml_df <- xmlToDataFrame(nodes = getNodeSet(data, "//body"))
xml_df <- xmlToDataFrame(nodes = getNodeSet(data, "TEI/text/body"))
xml_df <- xmlToDataFrame(nodes = getNodeSet(data, "//biblFull"))

test <- xpathApply(data, "TEI")
test <- xpathApply(data, "//biblFull")
test <- unlist(xpathApply(data, '//biblFull', xmlGetAttr, "when"))

# Exemple
doc = xmlParse(system.file("exampleData", "tagnames.xml", package = "XML"))
els = getNodeSet(doc, "/doc//a[@status]")
sapply(els, function(el) xmlGetAttr(el, "status"))

## 2
test <- read_xml("https://api.archives-ouvertes.fr/search/?q=+%28docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS%29+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=xml-tei&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s")
test2 <- xml_child(xml_child(xml_child(test, 2), 1), 1)
test3 <- xml_child(test2, 1)


## 3
library(tei2r) #remotes::install_github("michaelgavin/tei2r")
res <- parseTEI(data, node = "biblFull")

## 4
devtools::install_github("computationalstylistics/stylo")
library(stylo)
parse.corpus(data, markup.type = "xml", corpus.lang = "English")



parse.corpus(novels)









#--------------
#https://api.archives-ouvertes.fr/search/?q=+(docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS)+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=bibTex&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s

# ne fonctionne pas
remotes::install_github("GeoBosh/rbibutils")
library(rbibutils)
data <- readBib("https://api.archives-ouvertes.fr/search/?q=+(docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS)+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=bibtex&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s")
data <- readBib("~/Desktop/test.bib")

# fonctionne
install.packages("bib2df")
library(bib2df)
options(encoding="UTF-8")
bib_df <- bib2df("https://api.archives-ouvertes.fr/search/?q=+(docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS)+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=bibtex&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s")


# fonctionne
remotes::install_github("skgrange/threadr")
library(threadr)
data <- read_bibtex("~/Desktop/test.bib")
data <- read_bibtex("https://api.archives-ouvertes.fr/search/?q=+(docType_s%3ACOMM+OR++docType_s%3APROCEEDINGS)+AND++publicationDateY_i%3A2021&fq=collCode_s%3AINRIA2&rows=9000&wt=bibtex&fl=halId_s,conferenceTitle_s,country_s,city_s,conferenceStartDate_s,conferenceStartDate_s")






