# Load packages
library(tidyverse)
library(stringr)
library(rentrez)
library(scholar) # scholar ID: 'vlXRD0UAAAAJ'
library(rcrossref)
library(knitr)
library(pander)

# Initial prep
##############
## Set working directory
setwd('/Users/PeterKamerman/Dropbox/_home/cv/painblogr_cv/')
## Read publication list csv
publist <- read_csv('painblogr_cv.csv',
                    trim_ws = TRUE)
## Make new dataframe for articles with and without DOIs
#doi_yes <- filter(publist, !is.na(DOI))
#doi_no <- filter(publist, is.na(DOI))

# Crossref (xref) citations
###########################
## Create vector of DOIs
#dois <- doi_yes$DOI
## Get Crossref citation count for each DOI
#xref_cite <- sapply(dois, cr_citation_count)
## Add Crossref citation count to doi_yes dataframe
#xref_cite <- data.frame(DOI = names(xref_cite),
#                        Citations_xref = xref_cite) %>%
#    mutate(DOI = as.character(DOI)) %>%
#    full_join(doi_yes, by = "DOI") %>%
#    select(Number, Publications, DOI, PMID, Citations_xref)

# Google scholar (GS) citations
###############################
## Get citations from GS, and get page numbers and year to match to
## those in the publications data in doi_yes/xref_cite and doi_no
#gs_grep <- get_publications(id = 'vlXRD0UAAAAJ', flush = TRUE)[ , 4:6] %>%
#    filter(number != "") %>%
#    separate(number, into = c("volume", "issue", "pages"), sep = " ") %>%
#    unite(.grep, pages, year, sep = ", ") %>%
#    select(.grep, cites)
## Add GS citations to the articles with DOIs
## Make a function
#.temp <- list()
#gs_citations <- function(data){
#    for(i in seq_along(gs_grep$.grep)){
#            .temp[[i]] <- filter(data, str_detect(Publications, gs_grep$.grep[i]))
#            .temp[[i]] <- mutate(.temp[[i]], Citations_GS = gs_grep$cites[i])
#    }
#    .temp
#}
#gs_cite_doi <- gs_citations(data = xref_cite)
#gs_cite_doi <- do.call(rbind, gs_cite_doi)
#gs_cite_doi <- arrange(gs_cite_doi, Number)
## Get the leftovers from xref_cite without GS citation data
#xref_leftovers <- xref_cite %>%
#    mutate(Citations_GS = as.numeric(NA)) %>%
#    anti_join(gs_cite_doi, by = "Number")
## Add GS citations for the articles without DOIs (first add empty
## Citations_xref column to doi_no)
#doi_no <- doi_no %>%
#    mutate(Citations_xref = as.numeric(NA))
#gs_cite_nodoi <- gs_citations(data = doi_no)
#gs_cite_nodoi <- do.call(rbind, gs_cite_nodoi)
#gs_cite_nodoi <- arrange(gs_cite_nodoi, Number)
## Get the leftovers from doi_no without GS citation data
#nodoi_leftovers <- doi_no %>%
#    anti_join(gs_cite_nodoi, by = "Number")
## Merge gs_cite_doi, gs_cite_nodoi, xref_leftovers, nodoi_leftovers
#final <- gs_cite_doi %>%
#    bind_rows(xref_leftovers, gs_cite_nodoi, nodoi_leftovers) %>%
#    arrange(Number) %>%
    ## change NA in Citations_xref/GS to 0
#    mutate(Citations_xref = ifelse(is.na(Citations_xref), 0, Citations_xref)) %>%
#    mutate(Citations_GS = ifelse(is.na(Citations_GS), 0, Citations_GS))

# Add markdown links to DOIs and PMIDs
######################################
## DOI
publist$DOI <- as.character(publist$DOI)
doi_link <- function(data){
    for(i in seq_along(data$Publications))
        if(!is.na(data[i, 3])){
            data[i, 2] <- gsub(data[i, 3],
                            paste0("<a href=\"http://dx.doi.org/", data[i, 3], "\" target=\"_blank\">", data[i, 3], "</a>"),
                            data[i, 2],
                            fixed = TRUE)
        }
    data
}
publist_link <- doi_link(publist)

## PMID
publist_link$PMID <- as.character(publist_link$PMID)
pmid_link <- function(data){
    for(i in seq_along(data$Publications))
        if(!is.na(data[i, 4])){
            data[i, 2] <- gsub(data[i, 4],
                               paste0("<a href=\"http://www.ncbi.nlm.nih.gov/pubmed/", data[i, 4], "\" target=\"_blank\">", data[i, 4], "</a>"),
                               data[i, 2],
                               fixed = TRUE)
        }
    data
}
publist_link <- pmid_link(publist_link)
final <- publist_link %>%
    select(Number, Publications, Link) %>%
    rename(PDF = Link, Publication = Publications)
pander(final, missing = '', justify = 'left', split.cells = Inf, split.tables = Inf, caption = '**Table:** Peer-reviewed journal articles _(reverse chronological order)_')
