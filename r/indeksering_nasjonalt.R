rm(list = ls())

# library(readr)         #
library(tidyverse)
library(googlesheets4)
library(stringr)
library(haven)

# Leser variabelarket fra gsheets. Trekker ut "Nasjonal"
kodebok <- read_sheet("https://docs.google.com/spreadsheets/d/1GYFapeq8ND565JtsMbbmI9Qwjn4QFH6g0vDo-QQTcDY/",
                      range = "Nasjonalt")

# Fikser kolonner som blir tolket som lister
kodebok$datasett <- lapply(kodebok$datasett, function(x) {
  ifelse(is.null(x), NA, x)
}) %>% unlist()

kodebok$variabelnavn <- lapply(kodebok$variabelnavn, function(x) {
  ifelse(is.null(x), NA, x)
}) %>% unlist()

kodebok$datasettnr <- lapply(kodebok$datasettnr, function(x) {
  ifelse(is.null(x), NA, x)
}) %>% unlist()

kodebok$in_out <- lapply(kodebok$`0/1/?/!`, function(x) {
  ifelse(is.null(x), NA, x)
}) %>% unlist()


# Fyller inn tomme celler i arket
kodebok2 <- kodebok %>%                      # 
  fill(dimensjon, komponent, gruppe,         # Fyller dim, komp, grp, og
       komp_til_dim, grp_til_komp,           # agg regler nedover
       .direction = "down") %>%              #
  group_by(dimensjon, komponent, gruppe) %>% # Grupper på dim, komp, grp
  fill(indikator, ind_til_grp,               # Fyller ind, og ind_til_grp 
       .direction = "down") %>%              # nedover
  group_by(dimensjon, komponent, 
           gruppe, indikator) %>%            # Grupper på dim/komp/grp/ind
  fill(underindikator, uind_til_ind,         # Fyller unid og uind_til_ind
       .direction = "down") %>%              # nedover
  group_by(dimensjon, komponent, gruppe,     # Grupper på alt
           indikator, underindikator,        # og ekspanderer hele datasettet  
           underunderindikator,              # på årene 2017 og 2021
           datasettnr, variabelnavn,         # slik at hver variabel
           komp_til_dim, grp_til_komp,       # får 1 verdi pr år
           ind_til_grp, uind_til_ind,        #
           uuind_til_uind) %>%               #
  filter(in_out == 1) %>%                    # Tar med bare de som er "in"/1
  expand(year = c(2017, 2021))

# Lister opp alle strukturerte datasett (nasjonalt nivå)
struct_data <- list.files("./struct_data", full.names = TRUE, pattern = "[0-9]+\\.rda")

# Laster inn alle datasettene
for(i in struct_data) {
  # message(i)
  load(i)
}

# Holder for valuekolonnen
kodebok2$value <- NA

# For hver rad i dataseettet ...
for(i in 1:nrow(kodebok2)) {
  
  cat(paste0(" Mapping TiNDe values: ", sprintf("%.2f", i/nrow(kodebok2)*100), "%\r"))
  
  # Trekk ut datasettnr.
  datanr <- kodebok2$datasettnr[i]
  
  # Hvis det er NA, gi value NA
  if(datanr == "-999" | is.na(datanr)) {
    kodebok2$value[i] <- NA
    next
  }
  
  # Finn riktig objekt i environment som passer til datasettnr (222 = "ssb_222")
  datasett <- ls()[which(grepl(datanr, ls()))]
  
  # Hvis datasettet ikke finnes, får value NA
  if(identical(datasett, character())) {
    kodebok2$value[i] <- NA
    next
  }
  
  # Trekker ut variabelnavn for i
  var <- kodebok2$variabelnavn[i]
  
  if(is.na(var)) {
    kodebok2$value[i] <- NA
    next
  }
  
  
  if(var == "01185") {
    kodebok2$value[i] <- get(datasett) %>% 
      filter(variable == "01185") %>% 
      pull(value) %>% 
      mean()
    next
  }
  
  yr <- kodebok2$year[i]
  
  value <- get(datasett) %>%
    filter(variable == var & (year == yr | year == yr + 1 | year == yr - 1)) %>% 
    filter(is.na(value) == FALSE)
  
  if(nrow(value) == 0) {
    kodebok2$value[i] <- NA
    next
  }
  
  if(length(intersect(value$year, yr)) > 0) {
    kodebok2$value[i] <- unique(value$value[which(value$year == yr)])
    next
  }
  
  if(length(unique(value$value)) == 1) {
    kodebok2$value[i] <- unique(value$value)
    next
  }
  
  if(length(unique(value$value)) > 1) {
    kodebok2$value[i] <- unique(value$value[which(value$year == yr - 1)])
    next
  }
  stop("end of the line: ", i)
}

corrected_100_names <- c(
  "valgn_11", "valgl_07", "valgl_13", "valgr_03",
  "valgr_04", "libn_05", "libn_08", "libn_09", "libn_18", "delibn_07", 
  "delibl_05", "delibr_04", "delibr_07", "delibr_08", "delibr_09", "egaln_03", "egaln_04",
  "egall_06", "egall_10", "egalr_03", "egalr_07"
)


kodebok2 <- kodebok2 %>% 
  mutate(value = ifelse(variabelnavn %in% corrected_100_names,
                        (value * -1) + 1, value))


kodebok3 <- kodebok2 %>% 
  group_by(dimensjon, komponent, gruppe, indikator, underindikator, year) %>% 
  mutate(uind_agg = case_match(uuind_til_uind,
                               c("Snitt", "Link, så snitt") ~ mean(value, na.rm = TRUE),
                               c("Multiplikativ")           ~ prod(value),
                               "Snitt EF/EU avstemningene"  ~ mean(value),
                               .default = value)) %>% 
  ungroup()

kodebok3 <- kodebok3 %>%
  group_by(dimensjon, komponent, gruppe, indikator, year) %>% 
  mutate(ind_agg = case_match(uind_til_ind,
                              c("kun denne", "Kun denne")          ~ mean(unique(uind_agg), na.rm = TRUE),
                              c("Snitt", "Link, så snitt", "Link") ~ mean(uind_agg[which(duplicated(underindikator) == FALSE)], na.rm = TRUE),
                              c("Multiplikativ")                   ~ prod(value, na.rm = TRUE)),
         ind_agg = ifelse(is.na(ind_agg), mean(unique(uind_agg), na.rm = TRUE), ind_agg),
         ind_agg = ifelse(is.nan(ind_agg), NA, ind_agg)) %>% 
  ungroup()

kodebok3 <- kodebok3 %>%
  group_by(dimensjon, komponent, gruppe, year) %>%
  mutate(grp_agg = NA,
         grp_agg = ifelse(str_detect(ind_til_grp, "Snitt|Link, så snitt"), 
                          mean(ind_agg[which(duplicated(indikator) == FALSE)], na.rm = TRUE), grp_agg),
         grp_agg = ifelse(str_detect(ind_til_grp, "Snitt|Link, så snitt") & all(is.na(ind_agg) == TRUE), 
                          mean(ind_agg[which(duplicated(indikator) == FALSE)], na.rm = TRUE), grp_agg),
         grp_agg = ifelse(ind_til_grp == "Kun denne", mean(unique(ind_agg), na.rm = TRUE), grp_agg),
         grp_agg = ifelse(ind_til_grp == "Multiplikativ", prod(unique(ind_agg), na.rm = TRUE), grp_agg),
         grp_agg = ifelse(str_detect(gruppe, "Tillit, troverdighet og legitimitet"),
                          mean(ind_agg[which(str_detect(indikator, "politiske system"))] * (1/2), na.rm = TRUE) +
                            mean(ind_agg[which(str_detect(indikator, "overnasjonal"))] * (1/6), na.rm = TRUE) +
                            mean(ind_agg[which(str_detect(indikator, "troverdighet"))] * (1/3), na.rm = TRUE), 
                          grp_agg),
         grp_agg = ifelse(str_detect(ind_til_grp, "4/5 Direct"),
                          (ind_agg[which(str_detect(indikator, "Direct popular"))] * (4/5)) +
                            (ind_agg[which(str_detect(indikator, "Deltakelse i direkte"))] * (1/5)),
                          grp_agg),
         grp_agg = ifelse(str_detect(ind_til_grp, "2/3 Lik innflytelse"),
                          (mean(ind_agg[which(str_detect(indikator, "Lik innflytelse"))] * (2/3))) +
                            (mean(ind_agg[which(str_detect(indikator, "Lobbyvirksomhet"))] * (1/3))),
                          grp_agg),
         grp_agg = ifelse(str_detect(ind_til_grp, "1/4 vekting"),
                          (mean(ind_agg[which(str_detect(indikator, "Vekting stemmerett"))], na.rm = TRUE) * (1/4)) +
                            (mean(ind_agg[which(str_detect(indikator, "Likebehandling/lik beskyttelse"))], na.rm = TRUE) * (3/4)),
                          grp_agg),
         grp_agg = ifelse(str_detect(ind_til_grp, "1/5 Election vote registry"),
                          (mean(ind_agg[which(str_detect(indikator, "Andel av befolkning med stemmerett"))], na.rm = TRUE) * (1/5)) +
                            (mean(ind_agg[which(str_detect(indikator, "Election voter registry"))], na.rm = TRUE) * (4/5)),
                          grp_agg),
         grp_agg = ifelse(str_detect(ind_til_grp, "1/4 Territoriell"),
                          (mean(ind_agg[which(str_detect(indikator, "Territoriell"))], na.rm = TRUE) * (1/4)) +
                            (mean(ind_agg[which(str_detect(indikator, "Overordnet valgte embeter"))], na.rm = TRUE) * (3/4)),
                          grp_agg),
         grp_agg = ifelse(variabelnavn == "v2x_elecoff", ind_agg, grp_agg))


kodebok4 <- kodebok3 %>%
  group_by(dimensjon, komponent, year) %>%
  mutate(komp_agg = NA,
         komp_agg = ifelse(str_detect(grp_til_komp, "Snitt|Link, så snitt"), 
                           mean(grp_agg[which(duplicated(gruppe) == FALSE)], na.rm = TRUE), komp_agg),
         komp_agg = ifelse(grp_til_komp == "Kun denne", mean(grp_agg, na.rm = TRUE), komp_agg),
         komp_agg = ifelse(str_detect(grp_til_komp, "1/4 Deliberasjon"),
                           (mean(grp_agg[which(str_detect(gruppe, "Deliberasjon i parlamentet"))], na.rm = TRUE) * (1/4)) +
                             (mean(grp_agg[which(str_detect(gruppe, "Deliberasjon i partier"))], na.rm = TRUE) * (3/4)),
                           komp_agg),
         komp_agg = ifelse(str_detect(grp_til_komp, "1/4 Konstitusjonelle"),
                           (mean(grp_agg[which(str_detect(gruppe, "Konstitusjonelle føringer"))], na.rm = TRUE) * (1/4)) +
                             (mean(grp_agg[which(str_detect(gruppe, "Direkte demokrati i praksis"))], na.rm = TRUE) * (3/4)),
                           komp_agg),
         komp_agg = ifelse(str_detect(grp_til_komp, "2/3 Individuell deltakelse"),
                           (mean(grp_agg[which(str_detect(gruppe, "Individuell deltakelse"))], na.rm = TRUE) * (2/3)) +
                             (mean(grp_agg[which(str_detect(gruppe, "Innflytelse gjennom deltakelse"))], na.rm = TRUE) * (1/3)),
                           komp_agg),
         komp_agg = ifelse(str_detect(grp_til_komp, "3/4 Lik beskyttelse"),
                           (mean(grp_agg[which(str_detect(gruppe, "Lik beskyttelse av rettigheter"))], na.rm = TRUE) * (3/4)) +
                             (mean(grp_agg[which(str_detect(gruppe, "Trusler og trakassering"))], na.rm = TRUE) * (1/4)),
                           komp_agg),
         komp_agg = ifelse(str_detect(grp_til_komp, "1/2 Fri og rettferdig"),
                           (mean(grp_agg[which(str_detect(gruppe, "Fri og rettferdig valgprosess"))], na.rm = TRUE) * (1/2)) +
                             (mean(grp_agg[which(str_detect(gruppe, "Reell politisk konkurranse"))], na.rm = TRUE) * (1/3)) +
                             (mean(grp_agg[which(str_detect(gruppe, "Trusler og trakassering"))], na.rm = TRUE) * (1/6)),
                           komp_agg))





kodebok5 <- kodebok4 %>% 
  group_by(dimensjon, year) %>% 
  mutate(dim_agg = ifelse(str_detect(komp_til_dim, "^0,2"), 
                          (0.2 * 
                             (mean(komp_agg[which(str_detect(komponent, "Formelle deltakelsesrettigheter"))]) *
                                mean(komp_agg[which(str_detect(komponent, "Praktisk hindre deltakelse"))]) *
                                mean(komp_agg[which(str_detect(komponent, "Valgte embeter"))]) *
                                mean(komp_agg[which(str_detect(komponent, "Frie og rettferdige valg"))]) *
                                mean(komp_agg[which(str_detect(komponent, "Ansvarliggjøring"))]) *
                                mean(komp_agg[which(str_detect(komponent, "Organisasjonsfrihet"))]) *
                                mean(komp_agg[which(str_detect(komponent, "Informasjonstilgang"))]))) +
                            (0.8 * ((mean(komp_agg[which(str_detect(komponent, "Formelle deltakelsesrettigheter"))]) * (4/32)) +
                                      (mean(komp_agg[which(str_detect(komponent, "Praktisk hindre deltakelse"))]) * (4/32)) +
                                      (mean(komp_agg[which(str_detect(komponent, "Valgte embeter"))]) * (4/32)) +
                                      (mean(komp_agg[which(str_detect(komponent, "Frie og rettferdige valg"))]) * (8/32)) +
                                      (mean(komp_agg[which(str_detect(komponent, "Ansvarliggjøring"))]) * (2/32)) +
                                      (mean(komp_agg[which(str_detect(komponent, "Organisasjonsfrihet"))]) * (5/32)) +
                                      (mean(komp_agg[which(str_detect(komponent, "Informasjonstilgang"))]) * (5/32)))),
                          mean(komp_agg[which(duplicated(komponent) == FALSE)], na.rm = TRUE)))


googlesheets4::sheet_write(kodebok5, ss = "https://docs.google.com/spreadsheets/d/1GYFapeq8ND565JtsMbbmI9Qwjn4QFH6g0vDo-QQTcDY/",
                           sheet = "Test indeksering")

write.csv(kodebok5, file = "./quarto/indekser.csv")