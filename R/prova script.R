source(here('R', 'librerie.R'))


dati <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "numeric", "text", "text", "date", 
                                 "text", "text", "date", "text", "date", 
                                 "text", "text", "text", "skip"))


 


dati <- clean_names(dati)
view(dati)


#unique(dati$Specie)
#conferimenti <- unique(dati$`Conf. orig`)
#table(conferimenti)

# Riempire celle vuote con valore precedente ------------------------------


# dati_2022 <- dati %>% 
#   mutate(conf_orig = na.locf(conf_orig),
#          provenienza = na.locf(provenienza),
#          sacco = na.locf(sacco)) %>% 
#   group_by(specie,materiale) %>% 
#   select(-campioni_conf)

dati_2022 <- dati %>% slice(1:3871) %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf)) %>% 
  select(-campioni_conf)   #più ordinato di versione precedente

 n_distinct(dati_2022$conf_orig)

# Tabella frequenza per provincia -----------------------------------------


prov <- table(dati$provenienza)

round(prov/n_distinct(dati_2022$conf_orig)*100, 0)

prov2 <- as.data.frame(prov)
prov2 %>% add_column(percent = round(prov2$Freq/n_distinct(dati_2022$conf_orig)*100,0)) %>% view()


dati_2022 %>% 
  distinct(conf_orig, .keep_all = TRUE) %>%  
  group_by(provenienza) %>% 
  tally() %>% 
  mutate(prop=100*(n/sum(n)))



# Prove analisi --------------------------------------------
str(dati_2022)

dati_2022 <- dati_2022 %>%
  filter(!is.na(materiale)) %>% 
  filter(!is.na(progr)) %>% 
  filter(!is.na(specie)) %>% 
  filter(specie != "CINGHIALE")

unique(dati_2022$materiale)

dati_2022 <- dati_2022 %>% 
  mutate(materiale = replace(materiale, materiale %in% "T. NAS", "T.NAS"),
         materiale = replace(materiale, materiale %in% "T. RETT", "T.RETT"),
         materiale = replace(materiale, materiale %in% c("ILEO/DIGIUNO", "DIGIUNO","DUODENO"), "INTESTINO"),
         materiale = replace(materiale, materiale %in% "SENI NASALI", "T.NAS"))

unique(dati_2022$specie)


#voglio arrivare a fare un summary di quanti animali positivi a pancov per specie

dati_2022 %>% mutate(pancov = replace_na(pancov, "NEG"),
                     esito = replace_na(esito, "Negativo")) %>% 
              filter(pancov == "POS") %>%
              group_by(conf_orig, specie) %>% 
              summarise(materiale) %>% View()
              pivot_wider(names_from = "specie", values_from = "materiale") %>% view()
#mmm da aggiustare... ciclo for?


dati_2022 %>% mutate(pancov = ifelse(is.na(pancov), 0,
                                     ifelse(pancov == "NEG", 0,1))) %>%
  select(conf_orig,  specie, materiale, pancov) %>% 
  pivot_wider(names_from = "materiale", values_from = "pancov") %>% View()



# bozze -------------------------------------------------------------------



#dati$`Conf. orig` %>% na.locf() #riempie celle vuote con ultimo valore sopra

#glimpse(dati)
#view(mutate(dati,`Conf. orig`= na.locf(`Conf. orig`)))
