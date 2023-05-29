source(here("R", "librerie.R"))


# # Titanic data (senza crew)----
# 
unzip(here("dati","titanic.zip"), list = TRUE)
# 
# dt1 <- read_csv(unzip(here("dati","titanic.zip"), "test.csv"))
# dt2 <- read_csv(unzip(here("dati","titanic.zip"), "train.csv"))

#survtest <- read_csv(unzip(here("dati","titanic.zip"), "gender_submission.csv"))

# 
# dt <- dt1 %>% 
#   left_join(survtest) %>% 
#   bind_rows(dt2)
# 
# write.xlsx(dt, here("dati", "titanic.xlsx"))

# Import----

#dt <- read_excel(here("dati", "titanic.xlsx"))
titanic <- read_excel(here("dati", "titanicmod.xlsx"))

## guardiamo il dataset

head(titanic)
str(titanic)
glimpse(titanic)
View(titanic)

# rinominare le colonne con la funzione clean_names()

titanic <- clean_names(titanic)
# titanic <- rename(titanic, sblsp = siblings_and_spouses_on_board, 
#                            parch = parent_children_on_board)

## verbs----

select(titanic, name, age, fare)
select(titanic,  -c(6:7))
select(titanic, - age)
select(titanic, where(is.character))
select(titanic, where(is.numeric))
slice(titanic, 1:3)
top_n(titanic, 10)


rename(titanic, sblsp = siblings_and_spouses_on_board, 
       parch = parent_children_on_board)

select(titanic, c(1:5),tariffa = fare)

arrange(titanic, desc(fare))

filter(titanic, sex == "male")
filter(titanic, sex != "male")
filter(titanic, sex != "male", sex != "Male", sex != "male_")
filter(titanic, !sex %in% c("male", "Male", "male_"))
View(filter(titanic, sex == "female", 
                pclass == 3, 
                !is.na(cabin)))

mutate(titanic, sex = if_else(sex %in% c("Male", "male_"), "male", sex))
mutate(titanic, sex2 = if_else(sex %in% c("Male", "male_"), "male", sex))


glimpse(mutate(titanic, Sex = case_when(sex == "male" ~ "M",
                         sex == "female" ~ "F",
                         sex == "Male" ~ "M", 
                         sex == "male_" ~ "M"),
         Sex = as.factor(Sex), 
       age = replace(age, age %in% c("ND", "//"), NA), 
       age = as.numeric(age)))

mutate(titanic, across(c("survived", "sex", "embarked"), as.factor)) %>% glimpse()


summarise(titanic, mean(fare))
summarise(titanic, media = mean(fare, na.rm = T), 
          min = min(fare, na.rm = T), 
          mediana = median(fare, na.rm = T))  

dtgrouped <- group_by(titanic, pclass)

summarise(dtgrouped, media = mean(fare, na.rm = T), 
          n = n()) 



#qual'è il tasso di sopravvivenza complessivo?

titanic_survived <- filter(titanic, survived == 1)
sum(titanic_survived$survived)/nrow(titanic)
mean(titanic$survived)




## pipe 

#%>% # questo operatore permette di eseguire in sequenza le istruzioni in R utilizzando l'output di una
#istruzione come input ( argomento) della successiva istruzione

titanic <- read_excel(here("dati", "titanicmod.xlsx"))
titanic %>%  
  clean_names() %>%  
  rename( sblsp = siblings_and_spouses_on_board, 
           parch = parent_children_on_board ) %>%
  mutate(age = replace(age, age %in% c("ND", "//"), NA),
         age = as.numeric(age), 
         sex = if_else(sex %in% c("Male", "male_"), "male", sex), 
         across(c("sex", "embarked", "pclass"), as.factor)) %>% 
  group_by(pclass, sex) %>% 
  summarise(Surv = mean(survived), 
            n = n())  %>% 
  mutate(Surv = paste(round(Surv,2),"(",n, ")")) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "sex", values_from ="Surv")
  

titanic %>% clean_names() %>% 
  select(cabin, pclass) %>% #, survived) %>%
  group_by(cabin, pclass) %>% #, survived) %>%
  count() %>%  View()
  pivot_wider(names_from = "survived", values_from = "n", values_fill = 0) %>% 
  rename(death = `0`, surv = `1`)

#qual'è il tasso di sopravvivenza dei passeggeri con cabina?
  
......

titanic %>% 
  filter(Cabin == "G6")  



# titanic %>%
#   clean_names() %>% 
#   mutate(across(everything(), as.character)) %>%
#   pivot_longer(-name, names_to = "Parameter_name", values_to = "Parameter_value") -> x
#   
# x %>% pivot_wider(names_from = "Parameter_name", values_from = "Parameter_value")
# 
# x %>%
#   dplyr::group_by(name, Parameter_name) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) %>%  View()
  
  
  
  

# table2 table3 per pivotting, separate, unite.....


titanic %>%
  clean_names() %>% 
  separate(col = name, into = c("surname", "title", "name"), sep = "[,\\.]") %>% 
  select(passenger_id, surname:name)




#dati cellulle Berni
ER1175 <- read_excel("C:/Users/vito.tranquillo/Desktop/dataset esempi per corso/Copia di Esempio Dati_MBerni_AREG.xlsx", 
           col_types = c("date", "numeric", "text", 
                         "numeric", "skip", "skip", "skip", 
                         "skip", "skip", "skip", "skip"))

SL1344 <- read_excel("C:/Users/vito.tranquillo/Desktop/dataset esempi per corso/Copia di Esempio Dati_MBerni_AREG.xlsx", 
                     col_types = c("skip", "skip", "skip", 
                                   "skip", "skip", "skip", "skip", "date", 
                                   "numeric", "text", "numeric"))
dt <- ER1175 %>% 
  mutate(ceppo = "ER1175") %>% 
  rename(fluorescenza = ER1175) %>% 
  
  bind_rows(
    SL1344 %>% 
      mutate(ceppo = "SL1344") %>% 
      rename(fluorescenza = SL1344)
  ) %>% 
  clean_names() %>%
# mutate(duplicati = ifelse(duplicated(id_singola_cell) == "TRUE", "si", "no")) %>% 
  na.omit() %>% 



  group_by(ceppo, rep_tec) %>%
  summarise(n = n(),
            media = mean(fluorescenza),
            sd = sd(fluorescenza))
  
  # ggplot()+
  # aes(x = fluorescenza, fill = ceppo)+
  # geom_density(alpha = 0.6)+
  # scale_fill_manual(values=c("#868E74", "#AD5988"))+
  #  facet_grid(rep_tec~ rep_bio, scales = "free")
  # 

# dati who-----

data(who)


who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")

who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "type", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#Per ogni paese, anno e sesso calcolare il numero totale di casi di TB.


#zecche

dati <- read_excel(here("data", "zecche.xlsx"))



# dt <- dati %>%
#   filter(is.na(esito)) %>%
#   distinct() %>% 
#   
#   group_by(specie, stadio) %>% 
#   mutate(stadio = casefold(stadio), 
#          stadio = ifelse(stadio %in% c("femmina", "maschio"), "adulto", stadio )) %>% 
#   
#   count() 


dtpat <- dati %>%
  filter(!is.na(esito)) %>%  
  # distinct() %>%
  mutate(stadio = casefold(stadio),
         stadio = ifelse(stadio %in% c("femmina", "maschio", "Maschio", "Femmina"), "Adult", 
                         ifelse(stadio == "larva", "Larvae", 
                                ifelse(stadio == "ninfa", "Nymphae", stadio))), 
         stagione = recode(mese, 
                           "dicembre" = "Winter",
                           "gennaio" = "Winter", 
                           "febbraio" = "Winter", 
                           "marzo" = "Spring",
                           "aprile" = "Spring",
                           "maggio" = "Spring", 
                           "giugno" = "Summer", 
                           "luglio" = "Summer", 
                           "agosto" = "Summer", 
                           "settembre" = "Autumn",
                           "ottobre" = "Autumn", 
                           "novembre" = "Autumn"), 
         stagioneanno = paste0(annoreg,stagione)) %>%
  filter(stadio != "n.d.")


pat <- dtpat %>% 
  mutate(pat = ifelse(esito != "Non dimostrata presenza", "1", "0")) %>%  
  pivot_wider(names_from = "prova", values_from = "pat", values_fill = '0' ) %>% 
  select(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, annoreg, stagioneanno,  altitudine, 17:22 ) %>%   
  mutate(across(c(10:13), as.numeric)) %>% 
  group_by(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, stagioneanno,altitudine) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>% 
  rowwise() %>%
  mutate(Pat = sum(c_across(c(10:13)), na.rm = T),
         Pat2 = ifelse(Pat == 0, 0, 1), 
         PatCat = ifelse(Pat2 == 0 , "Neg", "Pos"),
         stadio = factor(stadio, levels =c("Larvae", "Nymphae",  "Adult"))
  )%>%
  filter(specie == "Ixodes ricinus")

