#source(here("R", "librerie.R"))


#Incontro del 16 Giugno----
#link utili: https://r-charts.com/ggplot2/; https://r-graph-gallery.com/

# esercizi su dati COVID----


covid <- read.csv("C:/Users/vito.tranquillo/Desktop/Git Projects/CorsoR/dati/covid.csv")

#carta/bertasio/parisi

#1-
esamiperanno <- select(covid, Tot_Eseguiti, anno) 
esamiperanno <- na.omit(esamiperanno) 
esamiperanno %>%  
  pivot_wider(names_from = anno, values_from = Tot_Eseguiti, values_fn = sum) 


#4--numero di esami eseguiti con la prova SARS-CoV-2: agente eziologico per reparto e anno

esamiperagenteeziologico <- select(covid, Tot_Eseguiti, Reparto, anno, Prova) 
esamiperagenteeziologicofilt <- filter(esamiperagenteeziologico, Prova == "SARS-CoV-2: agente eziologico") 
esamiperagenteeziologicofilt <- na.omit(esamiperagenteeziologicofilt) 
esamiperagenteeziologicofilt %>%  
  pivot_wider(names_from = Reparto,anno, values_from = Tot_Eseguiti, values_fn =sum) 


covid %>% 
  #filter(Prova == "SARS-CoV-2: agente eziologico") %>% 
  group_by(Reparto, anno) %>% 
  summarise(tot = sum(Tot_Eseguiti, na.rm = T))


#1-baselli 
covid_2 = covid %>%
  select(tot, anno) %>% #<-tot??
  group_by(anno) %>%
  summarise(tot = sum(tot, na.rm = TRUE), n = n())

#Iyad

#4--numero di esami eseguiti con la prova SARS-CoV-2: agente eziologico per reparto e anno

covid <-  clean_names(covid)
covid %>% 
  group_by("SARS-CoV-2: agente eziologico", anno, reparto) %>% #<----- ??
  summarise(tot_esami = sum(tot_eseguiti, na.rm = T))


#Reggiani

covid <- clean_names(covid)
names(covid)
unique(covid$comune)
NA %in% unique(covid$comune)
length(unique(covid$comune)) 

covid %>% mutate(materiale = replace(materiale, materiale %in% c("TAMPONE ", "TAMPOE", "TAMPONI"), "TAMPONE"),
                 materiale = replace(materiale, materiale %in% c("SALIVA ", "SALIVARI"), "SALIVA"),
                 materiale = replace(materiale, materiale %in% "RNA", "RNA SARS-CoV-2"),
                 materiale = replace(materiale, materiale %in% "materiale vari", "ALTRI MATERIALI"),
                 materiale = replace(materiale, materiale %in% "espettorato", "ESPETTORATO")) %>%
  group_by(materiale) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm = T)) %>% 
  mutate(prop= esami/sum(esami))
  pivot_wider(names_from = "materiale", values_from = "esami")

covid %>% 
 # mutate(anno = as.character(anno)) %>% 
  group_by(anno, conferente) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm=T))%>%
  pivot_wider(names_from = "anno", values_from = "esami")



# covid %>% clean_names() %>% 
#   group_by(anno) %>% 
#   summarise(totesami = sum(tot_eseguiti, na.rm = T))























## introduzione a ggplot2----




## dati sperimentazione topi----

#importazione-trasfomrazione-visualizzazione

mouse <- read_excel("C:/Users/vito.tranquillo/Desktop/SPERIMENTAZIONE prova132.xlsx", 
           range = "A2:T22", col_types = c("text", 
                                           "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "skip", "skip"))




dt <- mouse %>% 
  filter(!row_number() %in% c(1, 3, 7,11,16)) %>% 
  mutate(...1 = ifelse(is.na(...1), "idmouse", ...1)) %>% 
  row_to_names(row_number = 1) %>% 
  pivot_longer(2:18, names_to = "days", values_to = "weight") %>% 
    mutate(gruppi = ifelse(str_detect(idmouse, "1", ), "A", 
                                      ifelse(str_detect(idmouse, "2"), "B", 
                                                        ifelse(str_detect(idmouse, "3"), "C", "Controllo"))), 
      days = as.numeric(days)) 
  
  

#usiamo ggplot----

## come plottare seire di dati continui singoli cioè le distribuzioni----
ggplot(dt)+  
  #aes(x= weight)+#+ #, y = gruppi)+
 # geom_jitter(aes(y = ""), height = 0.2) +
  #geom_bar()
 # geom_histogram(bin=....)
    # geom_rug()+
    # geom_histogram(aes(y = ..density..), #<- mostra la proporzione di dati in un contenitore ( bin)
    #               color = "blue", fill="lightgrey")+
    #
    # geom_density()#adjust=1/3 <- per cambiare la forma della curva adattandola alla distriuzione

  #geom_jitter(aes(y = ""))+
 # stat_summary(aes(y= ""), color= "red", size = 0.8)

   #geom_boxplot(aes(y= "")) #, color = "navy", fill = "lightblue", width=0.3)+
   #geom_jitter(aes(y = "")) #, height = 0.2, color = ifelse(dt$weight == max(dt$weight, na.rm = T), "red",
                                                         #ifelse(dt$weight == min(dt$weight, na.rm = T),"red","black")),alpha = 0.3)
   #geom_violin(aes(y = ""), scale = "width")+
   #geom_jitter(aes(y = ""))
  #ggforce::geom_sina(aes(y = ""))
  #geom_hline(yintercept = 1) <- evidenzia come il violin plot deriva dal density curve


p1 <- ggplot(dt)+
  aes(x = days, y = weight)+ #col = gruppi)+
  geom_point()+
  geom_line()# aes(group = idmouse))


p2 <- ggplot(dt)+
  aes(x = days, y = weight, col = gruppi)+
  geom_point()+
  geom_line( aes(group = idmouse))


p3 <- ggplot(dt)+
  aes(x = days, y = weight, col = gruppi)+
  geom_point()+
  geom_line( aes(group = idmouse))+
  facet_wrap(.~ gruppi)

p4 <- ggplot(dt)+
  aes(x = days, y = weight, col = gruppi)+
  geom_point()+
  geom_line( aes(group = idmouse))+
  facet_wrap(.~ gruppi)+
  theme(
    legend.position = "none"
  )

p5 <- ggplot(dt)+
  aes(x = days, y = weight, col = gruppi)+
  #geom_violin(aes(x = factor(days), y = weight))+
  geom_line(aes(col = idmouse), alpha = 0.2)+
  geom_point( alpha = 0.2)+
  facet_wrap(.~ gruppi, scales = "free")+
  geom_smooth(se = FALSE, method = "lm")+
  theme(
    legend.position = "none"
  )



p6 <- ggplot(dt)+
  aes(x = days, y = weight, col = gruppi)+
  #geom_violin(aes(x = factor(days), y = weight))+
  #geom_line(aes(col = idmouse))+
  geom_point( alpha = 0.2)+
  #facet_wrap(.~ gruppi, scales = "free")+
  geom_smooth(se = FALSE, method = "lm")+
  theme(
    legend.position = "bottom"
  )

p7 <- ggplot(dt)+
  aes(x = days, y = weight, col = gruppi)+
  #geom_violin(aes(x = factor(days), y = weight))+
  #geom_line(aes(col = idmouse))+
  #geom_point( alpha = 0.2)+
  #facet_wrap(.~ gruppi, scales = "free")+
  geom_smooth(se = FALSE, method = "lm")+
  theme(
    legend.position = "bottom"
  )
  
  
library(patchwork)


((p1+p2+p3)/
  (p4+p5+p6))| p7

# aggiungere titolo, sottotitolo, didascalia ad un grafico

p8 <- p7+
  labs(
       # title = "Effetto della dieta sull'accrescimento nei topi neonati", 
       # subtitle = "Confronto tra quattro tipi di diete", 
       # caption = "le linee rappresentano le rette di regressione 
       # del peso nei giorni di osservazione per i differenti gruppi di dieta", 
       y = "Weight (gr)", 
       x = "Days")+
  xlim(1,max(dt$days))+
  ylim(0, max(dt$weight))+
  scale_x_continuous(breaks = c(1:17))+
  theme_bw()+

# modificare gli elementi del plot

theme(
  # plot.title = element_text(color = "blue", size = 18), 
  # plot.subtitle = element_text(size = 14), 
  # plot.caption = element_text(color = "blue", size = 12), 
  
  legend.title = element_blank(), 
  legend.position = "top", 
  
  axis.title.y = element_text(size = 15), 
  axis.title.x = element_text(size = 15), 
  
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14)
)


gruppi <- c( A = "Gruppo A: Dieta ipeproteica", 
             B = "Gruppo B: Dieta ipoproteica", 
             C = "Gruppo C: Dieta ipoproteca più integratore",
             Controllo = "Gruppo di controllo: Dieta normoproteica")

p9 <- ggplot(dt)+
  aes(x = days, y = weight, col = idmouse)+
  #geom_violin(aes(group = days))+
  geom_boxplot(aes(group = days))+
  geom_line(aes(group = idmouse))+
  ggforce::geom_sina(aes(group = days))+
  facet_wrap(.~ gruppi, scales = "free", labeller = labeller(gruppi = gruppi))
  

  
p10 <- 
  
  p9 +
  labs(
       # title = "Effetto della dieta sull'accrescimento nei topi neonati", 
       # subtitle = "Confronto tra quattro tipi di diete", 
       # caption = "nei diversi pannelli sono visualizzati,  per ogni giorno di osservazione, il peso dei singoli topi e
       # il boxplot della distribuzione del peso", 
       y = "Weight (gr)", 
       x = "Days")+
  xlim(1,max(dt$days))+
  ylim(0, max(dt$weight))+
  scale_x_continuous(breaks = c(1:17))+
  theme_bw()+
  theme(
    # plot.title = element_text(color = "blue", size = 18), 
    # plot.subtitle = element_text(size = 14), 
    # plot.caption = element_text(color = "blue", size = 12), 
    
    legend.title = element_blank(), 
    legend.position = "none", 
    
    axis.title.y = element_text(size = 15), 
    axis.title.x = element_text(size = 15), 
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14), 
    
    strip.text = element_text(color = "blue", 
                              size = 13)
    
  )

  

(p10 + p8) + plot_annotation(title = "Effetto della dieta sull'accrescimento nei topi neonati", 
                             subtitle = "Confronto tra quattro tipi di diete",
  
  tag_levels = "A", 
  caption = "A: distribuzione del peso dei singoli topi ( dato osservato e boxplot) nei singoli giorni di osservazione.
  B: le linee rappresentano le rette di regressione del peso nei giorni di osservazione per i differenti gruppi di dieta") & 
  theme(plot.title = element_text(color = "blue", size = 18), 
        plot.subtitle = element_text(size = 14), 
        plot.caption = element_text(color = "blue", size = 12)
  )


## dati fluorescenza cellule----

library(ggstatsplot)

fluo <- readRDS(here("dati", "fluo.rds"))
ggplot(fluo)+
  aes(x= fluorescenza,fill = ceppo)+
  geom_histogram(bins = 50, color = "black")

# ggplot(fluo)+
#   aes(x = fluorescenza, y = ceppo)+
#   geom_violin(scale = "width")+
#   #geom_boxplot()+
#  # geom_jitter(aes(group = ceppo), size = 0.03, alpha = 0.1)+
#   geom_sina(alpha = 0.3, size = 0.1)


fit <- lm(fluorescenza ~ ceppo, data = fluo)

p1 <- fluo %>% 
  ggplot()+
  aes(x=ceppo, y=fluorescenza, col = ceppo)+
  
  geom_violin()+
  ggforce::geom_sina(alpha = 0.03, size = 0.01)+
 # geom_boxplot(fill="firebrick4", width = 0.4, alpha = 0.6)+labs(x="")+
  labs(title="sample distribution of fluorescence")
  #scale_x_discrete(labels = c("Control","Treatment"))+
  #geom_jitter(alpha=0.01)+
  #coord_flip()

p2 <- fit %>% tidy(conf.int = TRUE) %>% 
  filter(term=="ceppoSL1344") %>% 
  ggplot(aes(term, estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  ylim(315,330)+
  coord_flip()+
  labs(title="95% C.I. estimate of effect size")+
  labs(x="", y="")+
  theme(axis.text.y = element_blank())


p1/p2


set.seed(123)

ggbetweenstats(
  data  = fluo,
  x     = ceppo,
  y     = fluorescenza,
  title = "distribution of fluorescence among bacterial strains"
)

# grafici per dati categorici ( numerosità / frequenza di diverse categorie)------

#basic barplot

dt <- readRDS(here("dati", "listeria.rds"))

x <- dt %>% mutate(st = paste0("ST",st)) %>%
  filter(!is.na(origin)) %>%
  mutate(origin = str_to_title(origin) ) %>%  
  group_by(origin) %>%
  tally()


  ggplot(dt)+
  aes(x = origin)+
    geom_bar(aes(y = (..count..)/sum(..count..))) 
  














dt %>% mutate(st = paste0("ST",st)) %>%
  filter(!is.na(origin)) %>%
  mutate(origin = str_to_title(origin) ) %>% 
  group_by(origin) %>%
  count() %>%
  arrange(desc(n)) %>% 
  ggplot()+
  aes(x = reorder(origin, +n), y = n)+
  geom_bar(color="black", stat = "identity", fill = "deepskyblue4", width = 0.5)+
  
  coord_flip()+
  theme_bw()+
  labs(y = "N. of isolates", x = "Source")





# data AMR



AMR_com%>% 
  filter(x=="TRUE") %>% 
  group_by("Gruppo"=Specieagg, SPECIE) %>% 
  summarise(n=n()) %>% 
  mutate(prop=round(100*(n/670),  2)) %>% 
  adorn_totals(where = "row")


AMR %>%  
  group_by(identificazione) %>% 
  filter(identificazione!="Non identificabile") %>% 
  tally() %>% arrange(desc(n)) %>% 
  mutate("prop(%)"=round(100*prop.table(n),2)) %>% 
  adorn_totals(where = c("row"))


ab<-AMR %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19)

ab[,13:19]<-apply(ab[,13:19], 2 , funz)

ab<-ab %>% 
  mutate(MDR = rowSums(.[13:19]))###antibiogram length###
ab$R<- ifelse(ab$MDR==0, 0, 1)
ab$MR<-ifelse(ab$MDR==0, "S", 
              ifelse(ab$MDR>=1 & ab$MDR<3, "R", "MR"))
#figura 2-ab length
ab %>% 
  drop_na(MDR) %>% 
  ggplot(aes(x=as.factor(MDR)))+geom_bar(aes(fill=MR))+
  labs(x="numero di resistenze al panel di antibiotici", 
       y="numero ceppi")+
  theme_ipsum_rc()+ theme(legend.title = element_blank())+
  scale_fill_brewer(labels = c("Ceppi multiresistenti", "Ceppi Resistenti", "Ceppi Suscettibili"), direction = -1)

amr %>% 
  filter(profilo!="SUSC") %>% 
  group_by(profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  arrange(n) %>% 
  #top_n(10, n) %>% 
  mutate(profilo = factor(profilo, unique(profilo))) %>% 
  #ggplot(aes(x=profilo, y=n))+geom_bar(stat = "identity")+coord_flip()
  ggplot(aes(x=profilo, y=n, label=n))+
  geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
  geom_point( aes(x=profilo, y=n), size=8.4, color="steelblue" )+
  geom_text(color="white", size=4)+
  coord_flip()+
  theme_ipsum_rc()+
  labs(y="n.ceppi",x="")

amr %>% 
  group_by(Specieagg,profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  ggplot( aes(Specieagg,profilo), label=n) + 
  geom_tile(aes(fill = n)) + 
  geom_text(aes(label = n), size=4) +
  scale_fill_gradient(low = "gray", high = "red")+
  #scale_fill_gradient(low = "lightgrey",high = "steelblue")+
  scale_x_discrete(expand = c(0, 0)) + theme_ipsum_rc()+
  scale_y_discrete(expand = c(0, 0)) + labs(x="Gruppo Specie")+
  theme(legend.position = "bottom",axis.ticks = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1,size=8),axis.text.y = element_text(size=8))



metares %>% 
  ggplot( aes(y=mean,ymin=lower, ymax=upper, x=articolo))+
  geom_point(color="blue", size=2)+geom_linerange(color="blue", size=.8)+
  coord_flip()+
  theme_ipsum_rc(axis_title_just = "mc")+
  facet_wrap(~specie)+
  labs(x="", y="Prevalenza")




# dati listeria 
library(RColorBrewer)
library(stringr)

dt <- readRDS(here("dati", "listeria.RDS"))

#fig.1
dt %>% mutate(st = paste0("ST",st)) %>%
  group_by(st, source,lineage) %>%
  count() %>%
  arrange(desc(n)) %>%
  
  ggplot()+
  aes(x = reorder(st,+n), y = n, fill = source)+
  geom_bar( color = "black", stat='identity', position = "stack")+
  facet_wrap(~lineage, ncol = 2, scales = "free", labeller = label_both)+
  coord_flip()+
  scale_fill_brewer(direction = +1, palette = "Blues")+
  theme_bw()+
  labs(y = "N. of isolates", x = "sequence types (ST)")

#fig.2
dt %>% mutate(st = paste0("ST",st)) %>%
  filter(!is.na(origin)) %>%
  mutate(origin = str_to_title(origin) ) %>% 
  group_by(origin) %>%
  count() %>%
  arrange(desc(n)) %>% 
  ggplot()+
  aes(x = reorder(origin, +n), y = n)+
  geom_bar(color="black", stat = "identity", fill = "deepskyblue4", width = 0.5)+
  
  coord_flip()+
  theme_bw()+
  labs(y = "N. of isolates", x = "Source")

#fig.3

dt %>% mutate(st = paste0("ST",st)) %>%
  group_by( serotype,source) %>%
  count() %>%
  arrange(n, .by_group = T) %>% 
  mutate(x = factor(interaction(source,serotype,  drop=TRUE))) -> my_df


ggplot(my_df)+
  aes(x = reorder(x, +n), y = n)+
  geom_bar(color="black", stat = "identity", fill = "deepskyblue4", width = 0.5)+
  facet_wrap(~ source, scales = "free")+
  theme_bw()+
  coord_flip()+
  labs(y = "N. of isolates", x = "Serotype")+
  scale_x_discrete(breaks = my_df$x, labels=gsub("^.*\\.", "", my_df$x))









### anaplasma
anap <- read_excel("dati/anaplasma.xlsx")





# grafici statistici----













# # frank harrell plot ----
# 
# library(broom)
# library(patchwork)
# dt <- read_excel("dati/twogroup.xlsx")
# 
# #plot_grid( (df() %>% 
# fit <- lm(y ~ group, data = dt)
# 
# p1 <- dt %>% 
#   ggplot()+
#   aes(x=group, y=y)+
#   geom_boxplot(fill="firebrick4", width = 0.4, alpha = 0.6)+labs(x="")+
#   labs(title="sample distribution of y")+
#   #scale_x_discrete(labels = c("Control","Treatment"))+
#   geom_jitter(alpha=0.5,position=position_jitter(w=0.1,h=0.1))+
#   coord_flip()
# 
# p2 <- fit %>% tidy(conf.int = TRUE) %>% 
#   filter(term=="groupTreatment") %>% 
#   ggplot(aes(term, estimate))+
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
#   ylim(-200, 200)+
#   coord_flip()+
#   labs(title="95% C.I. estimate of effect size")+
#   labs(x="", y="")+
#   theme(axis.text.y = element_blank())
# 
# 
# p1/p2





















#Incontro del 30 Maggio----

## Titanic data (senza crew)----
# 
unzip(here("dati","titanic.zip"), list = TRUE)
# 
dt1 <- read_csv(unzip(here("dati","titanic.zip"), "test.csv"))
dt2 <- read_csv(unzip(here("dati","titanic.zip"), "train.csv"))

survtest <- read_csv(unzip(here("dati","titanic.zip"), "gender_submission.csv"))

# 
dt <- dt1 %>%
  left_join(survtest, by = "PassengerId") %>% 
  bind_rows(dt2)
# 

 write.xlsx(dt, here("dati", "titanic.xlsx"))

 
table1
table2
table3
table4a
table4b

tcasi <- table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases")

tpop <- table4b %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "population")
  
  
table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases") %>% 
  left_join(
    table4b %>% 
      pivot_longer(cols = 2:3, names_to = "year", values_to = "population")
  ) %>% 
  mutate(rate = 1000*cases/population)


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
  select(cabin, pclass, survived) %>%
  group_by(cabin, pclass, survived) %>%
  count() %>%  
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




#dati cellulle Berni----
library(vioplot)
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
  #mutate(duplicati = ifelse(duplicated(id_singola_cell) == "TRUE", "si", "no")) %>% 
  na.omit() %>% saveRDS("fluo.rds")
  # group_by(ceppo) %>%
  # summarise(n = n(),
  #           media = mean(fluorescenza),
  #           sd = sd(fluorescenza))
  
  ggplot(dt)+ theme_bw()+
  # aes(y = fluorescenza, x = seq(1:nrow(dt)), col = ceppo)+
    aes(fluorescenza, group = ceppo)+
    geom_histogram(binwidth = 50,  color = "black", fill = "lightgrey")+
    facet_wrap(~ ceppo)
    
    #geom_jitter(size = 0.1, alpha = 0.3)+
    #geom_boxplot()+
   # scale_fill_manual(values=c("#868E74", "#AD5988"))
  
  
  
  
  geom_density(alpha = 0.6)+
  scale_fill_manual(values=c("#868E74", "#AD5988"))+
   facet_grid(rep_tec~ rep_bio, scales = "free")


# con vioplot
  
  # vioplot(ER1175$ER1175,
  #         colMed = "green", # Median with a line
  #         side = "right",   # Right side
  #         col = "#5773CC") 
  # 
  # vioplot(SL1344$SL1344,
  #         colMed = "green", # Median with a line
  #         side = "left",   # Right side
  #         col = "#FFB900",   # Color for the left side
  #         add = TRUE)  
  # legend("topleft",
  #        legend = c("ER1175", "SL1344"),
  #        fill = c("#5773CC", "#FFB900"))
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
  separate(sexage, c("sex", "age"), sep = 1) %>%
  group_by(country, year) %>% 
  summarise(totcasi = sum(cases)) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = "year", values_from = "totcasi", values_fill = 0)

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


# covid----

covid <- read.csv("C:/Users/vito.tranquillo/Desktop/Git Projects/CorsoR/dati/covid.csv")

covid 


