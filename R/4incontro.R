

#Incontro 20 Giugno----

library(crosstable)
library(flextable)
library(zoo)
library(rstatix)
library(gtsummary)
library(RColorBrewer)
library(patchwork)
library(hrbrthemes)
library(stringr)


# GRAFICI CON GGPLOT2 2a parte-----

## dati fluorescenza cellule----

library(ggstatsplot)

fluo <- readRDS(here("dati", "fluo.rds"))

ggplot(fluo)+
  aes(x= fluorescenza,fill = ceppo)+
  geom_histogram(bins = 50, color = "black")

ggplot(fluo)+
  aes(x = fluorescenza, y = ceppo)+
  geom_violin(scale = "width")+
  #geom_boxplot()+
  # geom_jitter(aes(group = ceppo), size = 0.03, alpha = 0.1)+
  ggforce::geom_sina(alpha = 0.3, size = 0.1)


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

p2 <- 
  
  fit %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="ceppoSL1344") %>% 
  ggplot(aes(term, estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  ylim(315,330)+
  coord_flip()+
  labs(title="95% C.I. estimate of effect size")+
  labs(x="", y="")+
  theme(axis.text.y = element_blank())


p1/p2


#set.seed(123)

# ggbetweenstats(
#   data  = fluo,
#   x     = ceppo,
#   y     = fluorescenza,
#   title = "distribution of fluorescence among bacterial strains"
# )

#Grafici per dati categorici ( numerosità / frequenza di diverse categorie)------

##basic barplot----
### dati listeria----

dt <- readRDS(here("dati", "listeria.rds"))

x <- 
  dt %>% mutate(st = paste0("ST",st)) %>% 
  filter(!is.na(origin)) %>%
  mutate(origin = str_to_title(origin) ) %>%  # rende maiuscola le prime lettere di parole in una frase
  group_by(origin) %>%
  tally()  # aggiungete per ogni origine la % di ceppi ..... 

dt %>% mutate(st = paste0("ST",st)) %>%
  filter(!is.na(origin)) %>%
  mutate(origin = str_to_title(origin) ) %>% 
  group_by(origin) %>%
  count() %>%
  #arrange(desc(n)) %>% 
  ggplot()+
  aes(x = reorder(origin, n),  y = n)+
  # aes(x = reorder(origin, n), y = n)+
  
  geom_bar(color="black",  fill = "deepskyblue4", width = 0.5, stat = "identity")+
  
  coord_flip()+
  theme_bw()+
  labs(y = "N. of isolates", x = "")


ggplot(dt)+
  aes(x = origin)+
  geom_bar(aes(y = after_stat(count/sum(count))))


dt %>% mutate(st = paste0("ST",st)) %>%
  group_by( serotype,source) %>%
  count() %>% 
  arrange(n, .by_group = T) %>% 
  mutate(x = factor(interaction(source,serotype,  drop=TRUE))) -> my_df


ggplot(my_df)+
  aes(x = reorder(x, n), y = n)+
  geom_bar(color="black", stat = "identity", fill = "deepskyblue4", width = 0.5)+
  facet_wrap(~ source, scales = "free")+
  theme_bw()+
  coord_flip()+
  labs(y = "N. of isolates", x = "Serotype")+
  
  scale_x_discrete(breaks = my_df$x, labels=gsub("^.*\\.", "", my_df$x))





### dati AMR---

AMR_com <- readRDS(here("dati", "AMR_com.rds"))

AMR_com%>% 
  filter(x=="TRUE") %>% 
  group_by("Gruppo"=Specieagg, SPECIE) %>% 
  summarise(n=n()) %>%  
  mutate(prop=round(100*(n/670),  2)) %>% 
  adorn_totals(where = "row") #<- far fare in aula barplot o dotplot




AMR<- readRDS(here("dati", "AMR.rds"))

AMR %>%  
  group_by(identificazione) %>% 
  #filter(identificazione!="Non identificabile") %>% 
  tally() %>% arrange(desc(n)) %>%  
  mutate("prop(%)"=round(100*prop.table(n),2)) %>% 
  adorn_totals(where = c("row")) #<- far fare in aula barplot o dotplot



funz<-function(x){
  abs(as.numeric(as.factor(x))-2)
}




ab<-AMR %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19)

ab[,13:19]<-apply(ab[,13:19], 2 , funz)

ab<-ab %>% 
  mutate(MDR = rowSums(.[13:19]))   ###antibiogram length###

ab$R<- ifelse(ab$MDR==0, 0, 1)
ab$MR<-ifelse(ab$MDR==0, "S", 
              ifelse(ab$MDR>=1 & ab$MDR<3, "R", "MR"))
#figura 2-ab length


ab %>% 
  drop_na(MDR) %>% 
  ggplot(aes(x=as.factor(MDR)))+
  geom_bar(aes(fill=MR))+
  labs(x="numero di resistenze al panel di antibiotici", 
       y="numero ceppi")+
  theme_ipsum_rc()+ 
  theme(legend.title = element_blank())+
  scale_fill_brewer(labels = c("Ceppi multiresistenti", "Ceppi Resistenti", "Ceppi Suscettibili"), direction = -1)

# questo codice permette di creare una colonna che identificaa i diversi profili di resistenza al pannello di antibiotici utilizzato
# e prepara il dataset amr che sarà utilizzato per fare il dotplot e l'heatmap che seguono
amr <- AMR %>% 
  drop_na(Specieagg) %>% 
  filter(identificazione!="Non identificabile") %>% 
  dplyr::select(-15,-18,-19) 
amr$COL<-ifelse(amr$COL=='R', 'COL',0)
amr$CFT<-ifelse(amr$CFT=='R', 'CFT',0)
amr$KAN<-ifelse(amr$KAN=='R', 'KAN',0)
amr$ENR<-ifelse(amr$ENR=='R', 'ENR',0)
amr$GEN<-ifelse(amr$GEN=='R', 'GEN',0)
amr$TET<-ifelse(amr$TET=='R', 'TET',0)
amr$AMP<-ifelse(amr$AMP=='R', 'AMP',0)
amr[,13:19]<-amr[,13:19] != 0
nomi_abb<-toupper(abbreviate(names(amr)[13:19]))
X<-  apply(amr[, 13:19], 1, function(x) nomi_abb[x])
XX<-lapply(X, paste, collapse="-")
amr$profilo<-unlist(XX)
amr<-amr %>% 
  filter(!profilo  %in% c("NA-NA-NA-NA-NA-NA-NA")) %>% 
  mutate( profilo= ifelse(profilo=="", "SUSC", profilo))


amr %>% 
  filter(profilo!="SUSC") %>% 
  group_by(profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  arrange(n) %>% 
  #top_n(10, n) %>% 
  mutate(profilo = factor(profilo, unique(profilo))) %>% 
  #ggplot(aes(x=profilo, y=n))+geom_bar(stat = "identity")+coord_flip()
  ggplot()+
  aes(x=profilo, y=n, label=n)+
  
  
  geom_segment( aes(x=profilo, xend=profilo, y=0, yend=n), color="grey")+
  geom_point( aes(x=profilo, y=n), size=8.4, color="steelblue" )+
  geom_text(color="white", size=4)+
  coord_flip()+
  theme_ipsum_rc()+
  labs(y="n.ceppi",x="")




amr %>% 
  filter(profilo!="SUSC") %>% 
  group_by(profilo) %>% 
  dplyr::summarise(n=n()) %>% 
  arrange(n) %>% 
  mutate(profilo = factor(profilo, unique(profilo))) %>% 
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
  theme(legend.position = "bottom",axis.ticks = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1,size=8),axis.text.y = element_text(size=8))




metares <- readRDS(here("dati", "metares.rds"))



metares %>% 
  ggplot( aes(y=mean,ymin=lower, ymax=upper, x=articolo))+
  geom_point(color="blue", size=2)+geom_linerange(color="blue", size=.8)+
  coord_flip()+
  theme_ipsum_rc(axis_title_just = "mc")+
  facet_wrap(~specie)+
  labs(x="", y="Prevalenza")




# dati listeria 





# STATISTICA CON R----


#t-test e lm
# T-test
dt <-  data.frame(A = rnorm(20, 100, 20),
                  B = rnorm(20, 80, 35))  # Fix mean and SD

t.test(dt$A, dt$B)
wilcox.test(dt$A, dt$B)


dt2 <- dt %>% 
  pivot_longer(1:2, names_to = "gruppo", values_to =  "Y")


t.test(dt2$Y~dt2$gruppo)
wilcox.test(dt2$Y~dt2$gruppo)
kruskal.test(dt2$Y~dt2$gruppo)


fit <- lm(Y ~ gruppo, data = dt2)
confint(fit)
fit$residuals # normalità dei residui


fitaov <- aov(Y ~ gruppo, data = dt2)
summary(aov)



D  <-  data.frame(
  disease = c('positive', 'positive', 'negative', 'negative'),
  sex = c('male', 'female', 'male', 'female'),
  Freq = c(100, 70, 30, 32)
)

D_table  <-   D %>%
  spread(key = disease, value = Freq) %>%  # Mood to columns
  select(-sex) %>%  # Remove sex column
  as.matrix()

chisq.test(D_table)
fisher.test(D_table)





# useremo i dati sulle bronchioliti di bambini durante il covid



dtplot <- readRDS(here("dati", "bronchiolitiplot.rds"))

ggplot(dtplot)+
  aes(x= YearMonth, y = n )+
  geom_rect( aes(xmin = as.yearmon("2019-9"), xmax = as.yearmon("2020-4"), ymin = -Inf, ymax = Inf),
             alpha = 0.05, fill = "lightgrey") +
  annotate("text", label = "Season I \n (Sett 2019-Apr 2020) \n #43 cases", color = "blue", x = as.yearmon("2020-1"),
           y = Inf, angle = 0, vjust = 2)+
  
  geom_rect( aes(xmin = as.yearmon("2020-9"), xmax = as.yearmon("2021-4"), ymin = -Inf, ymax = Inf),
             alpha = 0.05, fill = "lightgray") +
  annotate("text", label = "Season II \n (Sett 2020-Apr 2021) \n #6 cases",color = "blue", x = as.yearmon("2021-1"),
           y = Inf, angle = 0, vjust = 2   )+
  geom_rect( aes(xmin = as.yearmon("2021-9"), xmax = as.yearmon("2022-4"), ymin = -Inf, ymax = Inf),
             alpha = 0.05, fill = "lightgrey") +
  annotate("text", label = "Season III \n (Sett 2021-Apr 2022) \n #78 cases",color = "blue", x = as.yearmon("2022-1"),
           y = Inf, angle = 0, vjust = 2  )+
  
  geom_point()+
  geom_line(group = 1) +
  
  labs(y = "# cases admitted", x ="")+
  theme_bw()



## Bronchioliti----
# Crosstable

dt <- readRDS(here("dati", "bronchioliti.rds")) 


crosstable(dt,
           cols = 53,
           by= Season,
           test = TRUE,
           showNA = "ifany") %>% 
  as_flextable()




dt %>% 
  select(-Name, -Surname) %>% 
  saveRDS(here("dati", "bronchioliti.rds"))




crosstable(dt,
           cols = "Lenght-of-stay",
           by= Season,test = TRUE,
           showNA = "ifany") %>% 
  as_flextable()


dt %>%
  ggplot()+
  geom_boxplot()+
  aes(x= as.factor(YearMonth) , y = `Lenght-of-stay` , fill = Season )+
  geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  labs(y = "Lenght-of-stay", x ="")+
  theme_bw()

# età al ricovero
dt %>%
  ggplot()+
  geom_boxplot()+
  aes(x= as.factor(YearMonth) , y = `Age at Hospitalization (in months)` , fill = Season )+
  geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  labs(y = "Age at Hospitalization (in months)", x ="")+
  theme_bw()


crosstable(dt,
           cols = "Age at Hospitalization (in months)",
           by= Season,test = TRUE,
           showNA = "ifany") %>%
  as_flextable()



crosstable(dt,cols = c(6,7,8, 9, 10,11, 12, 13),
           by = Season,test = TRUE,
           total = "column",
           showNA = "ifany") %>%
  as_flextable()  


### Condizioni cliniche al ricovero


crosstable(dt,cols = c(26, 27, 28, 29, 30, 60,  33, 34),
           by = Season, test = TRUE,
           #total = "column",
           showNA = "ifany") %>%
  as_flextable() 



WBC

dt %>%
  filter(!is.na(WBC)) %>%
  ggplot()+
  aes(x=Season, y = WBC)+
  geom_boxplot(alpha = 0.3)+ geom_jitter(alpha = 0.3)+
  coord_flip()+
  geom_hline(yintercept = 13, color = "red")+
  theme_bw()+ labs(y = "WBC x 1000", x = "")





CRP


dt %>%
  filter(!is.na(CRP)) %>%
  ggplot()+
  aes(x=Season, y = CRP)+
  geom_boxplot(alpha = 0.3)+ geom_jitter(alpha = 0.3)+
  coord_flip()+
  geom_hline(yintercept = 30, color = "red")+
  theme_bw()+ labs(y = "CRP", x = "")
