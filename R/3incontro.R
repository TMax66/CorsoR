# 3^ incontro

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


## dati zecche codice per classificare le zecche sulla base della positività ad almeno uno dei patogeni ricercati


dati  <- ### importate i dati sulle zecche usando Import data in RStudio

dati <- read_excel("C:/Users/vito.tranquillo/Desktop/dataset esempi per corso/zecche.xlsx")

#questo codice pulisce un pò il dataset 
  
dtpat <- dati %>%
  filter(!is.na(esito)) %>%
  
  mutate(
    nconfcamp = paste(nconf,ncamp), 
    stadio = ifelse(stadio %in% c("femmina", "maschio", "Maschio", "Femmina"), "Adult",
                    ifelse(stadio == "larva", "Larvae",
                           ifelse(stadio == "ninfa", "Nymphae", stadio)))) %>% 
  filter(stadio != "n.d.")

  
  
pat <- dtpat %>% select(annoreg,nconfcamp, stadio, specie, comune, provincia, 
                        altitudine, prova, esito) %>% 
  mutate(pat = ifelse(esito != "Non dimostrata presenza", "1", "0")) %>%  
  pivot_wider(names_from = "prova", values_from = "pat", values_fill = '0' ) %>%  
  mutate(across(c(10:14), as.numeric)) %>% 
  group_by(annoreg,nconfcamp, stadio, specie, comune, provincia, altitudine) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  rowwise() %>%  
  mutate(Pat = sum(c_across(c(8:12)), na.rm = T),
         Pat2 = ifelse(Pat == 0, 0, 1), 
         PatCat = ifelse(Pat2 == 0 , "Neg", "Pos"),
         stadio = factor(stadio, levels =c("Larvae", "Nymphae",  "Adult"))
  )%>%
