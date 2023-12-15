source(here("R", "librerie.R"))

library(tidyverse)
library(openxlsx)
library(readxl)
library(rmarkdown)
library(knitr)
library(gt)
library(janitor)



titanic <- read_excel("dati/titanicmod.xlsx")

head(titanic)
str(titanic)
glimpse(titanic)

titanic <- clean_names(titanic)

titanic <- rename(titanic, sblsp = siblings_and_spouses_on_board, 
                  parch = parent_children_on_board)

# verbs----

newdt <- select(titanic, pclass, name, sex)
select(titanic, -c(6:8))
select(titanic, where(is.numeric))

slice(titanic, 1:3)
top_n(titanic, 10)

View(arrange(titanic, desc(fare))
     
filter(titanic, sex == "male")
filter(titanic, sex != "male")
filter(titanic, sex != "male", sex != "Male", sex != "male_")
filter(titanic, !sex %in% c("male", "Male", "male_"))
View(filter(titanic, 
            sex == "female", 
            pclass == 3, 
            !is.na(cabin)))

View(mutate(titanic, sex= if_else(sex %in% c("Male", "male_"), "male", sex)))

glimpse(mutate(titanic, sex2= if_else(sex %in% c("Male", "male_"), "male", sex)))

glimpse(mutate(titanic, survived = as.factor(survived)))

glimpse(mutate(titanic, across(c("survived", "sex", "embarked"), as.factor)))



View(
  summarise(titanic, 
            mediatariffa = mean(fare, na.rm = T), 
                     std = sd(fare, na.rm = T), 
                     n = n(), 
            tar1000 = fare*1000
            ) 
  )

dtgrouped <- group_by(titanic, pclass, sex)


write.xlsx(
  (summarise(dtgrouped, mediat = mean(fare, na.rm = T),
          n = n())), file = "tabella2.xlsx")

surv <- filter(titanic, survived == 1)
mean(titanic$survived)

summarise(dtgrouped, surv = mean(survived, na.rm = T))

# pipe-----

titanic <- read_excel("dati/titanicmod.xlsx")
titanic <- clean_names(titanic)

titanic %>% 
  rename(sblp = siblings_and_spouses_on_board, 
         parch = parent_children_on_board) %>% 
  mutate(age = replace(age, age %in% c("//", "ND"), NA ),
         age = as.numeric(age), 
         sex = if_else(sex %in% c("Male", "male_"), "male", sex), 
         across(c("sex", "embarked", "pclass"), as.factor)) %>% 
  group_by(pclass, sex) %>% 
  summarise(Surv = round(mean(survived),2), 
            n = n()) %>% 
  mutate(Surv = paste0(Surv, "(", n, ")")) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "sex", values_from = "Surv" )
  





table1
table2
table3
table4a
table4b



