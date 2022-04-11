
attitude_violence <- read.csv("/Users/margheritamaroni/Desktop/Datasets/attitude_towards_violence.csv", sep =";", header = TRUE)
women_parl <- read.csv("/Users/margheritamaroni/Desktop/Datasets/women_parlamentarians.csv", sep = ";", header = TRUE)
discrim_family <- read.csv("/Users/margheritamaroni/Desktop/Datasets/Discrimination_in_family.csv", sep = ';', header = TRUE)
law_dom_violence <- read.csv("/Users/margheritamaroni/Desktop/Datasets/laws_domestic_violence.csv", sep =";", header =TRUE)
prevalence_lifetime <- read.csv("/Users/margheritamaroni/Desktop/Datasets/prevalence_lifetime.csv", sep =";", header = TRUE)
restricted_access_asset <- read.csv("/Users/margheritamaroni/Desktop/Datasets/restricted_access_asset.csv", sep =";", header = TRUE)
restricted_civil_liberties <- read.csv("/Users/margheritamaroni/Desktop/Datasets/restricted_civil_liberties.csv", sep =";", header = TRUE)
restricted_physical_integrity <- read.csv("/Users/margheritamaroni/Desktop/Datasets/restricted_physical_integrity.csv", sep =";", header = TRUE)

#sistemiamo il primo dataset: teniamo solo il valore e il paese 
attitude_violence <- attitude_violence[, -c(2:6)]
attitude_violence <- attitude_violence[, -3]
#rinominiamo la colonna Value con il nome della variabile 
library(tidyverse)
attitude_violence <- attitude_violence %>% 
  rename(
    attitude_viol = Value
  )

#sistemiamo women_parl 
women_parl <- women_parl[,-c(2:6)]
women_parl <- women_parl %>%
  rename(
    women_parliament = X.5
  )

#sistemiamo discrimination_family 
discrim_family <- discrim_family[,-c(2:6)]
discrim_family <- discrim_family[,-3]
discrim_family <- discrim_family %>%
  rename(
    discrim_family = Value
  )

#sistemiamo law_domestic_violence dataset 
law_dom_violence <- law_dom_violence[, -c(2:6)]
law_dom_violence <- law_dom_violence[,-3]
law_dom_violence <- law_dom_violence %>% 
  rename(
    law_domestic_violence = Value
  )

#sistemiamo prevalence_lifetime 
prevalence_lifetime <- prevalence_lifetime[,-c(2:6)]
prevalence_lifetime <- prevalence_lifetime[,-3]
prevalence_lifetime <- prevalence_lifetime %>%
  rename(
    prevalence_life = Value
  )

#sistemiamo restricted access to asset index
restricted_access_asset <- restricted_access_asset[,-c(2:6)]
restricted_access_asset <- restricted_access_asset[,-3]
restricted_access_asset <- restricted_access_asset %>%
  rename(
    restricted_asset = Value
  )

#sistemiamo restricted civil liberties 
restricted_civil_liberties <- restricted_civil_liberties[,-c(2:6)]
restricted_civil_liberties <- restricted_civil_liberties[,-3]
restricted_civil_liberties <- restricted_civil_liberties %>%
  rename(
    restricted_liberties = Value
  )

#sistemiamo restricted physical integrity 
restricted_physical_integrity <- restricted_physical_integrity[,-c(2:6)]
restricted_physical_integrity <- restricted_physical_integrity[,-3]
restricted_physical_integrity <- restricted_physical_integrity %>%
  rename(
    restricted_physical = Value
  )


#Creiamo un unico dataset usando la funzione merge 
merge_1 <- merge(attitude_violence, discrim_family, by = "LOCATION")
merge_2 <- merge(merge_1, law_dom_violence, by ="LOCATION")
merge_3 <- merge(merge_2, prevalence_lifetime, by ="LOCATION")
merge_4 <- merge(merge_3, restricted_access_asset, by ="LOCATION")
merge_5 <- merge(merge_4, restricted_civil_liberties, by ="LOCATION")
merge_6 <- merge(merge_5, restricted_physical_integrity, by ="LOCATION")
dataset <- merge(merge_6, women_parl, by ="LOCATION")
write.csv(dataset, "dataset_csv")
str(dataset)

#Guardiamo come sono distribuite le nostre otto variabili con dei boxplot 
#Prima importiamo la libreria che ci serve: ggpubr
library(ggpubr)

#1. Attitude towards violence 
ggboxplot(dataset, y = "attitude_viol", color='pink',
          palette = c("#violet", "#blue", "red")) +
  geom_hline(yintercept = mean(dataset$attitude_viol), linetype = 2)+ 
  ylim(0, 65)

#aggiungiamo i nomi dei Paesi che stiamo considerando 
library(car)
rownames(dataset)<- dataset$LOCATION
Boxplot(~attitude_viol, data=dataset, col="blue", labels=dataset$LOCATION)
#abbiamo un outlier 

#vediamo con un'istogramma 
hist(dataset$attitude_viol)

#2.Discrimination family 
ggboxplot(dataset, y = "discrim_family", color='pink',
          palette = c("#violet", "#blue", "red")) +
  geom_hline(yintercept = mean(dataset$discrim_family), linetype = 2)+ 
  ylim(0, 65)
#aggiungiamo nomi dei paesi 
rownames(dataset)<- dataset$LOCATION
Boxplot(~discrim_family, data=dataset, col="blue", labels=dataset$LOCATION)
#due outliers 

hist(dataset$discrim_family)

#3. law domestic violence 
ggboxplot(dataset, y= "law_domestic_violence", color = "pink", 
          palette = c("#violet", "#blue", "#red")) +
  geom_hline(yintercept = mean(dataset$law_domestic_violence), linetype = 2)+
  ylim(0,65)
rownames(dataset)<- dataset$LOCATION
Boxplot(~law_domestic_violence, data=dataset, col="blue", labels=dataset$LOCATION)

hist(dataset$law_domestic_violence)

#4. prevalence life 
ggboxplot(dataset, y= "prevalence_life", color = "pink", 
          palette = c("#violet", "#blue", "#red")) +
  geom_hline(yintercept = mean(dataset$prevalence_life), linetype = 2)+
  ylim(0,65)
rownames(dataset)<- dataset$LOCATION
Boxplot(~prevalence_life, data=dataset, col="blue", labels=dataset$LOCATION)

hist(dataset$prevalence_life)

#5. restricted access to asset 
ggboxplot(dataset, y= "restricted_asset", color = "pink", 
          palette = c("#violet", "#blue", "#red")) +
  geom_hline(yintercept = mean(dataset$restricted_asset), linetype = 2)+
  ylim(0,65)
rownames(dataset)<- dataset$LOCATION
Boxplot(~restricted_asset, data=dataset, col="blue", labels=dataset$LOCATION)

hist(dataset$restricted_asset)

#6. restricted liberties 
ggboxplot(dataset, y= "restricted_liberties", color = "pink", 
          palette = c("#violet", "#blue", "#red")) +
  geom_hline(yintercept = mean(dataset$restricted_liberties), linetype = 2)+
  ylim(0,65)
rownames(dataset)<- dataset$LOCATION
Boxplot(~restricted_liberties, data=dataset, col="blue", labels=dataset$LOCATION)

hist(dataset$restricted_liberties)

#7. restricted physical 
ggboxplot(dataset, y= "restricted_physical", color = "pink", 
          palette = c("#violet", "#blue", "#red")) +
  geom_hline(yintercept = mean(dataset$restricted_physical), linetype = 2)+
  ylim(0,65)
rownames(dataset)<- dataset$LOCATION
Boxplot(~restricted_physical, data=dataset, col="blue", labels=dataset$LOCATION)

hist(dataset$restricted_physical)

#8. women in parliament 
ggboxplot(dataset, y= "women_parliament", color = "pink", 
          palette = c("#violet", "#blue", "#red")) +
  geom_hline(yintercept = mean(dataset$women_parliament), linetype = 2)+
  ylim(0,65)
rownames(dataset)<- dataset$LOCATION
Boxplot(~women_parliament, data=dataset, col="blue", labels=dataset$LOCATION)

hist(dataset$women_parliament)

####CONFRONTIAMO I PAESI #####

#facciamo un bar chart e vediamo le differenze tra italia e germania su restricted_liberties 
rownames(dataset)<- c(1:length(rownames(dataset)))
ggplot(dataset[c(52,26),],aes(x=reorder(LOCATION, +restricted_liberties), y = restricted_liberties))+
  geom_bar(stat = "identity", fill = "pink", alpha = 0.6, width = 0.5) +
  xlab("STATI")+
  ylab("DIRITTI CIVILI")

#possiamo ripetere lo stesso per tanti paesi che vogliamo confrontare
#su tutti gli aspetti che desideriamo 

### MEDIA E MEDIANA PER LE VARIABILI NUMERICHE ###
#1. attitude_violence 
att_viol_mean <- mean(dataset$attitude_viol)
att_viol_mean 

att_viol_median <- median(dataset$attitude_viol)
att_viol_median

#2. discrim_family 
discrim_family_mean <- mean(dataset$discrim_family)
discrim_family_mean 

discrim_family_median <- median(dataset$discrim_family)
discrim_family_median

#3. law domestic violence 
law_domestic_mean <- mean(dataset$law_domestic_violence)
law_domestic_mean 

law_domestic_median <- median(dataset$law_domestic_violence)
law_domestic_median

#4. prevalence life 
prevalence_life_mean <- mean(dataset$prevalence_life)
prevalence_life_mean 

prevalence_life_median <- median(dataset$prevalence_life)
prevalence_life_median

#5. restricted asset 
restricted_asset_mean <- mean(dataset$restricted_asset)
restricted_asset_mean 

restricted_asset_median <- median(dataset$restricted_asset)
restricted_asset_median

#6. restricted liberties 
restricted_liberties_mean <- mean(dataset$restricted_liberties)
restricted_liberties_mean 

restricted_liberties_median <- median(dataset$restricted_liberties)
restricted_liberties_median

#7. restricted physical 
restricted_physical_mean <- mean(dataset$restricted_physical)
restricted_physical_mean 

restricted_physical_median<- median(dataset$restricted_physical)
restricted_physical_median

#8. women in parliament 
women_parliament_mean <- mean(dataset$women_parliament)
women_parliament_mean 

women_parliament_median <- median(dataset$women_parliament)
women_parliament_median

#Facciamo una tabella dove vediamo per ogni variabile:
# - media
# - mediana 
# - minimo 
# - massimo 
tabella <- list()
for (i in c(2:9)) {
  x <- list(colnames(dataset[i]), round(mean(dataset[,i],2)), median(dataset[,i]), min(dataset[,i]), max(dataset[,i]))
  tabella <- rbind(tabella, x)
} #qui otteniamo una lista di liste 

#la vogliamo rendere dataframe 
tabella <- as.data.frame(tabella)
colnames(tabella) <- c("Variabile", "Media", "Mediana", "Minimo", "Massimo")
rownames(tabella) <- c(1:8)
tabella

summary(dataset)
####### FREQUENZE ######


#Se vogliamo guardare le frequenze, dobbiamo rendere qualitative
#le nostre variabili 

#Possiamo creare delle categorie per vedere con più chiarezza a che categoria appartengono i vari Paesi del mondo 

#Per esempio possiamo guardare quanto, nei Paesi che stiamo considerando,
#le donne siano sottoposte a restrizioni sul piano delle libertà individuali 
library(ggplot2)
ggplot( dataset, aes(x=restricted_liberties)) +
  geom_histogram( binwidth=3, fill="pink", color="violet", alpha=0.9) +
  ggtitle("Restrizione delle libertà") + 
  xlab("Quanto un paese vive restrizioni")+
  ylab("Quanti paesi hanno restrizioni")+
  theme(
    plot.title = element_text(size=15)
  ) 

#### Creazione delle variabili categoriche ####

#1. attitude towards violence 
min(dataset$attitude_viol) #0
max(dataset$attitude_viol) #92.1

dataset$attitude_viol_cat <- cut(dataset$attitude_viol, breaks=c(-1,25,50,100),
                                 labels = c("bassa", "media", "alta"))
summary(dataset$attitude_viol_cat)

#Vediamo quali paesi hanno una bassa attitudine verso la violenza di genere
library(tidyverse)
bassa <- dataset %>% filter(dataset$attitude_viol_cat == 'bassa')
bassa$LOCATION
count(bassa) #70 paesi 

#Vediamo quali paesi hanno una media attitudine verso la violenza di genere
media <- dataset %>% filter(dataset$attitude_viol_cat == 'media')
media$LOCATION
count(media) #29 paesi 

#Vediamo quali paesi hanno un'alta attitudine verso la violenza di genere
alta <- dataset %>% filter(dataset$attitude_viol_cat == 'alta')
alta$LOCATION
count(alta) #20 paesi 

#Grafici - visualizzazione 
table(dataset$attitude_viol_cat)
pie(table(dataset$attitude_viol_cat))
barplot(table(dataset$attitude_viol_cat), col="pink")

#2. Discriminazioni in famiglia 
min(dataset$discrim_family) #0.1
max(dataset$discrim_family) #89.9

dataset$discrim_family_cat <- cut(dataset$discrim_family, breaks=c(0,25,50,100),
                                 labels = c("poche", "abbastanza", "molte"))
summary(dataset$discrim_family_cat)

#Vediamo quali paesi hanno poche discriminazioni familiari di genere
library(tidyverse)
poche <- dataset %>% filter(dataset$discrim_family_cat == 'poche')
poche$LOCATION
count(poche) #30 paesi 

#Vediamo quali paesi hanno abbastanza discriminazioni familiari di genere
abbastanza <- dataset %>% filter(dataset$discrim_family_cat == 'abbastanza')
abbastanza$LOCATION
count(abbastanza) #57 paesi 

#Vediamo quali paesi hanno molte discriminazioni familiari di genere
molte <- dataset %>% filter(dataset$discrim_family_cat == 'molte')
molte$LOCATION
count(molte) #32 paesi 

#Grafici - visualizzazione 
table(dataset$discrim_family_cat)
pie(table(dataset$discrim_family_cat))
barplot(table(dataset$discrim_family_cat), col="pink")

#3. Leggi sulla violenza domestica (0 = no discriminazione, 1 = donne totalmente discriminate)
min(dataset$law_domestic_violence) #0.25
max(dataset$law_domestic_violence) #1

dataset$law_domestic_violence_cat <- cut(dataset$law_domestic_violence, breaks=c(0,0.30,0.60,1),
                                  labels = c("poca_discriminazione", "discriminazione_significativa", "alta_discriminazione"))
summary(dataset$law_domestic_violence_cat)

#Poca discriminazione
library(tidyverse)
poca_discriminazione <- dataset %>% filter(dataset$law_domestic_violence_cat == 'poca_discriminazione')
poca_discriminazione$LOCATION
count(poca_discriminazione) #30 paesi 

#Discriminazione significativa
discriminazione_significativa <- dataset %>% filter(dataset$law_domestic_violence_cat == 'discriminazione_significativa')
discriminazione_significativa$LOCATION
count(discriminazione_significativa) #28 paesi 

#alta discriminazione 
alta_discriminazione <- dataset %>% filter(dataset$law_domestic_violence_cat == "alta_discriminazione")
alta_discriminazione$LOCATION
count(alta_discriminazione) #61

#Grafici - visualizzazione 
table(dataset$law_domestic_violence_cat)
pie(table(dataset$law_domestic_violence_cat))
barplot(table(dataset$law_domestic_violence_cat), col="pink")

#4. Prevalence life 
min(dataset$prevalence_life) #1.9%
max(dataset$prevalence_life) #85% 

dataset$prevalence_cat <- cut(dataset$prevalence_life, breaks=c(0,20,50,90),
                                    labels = c("poca_violenza", "media_violenza", "molta_violenza"))
summary(dataset$prevalence_cat)

#Grafici - visualizzazione 
table(dataset$prevalence_cat)
pie(table(dataset$prevalence_cat))
barplot(table(dataset$prevalence_cat), col="pink")

#5. Restricted asset
min(dataset$restricted_asset) #2.1
max(dataset$restricted_asset) #77.7

dataset$restricted_asset_cat <- cut(dataset$restricted_asset, breaks=c(0,20,40,80),
                                         labels = c("molto_accesso", "accesso_medio", "poco_accesso"))
summary(dataset$restricted_asset_cat)

#Poco accesso agli assets
poco_accesso <- dataset %>% filter(dataset$restricted_asset_cat == 'poco_accesso')
poco_accesso$LOCATION
count(poco_accesso) #22 paesi

#Medio accesso agli assets 
medio_accesso <- dataset %>% filter(dataset$restricted_asset_cat == 'accesso_medio')
medio_accesso$LOCATION
count(medio_accesso) #50 paesi

#Molto accesso agli assets
molto_accesso <- dataset %>% filter(dataset$restricted_asset_cat == 'molto_accesso')
molto_accesso$LOCATION
count(molto_accesso) #47 paesi 

#Grafici - visualizzazione 
table(dataset$restricted_asset_cat)
pie(table(dataset$restricted_asset_cat))
barplot(table(dataset$restricted_asset_cat), col="pink")

#6. Restricted liberties
min(dataset$restricted_liberties) #3.6
max(dataset$restricted_liberties) #75.8

dataset$restricted_liberties_categorica <- cut(dataset$restricted_liberties, breaks = c(0,20,40,76),
                           labels = c('bassa', 'media', 'alta'))
summary(dataset$restricted_liberties_categorica)

#Vediamo quali paesi hanno poche restrizioni sui diritti civili delle donne
library(tidyverse)
low <- dataset %>% filter(dataset$restricted_liberties_categorica == 'bassa')
low$LOCATION 
count(low) #42 paesi hanno poche restrizioni

#Paesi con poche restrizioni 
medium <- dataset %>% filter(dataset$restricted_liberties_categorica == 'media')
medium$LOCATION 
count(medium) #48 

#Paesi con tante restrizioni
high <- dataset %>% filter(dataset$restricted_liberties_categorica== 'alta')
high$LOCATION 
count(high) #29 Paesi hanno alte restrizioni sui diritti civili delle donne

#Grafici - visualizzazione 
table(dataset$restricted_liberties_categorica)
pie(table(dataset$restricted_liberties_categorica))
barplot(table(dataset$restricted_liberties_categorica), col="pink")

#7. Restricted physical 

min(dataset$restricted_physical) #4.2
max(dataset$restricted_physical) #56.9

dataset$restricted_physical_cat <- cut(dataset$restricted_physical, breaks = c(0,15,30,60),
                                               labels = c('no_restrizione', 'media_restrizione', 'alta_restrizione'))
summary(dataset$restricted_physical_cat)

#Grafici - visualizzazione
table(dataset$restricted_physical_cat)
pie(table(dataset$restricted_physical_cat))
barplot(table(dataset$restricted_physical_cat), col="pink")

#8. Women in parliament 

min(dataset$women_parliament) #0
max(dataset$women_parliament) #61.3

dataset$women_parl_cat <- cut(dataset$women_parliament, breaks = c(-1,30,50,70),
                                       labels = c('poca_presenza', 'media_presenza', 'alta_presenza'))
summary(dataset$women_parl_cat)

#Grafici - visualizzazione
table(dataset$women_parl_cat)
pie(table(dataset$women_parl_cat))
barplot(table(dataset$women_parl_cat), col="pink")


############### CORRELATION MATRIX ###############
#Usiamo le variabili numeriche 
#Importiamo la libreria che ci serve
library(Hmisc)
library(corrplot)
dataset.rcorr = rcorr(as.matrix(dataset[,c(2:9)]))
dataset.rcorr

dataset.cor = cor(dataset[,c(2:9)], method = c("spearman"))

corrplot(dataset.cor)

#Ora usiamo le variabili che risultano essere più correlate
#dalla matrice di correlazione e facciamo le tabelle di contingenza
#poi guardiamo anche al chi-quadro 

######## CONTINGENCY TABLES ########

# TAB_1: ATTITUDE_VIOLENCE_CAT + DISCRIM_FAMILY_CAT

tab_1 <- table(dataset$attitude_viol_cat, dataset$discrim_family_cat)
tab_1

chisq.test(tab_1) #non rigetto indipendenza, quindi c'è associazione 

round(prop.table(tab_1, 1), 2) #percentuali per riga
round(prop.table(tab_1, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_1, 1), 2)*100 
round(prop.table(tab_1, 2),2)*100


# TAB_2: ATTITUDE_VIOLENCE_CAT + RESTRICTED_ASSET_CAT 
tab_2 <- table(dataset$attitude_viol_cat, dataset$restricted_asset_cat)
tab_2

chisq.test(tab_2) #non rigetto indipendenza, quindi c'è associazione 

round(prop.table(tab_2, 1), 2) #percentuali per riga
round(prop.table(tab_2, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_2, 1), 2)*100 
round(prop.table(tab_2, 2),2)*100

# TAB_3: ATTITUDE_VIOLENCE_CAT + RESTRICTED_PHYSICAL_CAT 
tab_3 <- table(dataset$attitude_viol_cat, dataset$restricted_physical_cat)
tab_3

chisq.test(tab_3) #non rigetto indipendenza, quindi c'è associazione 

round(prop.table(tab_3, 1), 2) #percentuali per riga
round(prop.table(tab_3, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_3, 1), 2)*100 
round(prop.table(tab_3, 2),2)*100

# TAB_4: LAW_DOMESTIC_VIOLENCE_CAT + RESTRICTED_PHYSICAL_CAT
tab_4 <- table(dataset$law_domestic_violence_cat, dataset$restricted_physical_cat)
tab_4

chisq.test(tab_4) #non rigetto indipendenza, quindi c'è associazione 

round(prop.table(tab_4, 1), 2) #percentuali per riga
round(prop.table(tab_4, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_4, 1), 2)*100 
round(prop.table(tab_4, 2),2)*100

#TAB_5: RESTRICTED_ASSET_CAT + WOMEN_PARL_CAT
tab_5 <- table(dataset$restricted_asset_cat, dataset$women_parl_cat)
tab_5

chisq.test(tab_5) #non rigetto indipendenza, quindi c'è associazione 

round(prop.table(tab_5, 1), 2) #percentuali per riga
round(prop.table(tab_5, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_5, 1), 2)*100 
round(prop.table(tab_5, 2),2)*100

#TAB_6: RESTRICTED LIBERTIES +  WOMEN IN PARLIAMENT  
tab_6 <- table(dataset$restricted_liberties_categorica, dataset$women_parl_cat)
tab_6

chisq.test(tab_6) #non rigetto indipendenza, quindi c'è associazione 

round(prop.table(tab_6, 1), 2) #percentuali per riga
round(prop.table(tab_6, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_6, 1), 2)*100 
round(prop.table(tab_6, 2),2)*100

#TAB_7: RESTRICTED_PHYSICAL_CAT + WOMEN_PARL_CAT
tab_7 <- table(dataset$restricted_physical_cat, dataset$women_parl_cat)
tab_7

chisq.test(tab_7) # rigetto indipendenza, quindi non c'è associazione 

round(prop.table(tab_7, 1), 2) #percentuali per riga
round(prop.table(tab_7, 2),2) #percentuali per colonna

#*100 
round(prop.table(tab_7, 1), 2)*100 
round(prop.table(tab_7, 2),2)*100

###### SCATTER PLOT + RETTA INTERPOLANTE #### 
#Adesso prendiamo le variabili che hanno dimostrato avere associazione 
#prendiamo il loro valore numerico e guardiamo con una retta se sono associate 

#1. attitude_viol & discrim_family
library(ggplot2)
ggplot(dataset, aes(x=attitude_viol, y=discrim_family))+
  geom_point() + 
  geom_smooth(method='lm')

library(plotly)

fig1 <- plot_ly(data = dataset, x = ~attitude_viol, y = ~discrim_family,
               marker = list(size = 10,
                             color = 'rgba(255, 182, 193, .9)',
                             line = list(color = 'rgba(152, 0, 0, .8)',
                                         width = 2)))
fig1 <- fig1 %>% layout(title = 'Discriminazione e attitudine alla violenza',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))

fig1
#2. attitude_viol + restricted_asset
ggplot(dataset, aes(x=attitude_viol, y=restricted_asset))+
  geom_point() + 
  geom_smooth(method='lm')
fig2 <- plot_ly(data = dataset, x = ~attitude_viol, y = ~restricted_asset,
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)))
fig2 <- fig2 %>% layout(title = 'Attitudine alla violenza e poco accesso agli asset',
                        yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))

fig2
#3. attitude_viol + restricted_physical 
ggplot(dataset, aes(x=attitude_viol, y=restricted_physical))+
  geom_point() + 
  geom_smooth(method='lm')
fig3 <- plot_ly(data = dataset, x = ~attitude_viol, y = ~restricted_physical,
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)))
fig3 <- fig3 %>% layout(title = 'Attitudine alla violenza e libertà sul proprio corpo',
                        yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))

fig3


#5. restricted_asset + women_parl 
ggplot(dataset, aes(x=women_parliament, y=restricted_asset))+
  geom_point() + 
  geom_smooth(method='lm')
fig4 <- plot_ly(data = dataset, x = ~women_parliament, y = ~restricted_asset,
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)))
fig4 <- fig4 %>% layout(title = 'Donne in parlamento e poco accesso agli asset',
                        yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))

fig4
#6. restricted_liberties + women_parl 
ggplot(dataset, aes(x=women_parliament, y=restricted_liberties))+
  geom_point() + 
  geom_smooth(method='lm')
fig5 <- plot_ly(data = dataset, x = ~women_parliament, y = ~restricted_liberties,
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)))
fig5 <- fig5 %>% layout(title = 'Donne in parlamento e poche libertà individuali',
                        yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))

fig5
#7. restricted_physical + women_parl 
ggplot(dataset, aes(x=women_parliament, y=restricted_physical))+
  geom_point() + 
  geom_smooth(method='lm')


fig6 <- plot_ly(data = dataset, x = ~women_parliament, y = ~restricted_physical,
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)))
fig6 <- fig6 %>% layout(title = 'Donne in parlamento e libertà sul proprio corpo',
                        yaxis = list(zeroline = FALSE),
                        xaxis = list(zeroline = FALSE))

fig6

