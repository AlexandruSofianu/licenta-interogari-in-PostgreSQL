###########################################################################################
###                          Licenta - Sofianu ...
###########################################################################################

library(tidyverse)
library(readxl)
library(janitor)
library(corrplot)
library(corrgram)
library(scales)
library(patchwork)
library(viridis)
library(ggsci)
#install.packages('svglite')
library(svglite)
# https://corrr.tidymodels.org/reference/correlate.html
library(corrr)
# install.packages('ggstatsplot')
library(ggstatsplot)

options(scipen = 999)  # renuntam la notatie stiitifica (cu exponent)


###########################################################################################
###                           I. Pregatire date
###########################################################################################

setwd('/Users/marinfotache/Library/CloudStorage/OneDrive-Personal/Lucrari_Diploma_Disertatie/Diploma/Diploma-2025/SofianuLiviu/TEST1 JMETER')

rez1 <- read_csv('rezultatesf1.csv') |>
    transmute(interogare_nr = label, durata_sec = elapsed, finalizare = if_else(success, 'succes', 'abandon')) |>
    mutate(marime_bd = 1)

glimpse(rez1)

rez10 <- read_csv('rezultatesf10.csv') |>
    transmute(interogare_nr = label, durata_sec = elapsed, finalizare = if_else(success, 'succes', 'abandon')) |>
    mutate(marime_bd = 10)

rez100 <- read_csv('rezultatesf100.csv') |>
    transmute(interogare_nr = label, durata_sec = elapsed, finalizare = if_else(success, 'succes', 'abandon')) |>
    mutate(marime_bd = 100)

rezultate <- bind_rows(rez1, rez10, rez100) |>
     filter(finalizare == 'succes') |>
     mutate(durata_sec = durata_sec / 1000)
table(rezultate$finalizare)


interogari_init <- read_csv('interogari jmeter TEST1.csv') |>
     mutate(query = str_replace_all(query, '\\n|\\r', ' ')) |>
     mutate(interogare_nr = paste0('Q', substring(row_number() + 1000, 2, 4))) |>
     mutate(nr_select = str_count(tolower(query),'(^| |\\))select ' )) |>
     mutate(nr_from = str_count(tolower(query),' from ' )) |>
     mutate(nr_join = str_count(tolower(query),' join ' )) |>
     mutate(nr_where = str_count(tolower(query),' where ' )) |>
     mutate(nr_group_by = str_count(tolower(query),' group by ' )) |>
     mutate(nr_having = str_count(tolower(query),' having ' )) |>
     mutate(nr_order_by = str_count(tolower(query),' order by ' )) |>
     mutate(nr_with = str_count(tolower(query),'(^| )with ' )) 


interogari <- interogari_init |>
     select(-query)

# select_uri <- interogari_init |>
#   transmute(interogare_nr, sectiuni_select = str_extract_all(query, 'SELECT .+ FROM')) |>
#   unnest(sectiuni_select)
# 
# select_uri$sectiuni_select[1]
# select_uri$sectiuni_select[2]

df <- rezultate |>
     inner_join(interogari) |>
     select(-interogare_nr, -finalizare)



###########################################################################################
###                           II. Analiza exploratorie
###########################################################################################


###########################################################################################
##                                 II.a Parametri interogari
df <- interogari |>
     select(-interogare_nr) |>
     mutate_if(is.numeric, as.factor)
glimpse(df)

# compute the frequencies for each categorical variables and values
eda_factors <- df %>%
#     mutate (marime_bd  = factor(marime_bd)) %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(df),2)) %>%
     arrange(variable, value) 

#View(eda_factors)

# plot only the factors with less than 20 distinct values 
g1 <- eda_factors %>%
     group_by(variable) %>%
     summarise(n_of_values = n()) %>%
     filter (n_of_values < 20) %>%    
     ungroup() %>%
     select (variable) %>%
     inner_join(eda_factors) %>%
ggplot(., aes(x = value, y = n_value, fill = value)) +
     geom_col() +
     geom_text (aes(label = paste0(round(percent,0), '%'), 
                  vjust = if_else(n_value > 20, 1.5, -0.5))) +
     facet_wrap(~ variable, scale = "free", nrow = 2) +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)) +
     theme(strip.text.x = element_text(size = 11)) +
     xlab("") + ylab("frequency") 

getwd()

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("01 parametri interogari.png", plot = x,  device = "png") 




###########################################################################################
##                                 II.b Marimea baze de date
df <- rezultate |>
     inner_join(interogari) |>
     select(-interogare_nr, -finalizare) |>
     select(marime_bd)

eda_factors <- df %>%
     mutate (marime_bd  = factor(marime_bd)) %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(df),2)) %>%
     arrange(variable, value) 

g1 <- eda_factors %>%
     group_by(variable) %>%
     summarise(n_of_values = n()) %>%
     filter (n_of_values < 20) %>%    
     ungroup() %>%
     select (variable) %>%
     inner_join(eda_factors) %>%
ggplot(., aes(x = value, y = n_value, fill = value)) +
     geom_col() +
     geom_text (aes(label = paste0(round(percent,0), '%'), 
                  vjust = if_else(n_value > 20, 1.5, -0.5))) +
     facet_wrap(~ variable, scale = "free", nrow = 3) +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)) +
     theme(strip.text.x = element_text(size = 11)) +
     xlab("") + ylab("frequency") 


x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("02 marime_bd.png", plot = x,  device = "png") 



###########################################################################################
##                                 II.c Durata interogarii
df <- rezultate |>
     inner_join(interogari) |>
     select(-interogare_nr, -finalizare) |>
     mutate(log10_durata = log10(durata_sec)) |>
     mutate(marime_bd = factor(marime_bd))


g1 <- ggplot(df, 
       aes(x = marime_bd, y = durata_sec)) +
     geom_boxplot() +
     theme(legend.position = "none") +
     scale_y_continuous(breaks = seq(0, 1800, by = 100))

g2 <- ggplot(df, 
       aes(x = marime_bd, y = log10_durata)) +
     geom_boxplot() +
     theme(legend.position = "none") +
     scale_y_continuous(breaks = seq(-4, 4, by = 0.5))

x <- g1 + g2 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("03 duration_sec si log10_durata.png", plot = x,  device = "png") 


###########################################################################################
###                           III. Statistica inferentiala
###########################################################################################

df <- rezultate |>
     inner_join(interogari) |>
     select(-interogare_nr, -finalizare) |>
     mutate(log10_durata = log10(durata_sec)) |>
     mutate(marime_bd = factor(marime_bd))

# testarea disributiei duratei 
# ipoteza numa a testului Shapiro-Wilk: distributia variabilei este normala
shapiro.test(df$durata_sec)
# p-value este sub 0.05, deci ipoteza nula se respinge


## Problema nr 1. Exista vreo asociere intre durata interogarii si marimea BD?
## 
ggbetweenstats(
     data = df, 
     x = marime_bd, 
     y = durata_sec,
     type = 'np')


## Problema nr 2. Exista vreo asociere intre durata interogarii si nr de jonctiuni?
## 
ggscatterstats(
     data = df, 
     x = nr_join, 
     y = durata_sec,
     type = 'np')




###########################################################################################
###                           IV. Model de regresie
###########################################################################################

df <- rezultate |>
     inner_join(interogari) |>
     select(-interogare_nr, -finalizare) |>
     mutate(log10_durata = log10(durata_sec)) |>
     mutate(marime_bd = factor(marime_bd))


lm1 <- lm (log10_durata ~ ., data = df |> select (-durata_sec))
summary(lm1)


