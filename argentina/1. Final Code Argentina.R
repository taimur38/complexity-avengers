
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(ggtext)
library(writexl)
library(stringr)
library(reshape2)
library(readr)
library(readxl)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tidyverse)
library(mapview)
library(economiccomplexity)
library(RColorBrewer)
library(treemap)
library(janitor)
mapviewOptions(fgb = F)
library(igraph)
library(ggraph)


rm(list=ls())

library(showtext)
font_add(family = "Arial", regular = "Arial.ttf") ## here is the path to the font to add.
showtext.auto()

# Theme for graphs (best)
theme <- theme_set(theme_minimal())

################################################

# Loading main dataset
df <- read_csv("establecimientos_departamento.csv")

# Loading highways dataset
# rutas <- st_read("_3_4_1_1_6_rutas_nacionales_dnv18_view.shp")

# We create CLAE3 using CLAE6
df <- df %>%
  mutate(length=nchar(clae6),
         clae3=ifelse(length==5, substr(clae6, 1,2), substr(clae6, 1,3)),
         clae3=as.numeric(clae3))

# We charge CLAE description
# CLAE Description
clae_desc <- read_csv("clae_desc.csv")

# CLAE 2
clae_2_desc <- clae_desc %>%
  select(clae2, clae2_desc)
clae_2_desc <- clae_2_desc %>%
  distinct()

df <- left_join(df, clae_2_desc, by="clae2")

# CLAE 3
clae_3_desc <- clae_desc %>%
  select(clae3, clae3_desc)
clae_3_desc <- clae_3_desc %>%
  distinct()

df <- left_join(df, clae_3_desc, by="clae3")

# CLAE 6
clae_6_desc <- clae_desc %>%
  select(clae6, clae6_desc)
clae_6_desc <- clae_6_desc %>%
  distinct()

df <- left_join(df, clae_6_desc, by="clae6")


# Population Dataset
population <- read_excel("poblacion.xlsx")

df <- left_join(df, population, by="in_departamentos")

# Regions Provincia
df <- df %>%
  mutate(region=case_when(provincia_id%in%c(94, 78, 26, 62, 58) ~ "Southern",
                          provincia_id%in%c(2, 6, 14, 30, 42, 50, 70, 74, 82) ~ "Central",
                          TRUE ~ "Northern"))

###########################################################################

# Descriptive Data at the Region and Province Level

# Establecimientos and Empleo by Province

province_df <- df %>%
  group_by(provincia, provincia_id, region) %>%
  summarise(firms=sum(Establecimientos, na.rm=TRUE),
            employment=sum(Empleo, na.rm=TRUE),
            activities=n_distinct(clae3))

province_stats <- read_excel("Tables.xlsx")

province_df <- left_join(province_df, province_stats, by=c("provincia_id"="Province ID"))

treemap(province_df, index="provincia", vSize="firms.x", palette = "Set2", show.labels=T)

###########################################################################
### ECI and PCI
df_2 <- df
df_2 <- na.omit(df_2)
df_firms <- df_2  %>%
  group_by(in_departamentos, clae3, provincia, clae2) %>%
  summarise(n_firms_department_activity=sum(Empleo, na.rm=TRUE))

# Using econcomplexity package to estimate ECI
dft <- df_firms

dft <- dft %>%
  rename(country = in_departamentos,
         product = clae3,
         value = n_firms_department_activity)

bi <- balassa_index(dft)

bi[1:10,1:40]

# Diversity and Avg Ubiquity
bi_matrix <- data.frame(as.matrix(bi))

bi_matrix_tai <-  bi_matrix %>%
  mutate(in_departamentos=row.names(.)) %>%
  pivot_longer(!in_departamentos, names_to="product", values_to="rca") %>%
  mutate(
    product=ifelse(str_detect(product, "^X"),
                   str_sub(product, start=2),
                   product)
  )

bi_matrix_tai <- bi_matrix_tai %>%
  group_by(product) %>%
  mutate(u=sum(rca),
         a=u*rca,
         ubiquity=case_when(a>0 ~ a))
         

nestedness <- bi_matrix_tai %>%
  group_by(in_departamentos) %>%
  summarise(diversity=sum(rca, na.rm=T),
         avg_ubiquity=mean(ubiquity, na.rm=TRUE))
  
nestedness %>%
  ggplot(aes(x=diversity, y=avg_ubiquity)) +
  geom_point() +
  labs(x="Diversity (number of activities made by a department)",
       y="Avg Ubiquity",
       color="")  +
  theme(axis.title.x = element_text(color="black", size=18), 
        axis.title.y = element_text(color="black", size=18), 
        axis.text.x = element_text(color="black", size=12), 
        axis.text.y = element_text(color="black", size=12),
        legend.text=element_text(color="black", size=14),
        legend.position="bottom") +
  geom_hline(yintercept=116, linetype="dashed", color = "black")+
  geom_vline(xintercept=30.59, linetype="dashed", color = "black")

mean(nestedness$avg_ubiquity)


# calculate eci / pci, using reflections here: same values as py-ecomplexity package
cm <- complexity_measures(bi,method='eigenvalues')

# convert to tibble, add country names, sort from most to least complex
# -- xci labels are set with setNames (extract with 'names')
df_eci <- cm$complexity_index_country %>%
  as_tibble() %>%
  mutate(country = names(cm$complexity_index_country)) %>%
  rename(eci = value)

#df_eci$country <- as.numeric(df_eci$country)

# same procedure for products (pci)
df_pci <- cm$complexity_index_product %>%
  as_tibble() %>%
  mutate(product = names(cm$complexity_index_product)) %>%
  rename(pci= value)

df_pci$product <- as.numeric(df_pci$product)
df_pci <- left_join(df_pci,clae_3_desc,by=c('product'='clae3'))

write_csv(df_pci, "pci_clae.csv")

departamentos <- st_read("departamentos.shp")
departamentos <- departamentos %>%
  filter(IN1!=94028 & IN1!=94021) %>%
  mutate(in_departamentos=as.numeric(IN1))

df_eci$country <- as.numeric(df_eci$country)

departamentos <- left_join(departamentos, df_eci, by=c('in_departamentos'='country'))
mapview(departamentos, zcol="eci",col.regions=brewer.pal(9, "BuGn"), grid=FALSE)


mapview(departamentos)
# Comunas


aglo <- departamentos

df_departamentos <- df_2 %>%
  group_by(in_departamentos, provincia) %>%
  summarise(mean=provincia_id) %>%
  unique()
  
  
aglo <- left_join(aglo, df_departamentos, by=c('in_departamentos'='in_departamentos'))

aglo <- aglo %>%
  mutate(region=case_when(mean%in%c(94, 78, 26, 62, 58) ~ "Southern",
                          mean%in%c(2, 6, 14, 30, 42, 50, 70, 74, 82) ~ "Central",
                          TRUE ~ "Northern"))

aglo <- aglo %>%
  mutate(commuting=case_when(in_departamentos%in%c(6028,6035,6091,6260,6270,6274,6364,6371,6408,6410,6412,6427,6441,6434,6490,6515,6525,6539,6560,6568,6638,6648,6658,6749,6756,6778,6805,6840,6861,2007,2070,2077,2084,2091,2098,2105,2014,2021,2028,2035,2042,2049,2056,2063,6252,6760) ~ "AMBA",
                             in_departamentos%in%c(14014,14021,14091,14105,14119,14147) ~ "Gran Cordoba",
                             in_departamentos%in%c(50007,50021,50028,50049,50063,50070) ~ "Gran Mendoza",
                             in_departamentos%in%c(82084,82119) ~ "Gran Rosario",
                             in_departamentos%in%c(90084,90014,90063,90105,90119) ~ "Gran Tucuman",
                             in_departamentos%in%c(6056) ~ "Bahia Blanca",
                             in_departamentos%in%c(6357) ~ "Mar del Plata",
                             in_departamentos%in%c(66028) ~ "Salta",
                             in_departamentos%in%c(82063) ~ "Santa Fe",
                             in_departamentos%in%c(18021) ~ "Corrientes",
                             in_departamentos%in%c(58035) ~ "Neuquen",
                             TRUE ~ region))

aglo <- aglo %>%
  group_by(commuting) %>%
  summarize(geometry=st_union(geometry),
            eci=mean(eci, na.rm=TRUE))

mapview(aglo, zcol="eci", col.regions=brewer.pal(9, "BuGn"))


rutas <- st_read("_3_4_1_1_6_rutas_nacionales_dnv18_view.shp")
mapview(rutas)

# Proximity
# For deparments
pro <- proximity(bi)
net <- projections(pro$proximity_country, pro$proximity_product)

aggregated_countries <- aggregate(
  dft$value,
  by = list(country = dft$country),
  FUN = sum
)

aggregated_countries <- setNames(aggregated_countries$x, aggregated_countries$country)

V(net$network_country)$size <- aggregated_countries[match(V(net$network_country)$name, names(aggregated_countries))]

ggraph(net$network_country, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "#86494d") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2,  check_overlap = TRUE) +
  ggtitle("Proximity Based Network Projection for Departments")

# For products
aggregated_products <- aggregate(
  dft$value,
  by = list(country = dft$product),
  FUN = sum
)

aggregated_products <- setNames(aggregated_products$x, aggregated_products$country)

V(net$network_product)$size <- aggregated_products[match(V(net$network_product)$name, names(aggregated_products))]

ggraph(net$network_product, layout = "kk") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(), color = "#86494d") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2, check_overlap = T) +
  ggtitle("Proximity Based Network Projection for Products") 



##################################################

# Complexity and Exports

# Value Exports

firms_export_fob <- read_csv("total_expo_total_empresas_por_clae3.csv")

firms_export_fob <- firms_export_fob %>%
  mutate(total_fob=ifelse(total_fob==-99,0,total_fob),
         year=substr(fecha,1,4))


firms_fob_clae3 <- firms_export_fob %>%
  group_by(year, clae3) %>%
  summarise(fob=sum(total_fob))

firms_fob_2021 <- firms_export_fob %>%
  filter(year==2021) %>%
  group_by(clae3) %>%
  summarise(fob=sum(total_fob))

df_pci_export_fob <- df_firms 

df_pci_export_fob <- df_pci_export_fob %>%
  group_by(clae3) %>%
  summarise(total_n=sum(n_firms_department_activity))

df_pci_export_fob <- left_join(df_pci_export_fob, firms_fob_2021, by="clae3")
df_pci_export_fob <- left_join(df_pci_export_fob, df_pci, by=c("clae3"="product"))
df_pci_export_fob <- left_join(df_pci_export_fob, clae_desc, by="clae3")
df_pci_export_fob <- df_pci_export_fob %>%
  mutate(sector=case_when(letra%in%c("A")~"Agriculture",
                          letra%in%c("B")~"Minerals Extraction",
                          letra%in%c("C") ~"Industry",
                          letra%in%c("G") ~ "Trade",
                          TRUE~"Other Services"))
df_pci_export_fob %>%
  filter(fob>0) %>%
  ggplot(aes(x=pci, y=log(fob+1), color=sector, group=sector)) +
  geom_point() +
  labs(x="Activity PCI",
       y="Exports 2021 (log)",
       color="") +
  theme(axis.title.x = element_text(color="black", size=18), 
        axis.title.y = element_text(color="black", size=18), 
        axis.text.x = element_text(color="black", size=12), 
        axis.text.y = element_text(color="black", size=12),
        legend.text=element_text(color="black", size=14),
        legend.position="bottom") 

# Exports
firms_export_3 <- read_csv("empresas_expo_privada_por_clae3.csv")

firms_export_3 <- firms_export_3 %>%
  mutate(empresas=ifelse(empresas==-99,0,empresas),
         year=substr(fecha,1,4))

firms_export_year <- firms_export_3 %>%
  group_by(year, clae3) %>%
  summarise(empresas=mean(empresas))

firms_export_total <- firms_export_year %>%
  filter(year>2017) %>%
  group_by(clae3) %>%
  summarise(empresas=mean(empresas))

df_pci_export <- df_firms 

df_pci_export <- df_pci_export %>%
  group_by(clae3) %>%
  summarise(total_n=sum(n_firms_department_activity))

df_pci_export <- left_join(df_pci_export, firms_export_total, by="clae3")
df_pci_export <- left_join(df_pci_export, df_pci, by=c("clae3"="product"))
df_pci_export <- left_join(df_pci_export, clae_desc, by="clae3")
df_pci_export <- df_pci_export %>%
  mutate(sector=case_when(letra%in%c("A")~"Agriculture",
                          letra%in%c("B")~"Minerals Extraction",
                          letra%in%c("C") ~"Industry",
                          letra%in%c("G") ~ "Trade",
                          TRUE~"Other Services"))


df_pci_export <- df_pci_export %>%
  mutate(export_intensity=empresas/total_n*100)


df_pci_export %>%
  filter(export_intensity<100 & export_intensity>0) %>%
  ggplot(aes(x=pci, y=export_intensity, color=sector, group=sector)) +
  geom_point() +
  labs(x="Activity PCI",
       y="Share of Firms Exporting by CLAE",
       color="") +
  theme(axis.title.x = element_text(color="black", size=18), 
        axis.title.y = element_text(color="black", size=18), 
        axis.text.x = element_text(color="black", size=12), 
        axis.text.y = element_text(color="black", size=12),
        legend.text=element_text(color="black", size=14),
        legend.position="bottom") 







