##tree map 
##get product categries 
library(readxl)
library(treemap)
#upload product codes 
product<- read_excel("product.xlsx")
product<- product%>%
  mutate(product=as.numeric(product))


##using the complexity wages file produced by Andres 
complexity_wg<- complexity_wg%>%
  mutate(product=as.numeric(product))

#merge complexity with products description 
complexity_wg<- left_join(complexity_wg,product,by="product")

##tree map 

complexity_wg$value<-as.numeric(complexity_wg$value)
##TREE Map for Colombia 
treemap(complexity_wg,
        index = c("Industry.y", "product"),
        vSize = "value",
        vColor = "Industry",
        type = "index",
        title = "Production Colombia 2022")%>%
  legend("topright", legend = industries, fill = colors)

##Tree Map for Bogota 

complexity_bog<- complexity_wg %>%
  filter(country=="Bogota")

treemap(complexity_bog,
        index = c("Industry.y", "product"),
        vSize = "value",
        vColor = "Industry",
        type = "index",
        title = "Production Bogota 2022")

legend("topright", legend = industries, fill = colors)

## Treemap for Amazonas 
complexity_am<- complexity_wg %>%
  filter(country=="Amazonas")

treemap(complexity_am,
        index = c("Industry.y", "product"),
        vSize = "value",
        vColor = "Industry.y",
        type = "index",
        title = "Production in Amazonas 2022")