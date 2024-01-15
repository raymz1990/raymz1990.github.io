# Importando Bibliotecas
library(rvest)
library(dplyr)
library(data.table)
library(tidyverse)

# Importando arquivo base
d_paises <- fread("./dados/dCountry.csv")

# Criando nova tabela
dcountry <- select(d_paises, tconst)
dcountry$url <- paste0("https://www.imdb.com/title/", dcountry$tconst, "/")


# Criando a função 
extractCountry <- function(url) {
  page <- read_html(url)
  countries <- page %>%
    html_nodes(".ipc-metadata-list__item[data-testid='title-details-origin'] .ipc-inline-list__item a.ipc-metadata-list-item__list-content-item--link") %>%
    html_text() %>%
    paste(collapse = ",")
  return(countries)
}

# Invoca a função salva e carregar os dados
dcountry <- dcountry %>%
  rowwise() %>%
  mutate(country = extractCountry(url))

dcountry <- dcountry %>%
  mutate(country = strsplit(country, ",")) %>%
  unnest(country)



dcountry <- dcountry %>%
  group_by(country) %>%
  summarise(freq = n())

dcountry1 <- dcountry

dcountry$iso3 <- countrycode(sourcevar = dcountry$country,
                             origin = "country.name",
                             destination = "iso3c")



#------------------


g6_data <- dcountry


get_lat_long <- function(country_name) {
  world <- map_data("world")
  country_data <- world[world$region %in% country_name, c("region", "long", "lat")]
  return(country_data)
}

# Obtendo coordenadas de latitude e longitude
coordenadas <- lapply(dcountry$country, get_lat_long)
coordenadas_df <- do.call(rbind, coordenadas)
coordenadas_df$iso3 <- countrycode(sourcevar = coordenadas_df$region,
                             origin = "country.name",
                             destination = "iso3c")


library(stringr)

# Remove leading and trailing spaces from the joining column in both data frames
coordenadas_df$iso3 <- str_trim(coordenadas_df$iso3)
g6_data$iso3 <- str_trim(g6_data$iso3)

# Perform the left join again
g6_data <- left_join(coordenadas_df, g6_data, by = "iso3")
head(g6_data)

unique(g6_data$freq)
g6_data <- select()




#----
  # Carregue as bibliotecas
library(countrycode)
library(leaflet)

# Suponha que 'dcountry' seja seu dataframe com a coluna 'Country' contendo nomes dos países

# Converta os nomes dos países em códigos ISO de países
dcountry$iso3 <- countrycode(sourcevar = dcountry$country, origin = "country.name", destination = "iso3c")

# Crie um mapa básico
mapa <- leaflet(data = dcountry) %>%
  addTiles() %>% # Adicione o fundo do mapa
  addCircleMarkers(radius = ~10, # Tamanho das bolhas
                   color = "blue", 
                   fillOpacity = 0.7, 
                   popup = ~paste(country, "<br>Value:", freq)) # Adicione as bolhas com informações do país

# Visualize o mapa
mapa



#------
# Library
# Exemplo de dados
dados <- data.frame(
  pais = c("Brazil", "United States", "United Kingdom"),  # Nomes dos países
  valor = c(100, 200, 300)  # Exemplo de valores associados às bolhas
)

# Converta os nomes dos países em códigos ISO de países
dados$iso3 <- countrycode(sourcevar = dados$pais, origin = "country.name", destination = "iso3c")

# Carregue o pacote 'maps' para obter dados geográficos
library(maps)

# Obtenha dados geográficos para os países
dados_geograficos <- map_data("world")

# Mescle os dados geográficos com os dados dos países
dados_completos <- merge(dados_geograficos, 
                         dcountry, 
                         by.x = "region",
                         by.y = "iso3",
                         all.x = TRUE)

# Crie o bubble map usando ggplot2
ggplot(data = dados_completos, 
       aes(x = long, 
           y = lat, 
           size = freq,
           fill = freq)) +
  geom_polygon(color = "white") +  # Adicione contornos dos países
  geom_point(shape = 21) +  # Adicione bolhas
  scale_size_continuous(range = c(3, 20)) +  # Ajusta o intervalo de tamanho das bolhas
  scale_fill_viridis_c() +  # Esquema de cores para as bolhas
  theme_minimal() +
  labs(title = "Bubble Map", x = "Longitude", y = "Latitude")

