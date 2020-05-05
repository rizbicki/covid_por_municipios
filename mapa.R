library(geobr)
library(ggplot2)
library(sf)
library(dplyr)

st_transform(sfc, 'ESRI:54009')

#br <- read_municipality(year=2018)
br <- read_rds("br.RDS")

teste <- read_csv("bra_adm2/BRA_adm2.csv")
shapes <- read_sf("bra_adm2/BRA_adm2.shp")

### Mapa de espalhamento

```{r}
selectInput("state_map", strong("Estado:"), 
            sort(unique(dados_estados$state)),
            selected = c("SP"), 
            multiple = FALSE, selectize = TRUE)
```


Aumento percentual dos últimos 7 dias (apenas municípios com ao menos `r n_min_obitos` óbitos). Selecione a opção *óbito* ou *casos*
  no menu da esquerda para alterar a estatística.

```{r}
renderPlot({
  casos_ultimos_dias <- dados_cidade %>%
    group_by(state,city,ibgeID) %>% 
    filter(date%in%c(max(dados_cidade$date)-7,max(dados_cidade$date)))
  
  if(input$estat=="Óbitos")
  {
    casos_ultimos_dias <- casos_ultimos_dias %>%
      summarise(Aumento=(max(deaths)-min(deaths))/min(deaths))%>%
      filter(Aumento!=Inf) %>% 
      rename(Município=city)
  } else {
    casos_ultimos_dias <- casos_ultimos_dias %>%
      summarise(Aumento=(max(totalCases)-min(totalCases))/min(totalCases))%>%
      filter(Aumento!=Inf) %>% 
      rename(Município=city)
    
  }
  
  casos_ultimos_dias <- casos_ultimos_dias  %>%  
    filter(state==input$state_map)
  
  brs <-left_join(br %>%  filter(abbrev_state==input$state_map),
                  casos_ultimos_dias, 
                  by = c("code_muni" = "ibgeID"))%>% 
    rename(geometry=geom)
  
  
  brs$geometry <- brs$geometry %>%  
    st_cast("MULTIPOLYGON")
  
  brs$Crescimento <- round(100*brs$Aumento,1)
    
  g <- ggplot() +
    geom_sf(data = brs, aes(), color= "grey", size=.3,fill="white") +
    geom_sf(data = brs[!is.na(brs$Aumento),], 
            aes(fill=Aumento,key1=Município,key2=Crescimento),
            color= "grey", size=.5) +
    scale_fill_fermenter(palette = "OrRd",name="",
                         labels = percent,direction = 1) +
    theme_minimal() +
    no_axis
  print(g)
  #ggplotly(g,tooltip = c("key2","key1")) 
})
```
