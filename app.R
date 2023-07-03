library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(geobr)
library(leaflet)

df_br = read_xlsx('data/brasil.xlsx')
df_macro = read_xlsx('data/macroregiao.xlsx')
df_muni = read_xlsx('data/municipio.xlsx')
df_reg = read_xlsx('data/regioes.xlsx')
df_uf = read_xlsx('data/uf.xlsx')

mapa = read_country(year= 2020)


# Interface Gráfica
ui <- fluidPage(
  titlePanel("Porcentagem de Coberturas de Estados"), #Nome da Pagina
  sidebarLayout(
    sidebarPanel(
      # Selecionar o Ano
      selectInput(inputId = "Ano",
                  label = "Selecione o Ano:",
                  choices = levels(factor(df_uf$ano)),
                  selected = "2021"),
      # Selecionar a variável
      selectInput(inputId = "Variavel",
                  label = "Selecione a Variável:",
                  choices = c("Atenção Básica" = 'cob_ab',
                              "Agentes Comunitários de Saúde" = "cob_acs",
                              "Estratégia de Saúde da Família"	 = "cob_esf",   
                              "Planos de Saúde"	 = "cob_priv",    
                              "Vacinal de BCG"	 = "cob_vac_bcg",   
                              'Vacinal de Hepatite A'	 = "cob_vac_hepa",	
                              "Vacinal de Hepatite B em crianças até 30 dias" = "cob_vac_hepb",	
                              'Vacinal de Meningococo C' = "cob_vac_menin"
                              ),
                  selected = "Porcentagem de Cobertura da Atenção Básica"),
    ),
    mainPanel(
      leafletOutput("mapa_coberturas") #Nome do grafico
    )
  )
)

# Funções
server <- function(input, output) {
  # Grafico **TESTE**
  output$mapa_coberturas <- renderLeaflet({
    # Filtragem dos dados
    df_mapa = inner_join(mapa, df_uf, c('abbrev_state' = 'sigla_uf')) %>% 
      filter(ano == input$Ano)
    # Criação do Gráfico
    #ggplot(df_mapa) +
    #  geom_sf(aes(fill = .data[[input$Variavel]]))
    pal <-  colorBin("Blues", domain = log(df_mapa[[input$Variavel]]), bins = 5)
    leaflet(data = df_mapa) %>%
      addPolygons(fillColor = ~ pal(log(df_mapa[[input$Variavel]])), 
                  fillOpacity = 0.9, 
                  color = "white", 
                  weight = 1)
  })
}

shinyApp(ui = ui, server = server)
#'''