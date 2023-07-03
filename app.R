library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(geobr)
library(leaflet)

# Datasets
df_br = read_xlsx('data/brasil.xlsx')
df_macro = read_xlsx('data/macroregiao.xlsx')
df_muni = read_xlsx('data/municipio.xlsx')
df_reg = read_xlsx('data/regioes.xlsx')
df_uf = read_xlsx('data/uf.xlsx')

# Mapa do Brasil
mapa = read_country(year= 2020)


# Interface Gráfica
ui <- fluidPage(
  navbarPage("IEPS DATA", # Nome do Aplicativo Shiny
             navbarMenu("Coberturas", # Nome do menu
                        tabPanel("Cobertura1", fluid= T, # Nome do item
                                 titlePanel("Porcentagem de Coberturas por Estados"),
                                 sidebarLayout(
                                   sidebarPanel(
                                    # Selecionar o Ano
                                    selectInput(inputId = 'Ano',
                                                label= 'Selecione o Ano:',
                                                choices= levels(factor(df_uf$ano)),
                                                selected= '2021'),
                                    # Selecionar a variável
                                    selectInput(inputId = "Variavel",
                                                label = "Selecione a Variável:",
                                                choices = c("Atenção Básica" = 'cob_ab',
                                                            "Agentes Comunitários de Saúde" = "cob_acs",
                                                            "Estratégia de Saúde da Família"	 = "cob_esf",   
                                                            "Planos de Saúde"	 = "cob_priv"),
                                                selected = "Atenção Básica")),
                                   mainPanel(
                                     leafletOutput("mapa_coberturas") #Nome do gráfico
                                     ))),
                        tabPanel("Vacinal", fluid= T,
                                 titlePanel("ESCREVER"),
                                 sidebarLayout(
                                   sidebarPanel(),
                                   mainPanel(
                                     plotOutput("tipos_linha")
                                     ))),
                        ),
             navbarMenu("Despesas",
                        tabPanel("Despesas"),
                        tabPanel("Gastos"),
                        tabPanel("Participacao")),
             navbarMenu("Infraestrutura",
                        tabPanel("Teste1"),
                        tabPanel("Teste2")),
             tabPanel("Demografia")))
  
# Funções
server <- function(input, output) {
  # Grafico Coberturas
  output$mapa_coberturas <- renderLeaflet({
    # Filtragem dos dados
    df_mapa = inner_join(mapa, df_uf, c('abbrev_state' = 'sigla_uf')) %>% 
      filter(ano == input$Ano)
    # Criação do Gráfico
    pal <-  colorBin("Blues", domain = df_mapa[[input$Variavel]], bins = 5)
    leaflet(data = df_mapa) %>%
      addPolygons(fillColor = ~ pal(df_mapa[[input$Variavel]]), 
                  fillOpacity = 0.9, 
                  color = "white", 
                  weight = 1,
                  popup = paste("Cobertura: ", round(df_mapa[[input$Variavel]], 2), "%")) %>% 
      addLegend("bottomright", pal= pal, values = df_mapa[[input$Variavel]], title = "Porcentagem da Cobertura", opacity = 1)
  })
  
  # Grafico
  # Outro
  # Outro
}

shinyApp(ui = ui, server = server)