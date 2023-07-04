library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(geobr)
library(leaflet)

#Despesas/Gastos/Porcentagem de part. - Todos
#desp_recp_saude_pc_uf	Despesa em Saúde Utilizando Recursos Próprios do Estado (por Hab., R$)
#

#Infraestrutura(Numeros) - Marcelo
#Demografia(Natalidade/Mortalidade) - Bruno


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
                        tabPanel("Assistêncial", fluid= T, # Nome do item
                                 titlePanel("Porcentagem de Coberturas Assistênciais por Estados"),
                                 sidebarLayout(
                                   sidebarPanel(
                                    # Selecionar o Ano
                                    selectInput(inputId = 'Ano1',
                                                label= 'Selecione o Ano:',
                                                choices= levels(factor(df_uf$ano)),
                                                selected= '2021'),
                                    # Selecionar a variável
                                    selectInput(inputId = "Variavel1",
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
                                 titlePanel("Porcentagem de Coberturas Vacinais por Estados"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     #Seleciona o Ano
                                     selectInput(inputId = 'Ano2',
                                                 label= 'Selecione o Ano:',
                                                 choices= levels(factor(df_uf$ano)),
                                                 selected= '2021'),
                                     # Selecionar a variável
                                     selectInput(inputId = "Variavel2",
                                                 label = "Selecione a Vacina:",
                                                 choices = c("BCG" = "cob_vac_bcg",
                                                             "Hepatite A"	= "cob_vac_hepa",
                                                             "Hepatite B em crianças até 30 dias" = "cob_vac_hepb",
                                                             "Meningococo C" = "cob_vac_menin",
                                                             "Pentavalente" = "cob_vac_penta",
                                                             "Pneumocócica" = "cob_vac_pneumo",
                                                             "Poliomielite" = "cob_vac_polio",
                                                             "Rotavírus Humano" = "cob_vac_rota",
                                                             "Tríplice Viral (1ª Dose)" = "cob_vac_tvd1"),
                                                 selected = "BCG")),
                                   mainPanel(
                                     leafletOutput("mapa_vacinal")
                                     )))),
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
    df_mapa = inner_join(mapa, df_uf, c('abbrev_state' = 'sigla_uf')) %>% 
      filter(ano == input$Ano1)

    pal <-  colorBin("Blues", domain = df_mapa[[input$Variavel1]], bins = 5)
    leaflet(data = df_mapa) %>%
      addPolygons(fillColor = ~ pal(df_mapa[[input$Variavel1]]), 
                  fillOpacity = 0.9, 
                  color = "white", 
                  weight = 1,
                  popup = paste("Cobertura: ", round(df_mapa[[input$Variavel1]], 2), "%")) %>% 
      addLegend("bottomright", pal= pal, values = df_mapa[[input$Variavel1]], title = "Porcentagem da Cobertura", opacity = 1)
  })
  
  # Grafico Vacinal
  output$mapa_vacinal <- renderLeaflet({
    df_mapa = inner_join(mapa, df_uf, c('abbrev_state' = 'sigla_uf')) %>% 
      filter(ano == input$Ano2)
    
    pal <-  colorBin("Blues", domain = df_mapa[[input$Variavel2]], bins = 5)
    leaflet(data = df_mapa) %>%
      addPolygons(fillColor = ~ pal(df_mapa[[input$Variavel2]]), 
                  fillOpacity = 0.9, 
                  color = "white", 
                  weight = 1,
                  popup = paste("Cobertura: ", round(df_mapa[[input$Variavel2]], 2), "%")) %>% 
      addLegend("bottomright", pal= pal, values = df_mapa[[input$Variavel2]], title = "Porcentagem da Cobertura", opacity = 1)
  })
  
  # Outro
  # Outro
}

shinyApp(ui = ui, server = server)