library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(geobr)
library(leaflet)
library(plotly)

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
             tabPanel("Infraestrutura",
                      fluid = T,
                      titlePanel("Graficos sobre dados Hospitalares"),
                      sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = 'variavel_hospital',
                                label = 'Selecione a variável',
                                choices = c(
                                  'Número de Médicos' = 'n_med',
                                  'Número de Enfermeiros' = 'n_enf',
                                  'Número de Hospitalizações Totais' = 'n_hosp',
                                  'Número de Leitos Não-SUS' = 'n_leito_nsus',
                                  'Número de Leitos SUS' = 'n_leito_sus',
                                  'Número de Leitos de UTI Não-SUS' = 'n_leitouti_nsus',
                                  'Número de Leitos de UTI SUS' = 'n_leitouti_sus'
                                )
                              )
                            ),
                            mainPanel(
                              plotlyOutput("grafico_hospital")
                            )
                          )
                        ),
             navbarMenu('Demografia',
                        tabPanel(
                          "Mortalidade",
                          fluid = T,
                          titlePanel("Graficos sobre mortalidade"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = 'variavel_mor',
                                label = 'Selecione a variável',
                                choices = c(
                                  'Mortalidade Bruta por Causas Mal Definidas' = 'pct_mort_maldef',
                                  'Mortalidade Bruta' = 'tx_mort',
                                  'Mortalidade Bruta por Condições Sensíveis a Atenção Primária' = 'tx_mort_csap',
                                  'Mortalidade Bruta por Causas Evitáveis' = 'tx_mort_evit',
                                  'Mortalidade Infantil' = 'tx_mort_inf_ibge'
                                )
                              )
                            ),
                            mainPanel(
                              plotlyOutput("grafico_mor")
                            )
                          )
                        ),
                        tabPanel(
                          "Natalidade",
                          fluid = T,
                          titlePanel("Graficos sobre natalidade"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = 'variavel_nat',
                                label = 'Selecione a variável',
                                choices = c(
                                  'Porcentagem de Nascidos Vivos com 1 a 6 Consultas Pré-Natal' = 'pct_prenatal_1a6',
                                  'Porcentagem de Nascidos Vivos com 7 ou Mais Consultas Pré-Natal' = 'pct_prenatal_7m',
                                  'Porcentagem de Nascidos Vivos com Pré-Natal Adequado' = 'pct_prenatal_adeq',
                                  'Porcentagem de Nascidos Vivos com Nenhuma Consulta Pré-Natal' = 'pct_prenatal_zero'
                                )
                              )
                            ),
                            mainPanel(
                              plotlyOutput("grafico_nat")
                            )
                          )
                        )
             )))
  
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
  
  # Grafico Mortalidade
  output$grafico_mor <- renderPlotly({
    a = ggplot(data = df_br, aes(x = ano, y = !!sym(input$variavel_mor))) +
      geom_line(col= "#5499C7") + geom_point(col= "#1F618D") +
      labs(x= "Anos")
    
    ggplotly(a)
  })
  
  # Gráfico Hospitalares
  output$grafico_hospital <- renderPlotly({
    b = ggplot(data = df_br, aes(x = ano, y = !!sym(input$variavel_hospital))) +
      geom_line(col= "#5499C7") + geom_point(col= "#1F618D") +
      labs(x= "Anos")
    
    ggplotly(b)
  })
  
  # Gráfico Natalidade
  output$grafico_nat <- renderPlotly({
    c = ggplot(data = df_br, aes(x = ano, y = !!sym(input$variavel_nat))) +
      geom_line(col= "#5499C7") + geom_point(col= "#1F618D") +
      labs(x= "Anos")
    
    ggplotly(c)
  })
  
}

shinyApp(ui = ui, server = server)