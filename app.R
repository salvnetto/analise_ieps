library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(geobr)
library(leaflet)
library(plotly)
library(shinythemes)
library(DT)


# Datasets
df_br = read_xlsx('data/brasil.xlsx')
df_macro = read_xlsx('data/macroregiao.xlsx')
df_muni = read_xlsx('data/municipio.xlsx')
df_reg = read_xlsx('data/regioes.xlsx')
df_uf = read_xlsx('data/uf.xlsx')

# Mapa do Brasil
mapa = read_country(year= 2020)
mapa_muni = read_municipality(year = 2020)
new_muni = c(levels(factor(df_muni$nome)),"Todos")

# Escolhas dos Slides
labels_hops <- c(
  'n_med' = 'Número de Médicos',
  'n_enf' = 'Número de Enfermeiros',
  'n_hosp' = 'Número de Hospitalizações Totais',
  'n_leito_nsus' = 'Número de Leitos Não-SUS',
  'n_leito_sus' = 'Número de Leitos SUS',
  'n_leitouti_nsus' = 'Número de Leitos de UTI Não-SUS',
  'n_leitouti_sus' = 'Número de Leitos de UTI SUS'
)

labels_mort <- c(
  'pct_mort_maldef' = 'Mortalidade Bruta por Causas Mal Definidas',
  'tx_mort' = 'Mortalidade Bruta',
  'tx_mort_csap' = 'Mort. por Condições Sensíveis a Atenção Primária',
  'tx_mort_evit' = 'Mortalidade Bruta por Causas Evitáveis',
  'tx_mort_inf_ibge' = 'Mortalidade Infantil'
)

labels_nat <- c(
  'pct_prenatal_1a6' = '1 a 6 Consultas Pré-Natal',
  'pct_prenatal_7m' = '7 ou Mais Consultas Pré-Natal',
  'pct_prenatal_adeq' = 'Pré-Natal Adequado',
  'pct_prenatal_zero' = 'Nenhuma Consulta Pré-Natal'
)



# Interface Gráfica
ui <- fluidPage(theme = shinytheme("sandstone"),
  navbarPage("IEPS DATA", # Nome do Aplicativo Shiny
             
             #ABA INTRODUCAO
             tabPanel("Introdução", fluid= T,
                      titlePanel("Sobre o IEPS DATA"),
                      br(),
                      fluidRow(
                        column(tags$img(src="https://storage.googleapis.com/basedosdados-website-images/organization/6eb4d140-b6c0-4ada-ab2a-04fdbc791f00.png?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=storage-django%40basedosdados-dev.iam.gserviceaccount.com%2F20230706%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20230706T161534Z&X-Goog-Expires=604800&X-Goog-SignedHeaders=host&X-Goog-Signature=95b8a6a27bb4a50516dc0a85eb4c9e52f89af71039e1ac26f6c46c2a586ac148827854aa786b386a853103148673c10765b04aea8eea9f5e37ecb9c2bdcac1c14d5cf828c9b43e274b0e758e14b179aa94e53ad55dbc9f3379d36c2038d07d6fcf63e73501995072d959b602afa08c787efc47247ac2285711aa6aa577aad890d736ed53d755598dd4de1118cdf579222b41cf400890afac6bca3fba1f6c24d94bc592f0323a05f049ac1b4a3cf19364f64c1194efb2fbf32b5b0d5de7dd8e7a248f949ec9135786b353c1df853a7c34c7f1d56d2304404a26371ec29500bd58fdd116cd88f4282cd202275acece044ee458ffffabe331c4353b29d1154c0f92",width="206px",height="53px"),width=2),
                        column(
                          p("O IEPS Data é uma iniciativa do Instituto de Estudos para Políticas de Saúde (IEPS), uma organização sem fins lucrativos, independente e apartidária, cujo único objetivo é contribuir para o aprimoramento das políticas públicas do setor de saúde no Brasil. O IEPS defende a ideia de que toda a população brasileira deva ter acesso à saúde de qualidade e que o uso de recursos e a regulação do sistema de saúde sejam os mais efetivos possíveis. Acreditamos que a melhor maneira de alcançar o nosso propósito é através de políticas públicas baseadas em evidências, desenhadas, implementadas e monitoradas de maneira transparente."),
                          width = 10, style= "text-align:justify")),
                      br(),
                      fluidRow(
                        DT::dataTableOutput("descricao_variaveis"), width= 12),
                      br(),
                      fluidRow(
                        h2("Resultados"),
                        p("Por meio dos dados analisados é possível perceber que
                          o houve um grande aumento no número de leitos do sus,
                          a partir do ano de 2019, o que se deve ao início da pandemia
                          de coronavírus. Essa tendência também é observada para
                          os leitos não-sus. Sobre a cobertura vacinal, percebe-se
                          que há grande diferente entre os estados da região sudeste
                          e norte/nordeste tendo esses apresentado grande defasagem
                          nesse aspecto."))),
             
             
             # ABA COBERTURAS
             navbarMenu("Coberturas",
                        #ITEM ASSISTENCIAL
                        tabPanel("Assistêncial", fluid= T,
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
                        
                        #ITEM VACINAL
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
             
             
             
             
             # ABA INVESTIMENTOS
             navbarMenu("Investimentos", # Nome do menu
                        tabPanel("Despesas Municipais",fluid= T, # Nome do item
                                 titlePanel("Despesa Municipal Total"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     # Selecionar o Ano
                                     selectInput(inputId = 'Ano10',
                                                 label= 'Selecione o Ano:',
                                                 choices= levels(factor(df_uf$ano)),
                                                 selected= '2021'),
                                     # Seleciona as Cidades
                                     selectInput(inputId = 'Cidades',
                                                 label= 'Selecione a cidade:',
                                                 choices= new_muni,
                                                 selected = "Todos"
                                     ),
                                     # Selecionar a variável
                                     selectInput(inputId = "Variavel10",
                                                 label = "Selecione a Despesa:",
                                                 choices = c("Despesa Total com Saúde Sob Responsabilidade do Município" = 'desp_recp_saude_pc_mun',
                                                             "Despesa em Saúde Utilizando Recursos Próprios do Município" = "desp_tot_saude_pc_mun"
                                                 ),
                                                 selected = "Despesa Total com Saúde Sob Responsabilidade do Município")),
                                   mainPanel(
                                     leafletOutput("mapa_despesas") #Nome do gráfico
                                   ))),
                        tabPanel(
                          "Despesas Estaduais", # Nome do item
                          fluid = T,
                          titlePanel("Despesa Estadual Total"),
                          sidebarLayout(
                            sidebarPanel(
                              # Selecionar o Ano
                              selectInput(inputId = 'Ano11',
                                          label= 'Selecione o Ano:',
                                          choices= levels(factor(df_uf$ano)),
                                          selected= '2021'),
                              # Selecionar a variável
                              selectInput(
                                inputId = 'Variavel11',
                                label = 'Selecione a variável',
                                choices = c(
                                  "Despesa Total com Saúde Sob Responsabilidade do Estado" = 'desp_tot_saude_pc_uf_def',
                                  "Despesa em Saúde Utilizando Recursos Próprios do Estado" = "desp_recp_saude_pc_uf"
                                )
                              )
                            ),
                            mainPanel(
                              plotlyOutput("grafico_desp") #Nome do gráfico
                            )
                          )
                        ),
                        tabPanel("Infraestrutura",
                                 fluid = T,
                                 titlePanel("Graficos sobre dados Hospitalares"),
                                 fluidRow(
                                   br(),
                                   div(style = "display: flex; justify-content: center;",
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
                                   )),
                                   fluidRow(
                                     plotlyOutput("grafico_hospital")
                                   )
                                 )
                        ),
             
             
             #ABA DEMOGRAFIA
             navbarMenu('Demografia',
                        tabPanel(
                          "Mortalidade",
                          fluid = T,
                          titlePanel("Graficos sobre mortalidade"),
                          fluidRow(
                            br(),
                            div(style = "display: flex; justify-content: center;",
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
                            ))),
                            fluidRow(
                              plotlyOutput("grafico_mor")
                            )
                        ),
                        tabPanel(
                          "Natalidade",
                          fluid = T,
                          titlePanel("Graficos sobre natalidade"),
                          fluidRow(
                              br(),
                              div(style = "display: flex; justify-content: center;",
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
                            )),
                          fluidRow(
                            plotlyOutput("grafico_nat")
                          )
                        )
                  )
             ))
  
# Funções
server <- function(input, output) {
  # Descrição das variáveis
  output$descricao_variaveis <- renderDataTable({
    descricao <- data.frame(
      "Variável" = c("ano", "cob_ab", "cob_acs", "cob_esf", "cob_priv", "cob_vac_bcg", "cob_vac_hepa", "cob_vac_hepb", "cob_vac_menin", "cob_vac_penta", "cob_vac_pneumo", "cob_vac_polio", "cob_vac_rota", "cob_vac_tvd1", "desp_recp_saude_pc_mun", "desp_recp_saude_pc_mun_def", "desp_recp_saude_pc_uf", "desp_recp_saude_pc_uf_def", "desp_tot_saude_pc_mun", "desp_tot_saude_pc_mun_def", "desp_tot_saude_pc_uf", "desp_tot_saude_pc_uf_def", "gasto_pbf_pc_def", "n_enf", "n_enf_ch", "n_hosp", "n_hosp_csap", "n_leito_nsus", "n_leito_sus", "n_leitouti_nsus", "n_leitouti_sus", "n_med", "n_med_ch", "n_obitos", "n_obitos_csap", "n_obitos_evit", "n_obitos_maldef", "num_familias_bf", "pct_desp_recp_saude_mun", "pct_desp_recp_saude_uf", "pct_mort_maldef", "pct_prenatal_1a6", "pct_prenatal_7m", "pct_prenatal_adeq", "pct_prenatal_zero", "sigla_uf", "tx_enf", "tx_enf_ch", "tx_hosp", "tx_hosp_csap", "tx_leito_nsus", "tx_leito_sus", "tx_leitouti_nsus", "tx_leitouti_sus", "tx_med", "tx_med_ch", "tx_mort", "tx_mort_aj_cens", "tx_mort_aj_oms", "tx_mort_csap", "tx_mort_csap_aj_cens", "tx_mort_csap_aj_oms", "tx_mort_evit", "tx_mort_evit_aj_cens", "tx_mort_evit_aj_oms", "tx_mort_inf_ibge"),
      "Descrição" = c("Ano", "Porcentagem de Cobertura da Atenção Básica", "Porcentagem da Cobertura de Agentes Comunitários de Saúde", "Porcentagem da Cobertura de Estratégia de Saúde da Família", "Porcentagem da Cobertura de Planos de Saúde", "Porcentagem da Cobertura Vacinal de BCG", "Porcentagem da Cobertura Vacinal de Hepatite A", "Porcentagem da Cobertura Vacinal de Hepatite B em crianças até 30 dias", "Porcentagem da Cobertura Vacinal de Meningococo C", "Porcentagem da Cobertura Vacinal de Pentavalente", "Porcentagem da Cobertura Vacinal de Pneumocócica", "Porcentagem da Cobertura Vacinal de Poliomielite", "Porcentagem da Cobertura Vacinal de Rotavírus Humano", "Porcentagem da Cobertura Vacinal de Tríplice Viral (1ª Dose)", "Despesa em Saúde Utilizando Recursos Próprios do Município (por Hab., R$)", "Despesa em Saúde Utilizando Recursos Próprios do Município (por Hab., R$ de 2019)", "Despesa em Saúde Utilizando Recursos Próprios do Estado (por Hab., R$)", "Despesa em Saúde Utilizando Recursos Próprios do Estado (por Hab., R$ de 2019)", "Despesa Total com Saúde Sob Responsabilidade do Município (por Hab., R$)", "Despesa Total com Saúde Sob Responsabilidade do Município (por Hab., R$ de 2019)", "Despesa Total com Saúde Sob Responsabilidade do Estado (por Hab., R$)", "Despesa Total com Saúde Sob Responsabilidade do Estado (por Hab., R$ de 2019)", "Gasto com o Programa Bolsa Familia (por Hab., R$ de 2021)", "Número de Enfermeiros", "Número de Enfermeiros (Padronizados por Carga Horária)", "Número de Hospitalizações Totais", "Número de Hospitalizações por Condições Sensíveis à Atenção Primária", "Número de Leitos Não-SUS", "Número de Leitos SUS", "Número de Leitos de UTI Não-SUS", "Número de Leitos de UTI SUS", "Número de Médicos", "Número de Médicos (Padronizados por Carga Horária)", "Número de Óbitos por Todas as Causas", "Número de Óbitos por Condições Sensíveis a Atenção Primária", "Número de Óbitos por Causas Evitáveis", "Número de Óbitos por Causas Mal Definidas", "Número de beneficiários do Bolsa Familia", "Porcentagem da Participação da Receita Própria Municipal Aplicada em Saúde - EC 29", "Porcentagem da Participação da Receita Própria Estadual Aplicada em Saúde - EC 29", "Mortalidade Bruta por Causas Mal Definidas (%)", "Porcentagem de Nascidos Vivos com 1 a 6 Consultas Pré-Natal", "Porcentagem de Nascidos Vivos com 7 ou Mais Consultas Pré-Natal", "Porcentagem de Nascidos Vivos com Pré-Natal Adequado", "Porcentagem de Nascidos Vivos com Nenhuma Consulta Pré-Natal", "Sigla da UF", "Enfermeiros (por 1.000 Hab.)", "Enfermeiros (Padronizados por Carga Horária, por 1.000 Hab.)", "Hospitalizações (por 100.000 Hab.)", "Hospitalizações por Condições Sensíveis à Atenção Primária (por 100.000 Hab.)", "Leitos Não-SUS (por 100.000 Hab.)", "Leitos SUS (por 100.000 Hab.)", "Leitos de UTI Não-SUS (por 100.000 Hab.)", "Leitos de UTI SUS (por 100.000 Hab.)", "Médicos (por 1.000 Hab.)", "Médicos (Padronizados por Carga Horária, por 1.000 Hab.)", "Mortalidade Bruta (por 100.000 Hab.)", "Mortalidade Ajustada (Censo 2010, por 100.000 Hab.)", "Mortalidade Ajustada (OMS, por 100.000 Hab.)", "Mortalidade Bruta por Condições Sensíveis a Atenção Primária (por 100.000 Hab.)", "Mortalidade Ajustada por Condições Sensíveis à Atenção Primária (Censo, por 100.000 Hab.)", "Mortalidade Ajustada por Condições Sensíveis à Atenção Primária (OMS, por 100.000 Hab.)", "Mortalidade Bruta por Causas Evitáveis (por 100.000 Hab.)", "Mortalidade Ajustada por Causas Evitáveis (Censo, por 100.000 Hab.)", "Mortalidade Ajustada por Causas Evitáveis (OMS, por 100.000 Hab.)", "Mortalidade Infantil (por 1.000 Nascidos Vivos, Média Trienal)")
    )
    
    DT::datatable(descricao)
  })
  
  
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
    y_mor <- labels_mort[input$variavel_mor]
    a <- ggplot(data = df_br, aes(x = ano, y = !!sym(input$variavel_mor))) +
      geom_line(col = "#5499C7") + geom_point(col = "#1F618D") +
      labs(x = "Anos", y = y_mor) +
      theme_minimal() 
    ggplotly(a)
  })
  
  
  # Gráfico Hospitalares
  output$grafico_hospital <- renderPlotly({
    y_hosp = labels_hops[input$variavel_hospital]
    b = ggplot(data = df_br, aes(x = ano, y = !!sym(input$variavel_hospital))) +
      geom_line(col= "#5499C7") + geom_point(col= "#1F618D") +
      labs(x= "Anos", y= y_hosp) +
      theme_minimal()
    
    ggplotly(b)
  })
  
  # Gráfico Natalidade
  output$grafico_nat <- renderPlotly({
    y_nat = labels_nat[input$variavel_nat]
    c = ggplot(data = df_br, aes(x = ano, y = !!sym(input$variavel_nat))) +
      geom_line(col= "#5499C7") + geom_point(col= "#1F618D") +
      labs(x= "Anos", y= y_nat) +
      theme_minimal()
    
    ggplotly(c)
  })
  
  # Mapa Despesas 
  output$mapa_despesas <- renderLeaflet({
    if (input$Cidades == "Todos"){
      df_mapa = inner_join(mapa_muni, df_muni, c('name_muni' = 'nome')) %>% 
        filter(ano == input$Ano10)}
    else{
      df_mapa = inner_join(mapa_muni, df_muni, c('name_muni' = 'nome')) %>% 
        filter(ano == input$Ano10, name_muni == input$Cidades)
    }
    
    pal <-  colorBin("Blues", domain = log(df_mapa[[input$Variavel10]]), bins = 5)
    leaflet(data = df_mapa) %>%
      addPolygons(fillColor = ~ pal(log(df_mapa[[input$Variavel10]])), 
                  fillOpacity = 0.9, 
                  color = "white", 
                  weight = 1,
                  popup = paste("Gasto Municipal:", round(df_mapa[[input$Variavel10]], 2))) %>% 
      addLegend("bottomright", pal= pal, values = ~log(df_mapa[[input$Variavel10]]), labFormat=labelFormat(transform = function(x)exp(x), digits = 1),title = "Despesas Total em Saúde", opacity = 1)
  })
  
  # Grafico Despesas 
  output$grafico_desp <- renderPlotly({  
    
    #y_gastos <- labels_gast1[input$Variavel11]
    df_uf_gastos <- df_uf %>% 
      select(sigla_uf,desp_tot_saude_pc_uf_def,desp_recp_saude_pc_uf,ano) %>% 
      mutate(sigla_uf = factor(sigla_uf)) %>% 
      filter(ano == input$Ano11)
    
    
    g1 = ggplot(data = df_uf_gastos, aes(x = sigla_uf, y = !!sym(input$Variavel11))) +
      geom_bar(stat = "identity", fill = "#1F618A" ) +
      labs(x= "UF", y= "") +
      theme_minimal()
    
    ggplotly(g1)
  })
}

shinyApp(ui = ui, server = server)