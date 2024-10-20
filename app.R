pacman::p_load(shiny, tidyverse, bslib, mirt, mirtCAT, plotly, epiR, shinythemes,markdown,plotly,data.table)

######## Data #################

mfq_params <- readRDS("irt_params.Rds")

diagnostic_measures <- readRDS("case_examples_atualizado.Rds") |> 
  mutate(
    across(c("low_coff", "high_coff"), as.numeric)
  ) 

disorder_vec <- diagnostic_measures$disorder |> unique()

######## Components #############

item <- function(id, label) {
  radioButtons(id, label, 
               choices = list("Não é verdade" = 0,
                              "Às vezes" = 1,
                              "Verdade" = 2),
               selected = 0)
}

######## UI #############
ui <- fluidPage(
  theme = shinytheme("flatly"), # Aplicar o tema Flatly
  tags$head(
    tags$style(HTML(
      "
      .navbar-static-top {
        background-color: #f8f9fa;
        color: #343a40;
      }
      
      .navbar-default .navbar-nav>li>a {
        color: #343a40; 
      }
      .navbar-default .navbar-nav>.active>a {
          background-color: #343a40;
      }
      "
    ))
  ),
  navbarPage(
    title = tagList(
      img(src = "logo.png", height = "30px", style = "margin-right: 20px"), 
      span("Calculadora SMFQ", hidden=TRUE)
    ),
    fluid = TRUE,
    collapsible=TRUE, 
    
    # Aba da Calculadora SMFQ
    tabPanel("Calculadora SMFQ",
             tags$h3("Calculadora transdiagnóstica SMFQ"),
             tags$p("Esta ferramenta utiliza os 13 itens que compõem a escala Short Mood and Feelings Questionnaire (SMFQ). Ao clicar em 'calcular', será gerado um escore-T que reflete o nível de sintomas com base nas respostas apresentadas."),
             sidebarLayout(
               sidebarPanel(width = 6,
                            fluidRow(
                              column(12, markdown("Para cada pergunta, responda como você esteve se sentindo ou agindo nas últimas duas semanas")),
                              column(6,
                                     item("mfq1", label = "Eu me senti muito triste ou infeliz"),
                                     item("mfq2", label = "Eu não consegui me divertir com absolutamente nada"),
                                     item("mfq3", label = "Eu me senti tão cansado(a) que só ficava sentado(a) sem fazer nada"),
                                     item("mfq4", label = "Eu estive muito agitado(a)"),
                                     item("mfq5", label = "Eu senti que eu não valia mais nada"),
                                     item("mfq6", label = "Eu chorei muito"),
                                     item("mfq7", label = "Eu achei difícil raciocinar ou me concentrar")
                              ),
                              column(6,
                                     item("mfq8", label = "Eu me odiei"),
                                     item("mfq9", label = "Eu me senti uma pessoa ruim"),
                                     item("mfq10", label = "Eu me senti sozinho(a)"),
                                     item("mfq11", label = "Eu pensei que ninguém me amava de verdade"),
                                     item("mfq12", label = "Eu pensei que eu nunca seria tão bom(boa) quanto os outros da minha idade"),
                                     item("mfq13", label = "Eu fiz tudo errado"),
                                     actionButton("calcular", "Calcular")
                              ),
                              column(12, p("Messer, S. C., Angold, A., Costello, E. J., Loeber, R., Van Kammen, W., & Stouthamer-Loeber, M. (1995). Development of a short questionnaire for use in epidemiological studies of depression in children and adolescents: Factor composition and structure across development. International journal of methods in psychiatric research."))
                            )
               ),
               mainPanel(width = 6,
                         tabsetPanel(
                           tabPanel("Probabilidade diagnóstica",
                                    fluidRow(
                                      column(12, p("Responda ao questionário e clique em 'Calcular' para obter a probabilidade pós-teste para diferentes condições com base no escore-T calculado. Você verá o ",
                                                   tags$span(style = "color:red; font-weight:bold;", "acréscimo"),
                                                   " ou o ",
                                                   tags$span(style = "color:green; font-weight:bold;", "decréscimo"),
                                                   " em relação à prevalência (probabilidade pré-teste).")),
                                      column(12, uiOutput("show_score_gauge"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_any")),
                                      column(12, plotlyOutput("forestplot_any", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_anyint")),
                                      column(12, plotlyOutput("forestplot_anyint", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_dep")),
                                      column(12, plotlyOutput("forestplot_dep", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_tag")),
                                      column(12, plotlyOutput("forestplot_tag", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_tas")),
                                      column(12, plotlyOutput("forestplot_tas", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_pan")),
                                      column(12, plotlyOutput("forestplot_pan", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("text_tept")),
                                      column(12, plotlyOutput("forestplot_tept", height = "500px", width = "100%"))
                                    )
                           ),
                           tabPanel("Nível de sintomas",
                                    fluidRow(
                                      column(12, plotOutput("barplot", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("show_score_bar"))
                                    ),
                                    fluidRow(
                                      column(12, uiOutput("show_score_explain"))
                                    )
                           ),
                           tabPanel("Mudar prevalência",
                                    fluidRow(
                                      column(12, p("Nesta página, você pode testar a probabilidade pós-teste correspondente ao escore-T calculado utilizando diferentes prevalências."))
                                    ),
                                    fluidRow(
                                      column(6, h3("Qualquer condição")),
                                      column(6, numericInput("prev_any", "Prevalência (%)", 24.16, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(6, h3("Qualquer condição internalizante")),
                                      column(6, numericInput("prev_any_int", "Prevalência (%)", 18.99, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(6, h3("Depressão")),
                                      column(6, numericInput("prev_dep", "Prevalência (%)", 12.79, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(6, h3("Ansiedade Generalizada")),
                                      column(6, numericInput("prev_tag", "Prevalência (%)", 5.01, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(6, h3("Ansiedade Social")),
                                      column(6, numericInput("prev_tas", "Prevalência (%)", 2.76, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(6, h3("Pânico/agorafobia")),
                                      column(6, numericInput("prev_pan", "Prevalência (%)", 5.68, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(6, h3("Estresse pós-traumático")),
                                      column(6, numericInput("prev_tept", "Prevalência (%)", 2.1, min = 0, max = 100))
                                    ),
                                    fluidRow(
                                      column(12, actionButton("calc_prob", "Estimar probabilidade diagnóstica"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_any", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_anyint", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_dep", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_tag", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_tas", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_pan", height = "500px", width = "100%"))
                                    ),
                                    fluidRow(
                                      column(12, plotlyOutput("forestPlot_prev_tept", height = "500px", width = "100%"))
                                    )
                            )
                         )
               )
             )
    ),
    # Aba Saiba mais
    tabPanel("Saiba mais",
             fluidPage(
               titlePanel("Saiba mais"),
               sidebarLayout(
                 sidebarPanel(
                   h4("Short Mood and Feelings Questionnaire"),
                   uiOutput("info_adicional")
                 ),
                 mainPanel(
                   uiOutput("conteudo_principal")
                 )
               ),
               fluidRow(
                 column(
                   width=12,
                   uiOutput("referencias_saiba")
                 )
               )
             )
    ),
    tabPanel("Team",
             fluidPage(
               titlePanel("Team"),
               sidebarLayout(
                 sidebarPanel(
                   h4("Equipe"),
                   p("Conheça nossa equipe e sua experiência.")
                 ),
                 mainPanel(
                   h4("Detalhes da Equipe"),
                   p("Aqui você pode adicionar informações sobre os membros da equipe, suas funções, e contribuições.")
                 )
               )
             )
    )
  ),
  # Aba Saiba mais
  fluid = TRUE
)

######## Server #############
server <- function(input, output) {
  
  output$info_adicional <- renderUI({
    HTML("O <strong>Short Mood and Feelings Questionnaire (SMFQ)</strong> é um instrumento gratuito de 13 itens usado para avaliar sintomas depressivos em jovens nas últimas duas semanas (Angold et al., 1995). Ele apresenta alta correlação com o MFQ original de 33 itens, mas requer metade do tempo para ser completado (Angold et al., 1995; Costello & Angold, 1988). Um ponto de corte clínico recomendado é de 8 ou mais (Angold et al., 1995). A versão brasileira do SMFQ foi traduzida e validada (Pinto, 2014; Sucupira et al., 2017), embora ainda não haja um ponto de corte específico para esta população.
         O SMFQ é tradicionalmente composto por uma estrutura unifatorial (Messer et al., 1995; Sharp et al., 2006).")
  })
  
  output$conteudo_principal <- renderUI({
    tagList(
      p("Uma calculadora sintomática é uma ferramenta web projetada para avaliar e interpretar respostas a questionários psicométricos validados, como o SMFQ. Ao integrar uma calculadora sintomática na prática clínica, profissionais de saúde podem se beneficiar das seguintes maneiras:"),
      tags$ul(
        tags$li(tags$strong("Eficiência:"), " A ferramenta automatiza a coleta e análise de dados, economizando tempo e recursos na avaliação dos pacientes."),
        tags$li(tags$strong("Acurácia Diagnóstica:"), " A combinação de TRI e T-escores proporciona uma avaliação mais precisa dos sintomas, auxiliando na detecção precoce e no acompanhamento das condições de saúde mental."),
        tags$li(tags$strong("Decisões Informadas:"), " Fornece dados padronizados que ajudam os clínicos a tomar decisões baseadas em evidências.")
      ),
      tags$h4(tags$strong("Funcionamento básico")),
      p("A calculadora apresenta aos usuários um conjunto de perguntas do SMFQ, que é composto por 13 itens. Cada resposta é avaliada em uma escala de Likert de 3 pontos, que inclui opções como 'não é verdade', 'às vezes' e 'verdade'. A partir das respostas, é calculado um escore que fornece uma indicação inicial da gravidade dos sintomas depressivos."),
      tags$h4(tags$strong("Teoria de Resposta ao Item")),
      p("Esta calculadora incorpora a Teoria de Resposta ao Item (TRI), uma abordagem estatística que considera não apenas as respostas individuais, mas também as características dos itens do questionário. A TRI permite modelar a probabilidade de um indivíduo responder a um item de uma certa maneira com base em sua habilidade latente, que no caso de um questionário psicométrico, corresponde ao nível do traço psicológico que está sendo medido (como a depressão). A TRI proporciona uma avaliação mais precisa em comparação à Teoria Clássica dos Testes (TCT), que estima o traço latente a partir da soma bruta dos escores em cada item, compondo um escore total. A TRI ajusta o peso de cada item com base em sua dificuldade e discriminação para gerar um escore fatorial."),
      tags$h4(tags$strong("Conversão para T-escore")),
      p("Para padronizar os resultados e facilitar a comparação entre diferentes medidas, a calculadora converte o escore total em um T-escore. Um T-escore é uma medida estatística que transforma escores brutos em uma escala comum, com uma média de 50 e um desvio padrão de 10. A fórmula para a conversão é:"),
      tags$blockquote("T = 50 + (Escore Fatorial * 10)",tags$footer("de Beurs et al., 2022; McCall, 1922")),
      tags$h4(tags$strong("Probabilidade pós-teste")),
      p("A calculadora também incorpora a probabilidade pré-teste, calculada com pesos amostrais, para diferentes condições mentais, usando dados da Coorte Brasileira de Alto Risco para Transtornos Psiquiátricos (BHRCS). Com esses dados, a ferramenta gera uma probabilidade pós-teste por meio do cálculo da razão de verossimilhança por intervalos. Esta estimativa da probabilidade pós-teste ajuda a refinar a análise clínica, permitindo aos profissionais de saúde ajustar suas avaliações com base na verossimilhança de diagnósticos específicos. Isso facilita a tomada de decisões mais informadas e precisas, contribuindo para a personalização e eficácia dos cuidados de saúde mental."),
      actionButton("btn_paper", "Clique para ler o artigo original na íntegra", class = "btn-primary")
      )
  })
  
  output$referencias_saiba <- renderUI({
    tagList(
      tags$h4(tags$strong("Referências")),
      p("Texto")
    )
  })
  
  # Observador para o botão
  observeEvent(input$btn_paper, {
    showModal(modalDialog(
      title = "Aviso",
      "Você será redirecionado para...",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  values <- reactiveValues(escoreT = 0, df_prob = NULL)
  
  score <- reactive({
    
    respostas <- map(1:13, \(i) {
      as.numeric(input[[paste0("mfq", i)]])
    })
    
    # Create generic model with desired params
    mod_generic <- generate.mirt_object(mfq_params, itemtype = "graded")
    
    # Get new theta in t score
    theta <- fscores(mod_generic, response.pattern = respostas)[1]
    
    values$escoreT <- theta*10 + 50
    
    return(values$escoreT)
    
  }) |> 
    bindEvent(input$calcular)
  
  score_2 <- reactive({
    
    respostas <- map(1:13, \(i) {
      as.numeric(input[[paste0("mfq", i)]])
    })
    
    # Create generic model with desired params
    mod_generic <- generate.mirt_object(mfq_params, itemtype = "graded")
    
    # Get new theta in t score
    theta <- fscores(mod_generic, response.pattern = respostas)[1]
    
    values$escoreT <- theta*10 + 50
    
    return(values$escoreT)
    
  }) |> 
    bindEvent(input$calcular)
  
  barPlot <- reactive({
    # Definir a cor da barra com base no escore
    bar_color <- if (values$escoreT >= 60) {
      "#e7298a"  # Cor para escore >= 60
    } else if (values$escoreT >= 58) {
      "#7570b3"  # Cor para escore >= 58
    } else if (values$escoreT >= 56) {
      "#d95f02"  # Cor para escore >= 56
    } else if (values$escoreT >= 54) {
      "#1b9e77"  # Cor para escore >= 54
    } else {
      "grey"  # Cor padrão para escore < 54
    }
    
    if (values$escoreT == 0) {
    ggplot(data.frame(score = "Escore-T", n = 0:100), aes(x = score, y = n)) +
      geom_blank() +
      xlab("Pontuação") +
      ylab("Escore T") +
      scale_y_continuous(breaks = seq(0, 100, by = 10))  # Define o intervalo de 10 em 10 no eixo y
  }
    else {
      ggplot(data.frame(score = "Escore-T", n = values$escoreT), aes(x = score, y = n)) +
        geom_bar(stat = "identity", fill = bar_color, width = 0.5) +
        ylim(0, 100) +
        scale_y_continuous(breaks = seq(0, 100, by = 10)) +
        xlab("Pontuação obtida") +    
        ylab("Escore-T") +
        annotate("text", x = 1.5, y = 50, label = "Média", color = "grey", size = 4, hjust = 0) +
        annotate("text", x = 1.5, y = 75, label = "2.5 DP", color = "grey", size = 4, hjust = 0) +
        annotate("text", x = 1.5, y = 25, label = "2.5 DP", color = "grey", size = 4, hjust = 0) +
        geom_hline(aes(yintercept = 53, color = "Qualquer condição e Transtorno de Ansiedade Social (TAS)"), linetype = "dashed") +
        geom_hline(aes(yintercept = 55, color = "Qualquer condição internalizante e Transtorno Depressivo Maior (TDM)"), linetype = "dashed") +
        geom_hline(aes(yintercept = 57, color = "Transtorno do Pânico ou Agorafobia e Transtorno do Estresse Pós-Traumático (TEPT)"), linetype = "dashed") +
        geom_hline(aes(yintercept = 59, color = "Transtorno de Ansiedade Generalizada (TAG)"), linetype = "dashed") +
        scale_color_manual(name = "Pontos de corte",
                           values = c("Qualquer condição e Transtorno de Ansiedade Social (TAS)" = "#1b9e77",
                                      "Qualquer condição internalizante e Transtorno Depressivo Maior (TDM)" = "#d95f02",
                                      "Transtorno do Pânico ou Agorafobia e Transtorno do Estresse Pós-Traumático (TEPT)" = "#7570b3",
                                      "Transtorno de Ansiedade Generalizada (TAG)" = "#e7298a")) +
        labs(color = "Pontos de corte") +
        theme(
          legend.position = "bottom",            # Posiciona a legenda abaixo do gráfico
          legend.box.margin = margin(0, 0, 10, 0),
          legend.text = element_text(size = 12), # Ajusta o tamanho do texto da legenda
          legend.title = element_text(size = 14) # Ajusta o tamanho do título da legenda
        ) +
        guides(color = guide_legend(nrow = 4)) + # Define o número de linhas na legenda
        coord_flip()  # Inverte os eixos
        }
  }) 
  
  forestPlot_prev_any <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Qualquer transtorno", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para qualquer condição"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_any))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Qualquer transtorno", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para qualquer condição"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_any))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Qualquer transtorno", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para qualquer condição"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_any))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Qualquer transtorno", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para qualquer condição"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_any))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Qualquer transtorno", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para qualquer condição"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_any))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Qualquer transtorno", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para qualquer condição"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_any))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Qualquer transtorno", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para qualquer condição"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_any))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Qualquer transtorno", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_any / (100-input$prev_any) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para qualquer condição"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_any, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_any))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_prev_anyint <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Qualquer transtorno internalizante", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_any_int))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Qualquer transtorno internalizante", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_any_int))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Qualquer transtorno internalizante", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_any_int))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Qualquer transtorno internalizante", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_any_int))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Qualquer transtorno internalizante", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_any_int))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Qualquer transtorno internalizante", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_any_int))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Qualquer transtorno internalizante", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_any_int))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno internalizante
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Qualquer transtorno internalizante", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_any_int / (100-input$prev_any_int) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_any_int, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_any_int))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_prev_dep <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Depressão", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para depressão"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_dep))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Depressão", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_dep_int / (100-input$prev_dep_int) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para depressão"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_dep))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Depressão", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para depressão"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_dep))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Depressão", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para depressão"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_dep))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Depressão", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para depressão"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_dep))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Depressão", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para depressão"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_dep))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Depressão", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para depressão"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_dep))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno internalizante
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Depressão", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_dep / (100-input$prev_dep) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para depressão"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_dep, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_dep))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_prev_tag <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Ansiedade Generalizada", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_tag))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Ansiedade Generalizada", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_tag))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Ansiedade Generalizada", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_tag))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Ansiedade Generalizada", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_tag))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Ansiedade Generalizada", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_tag))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Ansiedade Generalizada", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_tag))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Ansiedade Generalizada", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_tag))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno internalizante
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Ansiedade Generalizada", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_tag / (100-input$prev_tag) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_tag, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_tag))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_prev_tas <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Ansiedade Social", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para ansiedade social"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_tas))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Ansiedade Social", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_tas_int / (100-input$prev_tas_int) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para ansiedade social"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_tas))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Ansiedade Social", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para ansiedade social"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_tas))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Ansiedade Social", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para ansiedade social"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_tas))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Ansiedade Social", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para ansiedade social"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_tas))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Ansiedade Social", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para ansiedade social"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_tas))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Ansiedade Social", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para ansiedade social"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_tas))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno internalizante
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Ansiedade Social", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_tas / (100-input$prev_tas) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para ansiedade social"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_tas, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_tas))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_prev_pan <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Pânico/Agorafobia", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_pan))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Pânico/Agorafobia", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_pan_int / (100-input$prev_pan_int) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_pan))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Pânico/Agorafobia", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_pan))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Pânico/Agorafobia", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_pan))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Pânico/Agorafobia", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_pan))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Pânico/Agorafobia", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_pan))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Pânico/Agorafobia", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_pan))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno internalizante
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Pânico/Agorafobia", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_pan / (100-input$prev_pan) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_pan, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_pan))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_prev_tept <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "TEPT", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = input$prev_tept))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "TEPT", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- input$prev_tept_int / (100-input$prev_tept_int) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = input$prev_tept))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "TEPT", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = input$prev_tept))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "TEPT", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = input$prev_tept))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "TEPT", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = input$prev_tept))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "TEPT", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = input$prev_tept))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "TEPT", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = input$prev_tept))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno internalizante
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "TEPT", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- input$prev_tept / (100-input$prev_tept) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = input$prev_tept, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = input$prev_tept))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"))
                  }
                }
              }
            }
          }
        }
      }
    }
  }) |> 
    bindEvent(input$calc_prob)
  
  forestPlot_any <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Qualquer transtorno", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para qualquer condição"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 24.16, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 24.16))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 4.4% - 8%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 40, 44.9999))
      {
        #Qualquer transtorno
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Qualquer transtorno", low_coff == 40) %>%
          pull(likelihood_ratio)
        post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para qualquer condição"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 24.16, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 24.16))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 4.7% - 8%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 45, 49.9999))
        {
          #Qualquer transtorno
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Qualquer transtorno", low_coff == 45) %>%
            pull(likelihood_ratio)
          post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para qualquer condição"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 24.16, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 24.16))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 9.8% - 16.5%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 50, 54.9999))
          {
            #Qualquer transtorno
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Qualquer transtorno", low_coff == 50) %>%
              pull(likelihood_ratio)
            post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para qualquer condição"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 24.16, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 24.16))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 15.7% - 23.3%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 55, 59.9999))
            {
              #Qualquer transtorno
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Qualquer transtorno", low_coff == 55) %>%
                pull(likelihood_ratio)
              post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para qualquer condição"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 24.16, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 24.16))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 30.8% - 41%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 60, 64.9999))
              {
                #Qualquer transtorno
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Qualquer transtorno", low_coff == 60) %>%
                  pull(likelihood_ratio)
                post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para qualquer condição"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 24.16, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 24.16))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 54.3% - 69.4%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 65, 69.9999))
                {
                  #Qualquer transtorno
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Qualquer transtorno", low_coff == 65) %>%
                    pull(likelihood_ratio)
                  post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para qualquer condição"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 24.16, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 24.16))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 71.6% - 91%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
                else
                {
                  if(between(round(score_2(), 2), 70, 79.9999))
                  {
                    #Qualquer transtorno
                    likelihood_ratio_value <- diagnostic_measures %>%
                      filter(disorder == "Qualquer transtorno", low_coff == 70) %>%
                      pull(likelihood_ratio)
                    post_odds <- 24.16 / (100-24.16) * likelihood_ratio_value
                    post_prob <- post_odds / (1 + post_odds)
                    
                    plot_ly(
                      domain = list(x = c(0, 1), y = c(0, 1)),
                      value = post_prob * 100,
                      number = list(suffix = "%"),
                      title = list(text = "Probabilidade pós-teste para qualquer condição"),
                      type = "indicator",
                      mode = "gauge+number+delta",
                      delta = list(reference = 24.16, increasing = list(color = "red"), 
                                   decreasing = list(color = "forestgreen")),
                      gauge = list(
                        axis =list(range = list(NULL, 100)),
                        bar = list(color = "darkblue"),
                        threshold = list(
                          line = list(color = "red", width = 4),
                          hovertext="Prevalência",
                          thickness = 0.75,
                          value = 24.16))) |> 
                      layout(margin = list(l=20,r=30),
                             font = list(family = "Arial", color="black"),
                             annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 77.2% - 96.3%)",
                                                hovertext = "IC 95%",
                                                font = list(size=30, color="lightgray"),
                                                showarrow = FALSE))
                  }
                }
              }
            }
          }
        }
      }
    }
    }) |> 
      bindEvent(input$calcular)
  
  forestPlot_anyint <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Qualquer transtorno internalizante", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 18.99, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 18.99))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 1.7% - 4.2%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
    if(between(round(score_2(), 2), 40, 44.9999))
    {
      #Qualquer transtorno internalizante
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Qualquer transtorno internalizante", low_coff == 40) %>%
        pull(likelihood_ratio)
      post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 18.99, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 18.99))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 1.7% - 4.2%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 45, 49.9999))
      {
        #Qualquer transtorno internalizante
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Qualquer transtorno internalizante", low_coff == 45) %>%
          pull(likelihood_ratio)
        post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 18.99, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 18.99))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 6.8% - 12.5%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 50, 54.9999))
        {
          #Qualquer transtorno internalizante
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Qualquer transtorno internalizante", low_coff == 50) %>%
            pull(likelihood_ratio)
          post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 18.99, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 18.99))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 10.7% - 17.2%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 55, 59.9999))
          {
            #Qualquer transtorno internalizante
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Qualquer transtorno internalizante", low_coff == 55) %>%
              pull(likelihood_ratio)
            post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 18.99, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 18.99))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 22.4% - 31.5%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 60, 64.9999))
            {
              #Qualquer transtorno internalizante
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Qualquer transtorno internalizante", low_coff == 60) %>%
                pull(likelihood_ratio)
              post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 18.99, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 18.99))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 46.6% - 61.6%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 65, 69.9999))
              {
                #Qualquer transtorno internalizante
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Qualquer transtorno internalizante", low_coff == 65) %>%
                  pull(likelihood_ratio)
                post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 18.99, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 18.99))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 68.9% - 89.3%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 70, 79.9999))
                {
                  #Qualquer transtorno internalizante
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Qualquer transtorno internalizante", low_coff == 70) %>%
                    pull(likelihood_ratio)
                  post_odds <- 18.99 / (100-18.99) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para qualquer condição internalizante"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 18.99, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 18.99))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 73.8% - 94.6%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
              }
            }
          }
        }
      }
    }
    }
  }) |> 
    bindEvent(input$calcular)
  
  forestPlot_dep <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Depressão
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Depressão", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds) 
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para depressão"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 12.79, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 12.79))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.9% - 3%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
    if(between(round(score_2(), 2), 40, 44.9999))
    {
      #Depressão
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Depressão", low_coff == 40) %>%
        pull(likelihood_ratio)
      post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para depressão"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 12.79, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 12.79))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.9% - 3%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 45, 49.9999))
      {
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Depressão", low_coff == 45) %>%
          pull(likelihood_ratio)
        post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para depressão"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 12.79, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 12.79))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 3.9% - 8.5%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 50, 54.9999))
        {
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Depressão", low_coff == 50) %>%
            pull(likelihood_ratio)
          post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para depressão"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 12.79, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 12.79))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 5.2% - 10.1%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 55, 59.9999))
          {
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Depressão", low_coff == 55) %>%
              pull(likelihood_ratio)
            post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para depressão"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 12.79, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 12.79))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 15.2% - 23%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 60, 64.9999))
            {
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Depressão", low_coff == 60) %>%
                pull(likelihood_ratio)
              post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para depressão"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 12.79, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 12.79))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 28.8% - 42.2%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 65, 69.9999))
              {
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Depressão", low_coff == 65) %>%
                  pull(likelihood_ratio)
                post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para depressão"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 12.79, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 12.79))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 47.5% - 71%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 70, 79.9999))
                {
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Depressão", low_coff == 70) %>%
                    pull(likelihood_ratio)
                  post_odds <- 12.79 / (100-12.79) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para depressão"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 12.79, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 12.79))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 49.6% - 76.2%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
              }
            }
          }
        }
      }
      }
    }
  }) |> 
    bindEvent(input$calcular)
  
  forestPlot_tag <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #TAG
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Ansiedade Generalizada", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 5.01, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 5.01))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.2% - 1.6%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
    if(between(round(score_2(), 2), 40, 44.9999))
    {
      #TAG
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Ansiedade Generalizada", low_coff == 40) %>%
        pull(likelihood_ratio)
      post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 5.01, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 5.01))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.2% - 1.6%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 45, 49.9999))
      {
        #TAG
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Ansiedade Generalizada", low_coff == 45) %>%
          pull(likelihood_ratio)
        post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 5.01, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 5.01))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 1.2% - 4.6%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 50, 54.9999))
        {
          #TAG
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Ansiedade Generalizada", low_coff == 50) %>%
            pull(likelihood_ratio)
          post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 5.01, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 5.01))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 1.4% - 4.7%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 55, 59.9999))
          {
            #TAG
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Ansiedade Generalizada", low_coff == 55) %>%
              pull(likelihood_ratio)
            post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 5.01, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 5.01))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 3% - 7.6%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 60, 64.9999))
            {
              #TAG
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Ansiedade Generalizada", low_coff == 60) %>%
                pull(likelihood_ratio)
              post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 5.01, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 5.01))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 12.4% - 22.2%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 65, 69.9999))
              {
                #TAG
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Ansiedade Generalizada", low_coff == 65) %>%
                  pull(likelihood_ratio)
                post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 5.01, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 5.01))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 18.2% - 37.4%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 70, 79.9999))
                {
                  #TAG
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Ansiedade Generalizada", low_coff == 70) %>%
                    pull(likelihood_ratio)
                  post_odds <- 5.01 / (100-5.01) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para ansiedade generalizada"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 5.01, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 5.01))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 11.7% - 32%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
              }
            }
          }
        }
        }
      }
    }
  }) |> 
    bindEvent(input$calcular)
  
  forestPlot_tas <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #TAS
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Ansiedade Social", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para ansiedade social"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 2.76, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 2.76))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0% - 1.2%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
    if(between(round(score_2(), 2), 40, 44.9999))
    {
      #TAS
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Ansiedade Social", low_coff == 40) %>%
        pull(likelihood_ratio)
      post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para ansiedade social"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 2.76, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 2.76))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0% - 1.2%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 45, 49.9999))
      {
        #TAS
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Ansiedade Social", low_coff == 45) %>%
          pull(likelihood_ratio)
        post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para ansiedade social"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 2.76, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 2.76))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.5% - 3%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 50, 54.9999))
        {
          #TAS
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Ansiedade Social", low_coff == 50) %>%
            pull(likelihood_ratio)
          post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para ansiedade social"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 2.76, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 2.76))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.8% - 3.6%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 55, 59.9999))
          {
            #TAS
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Ansiedade Social", low_coff == 55) %>%
              pull(likelihood_ratio)
            post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para ansiedade social"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 2.76, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 2.76))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 2.5% - 6.6%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 60, 64.9999))
            {
              #TAS
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Ansiedade Social", low_coff == 60) %>%
                pull(likelihood_ratio)
              post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para ansiedade social"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 2.76, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 2.76))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 5.8% - 13.3%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 65, 69.9999))
              {
                #TAS
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Ansiedade Social", low_coff == 65) %>%
                  pull(likelihood_ratio)
                post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para ansiedade social"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 2.76, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 2.76))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 6.9% - 21.3%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 70, 79.9999))
                {
                  #TAS
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Ansiedade Social", low_coff == 70) %>%
                    pull(likelihood_ratio)
                  post_odds <- 2.76 / (100-2.76) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para ansiedade social"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 2.76, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 2.76))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 2.8% - 17%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
              }
            }
          }
        }
      }
      }
    }
  }) |> 
    bindEvent(input$calcular)
  
  forestPlot_pan <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #Pânico
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Pânico/Agorafobia", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 5.68, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 5.68))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.2% - 1.6%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
    if(between(round(score_2(), 2), 40, 44.9999))
    {
      #Pânico
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "Pânico/Agorafobia", low_coff == 40) %>%
        pull(likelihood_ratio)
      post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 5.68, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 5.68))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.2% - 1.6%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 45, 49.9999))
      {
        #Pânico
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "Pânico/Agorafobia", low_coff == 45) %>%
          pull(likelihood_ratio)
        post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 5.68, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 5.68))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.6% - 3.2%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 50, 54.9999))
        {
          #Pânico
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "Pânico/Agorafobia", low_coff == 50) %>%
            pull(likelihood_ratio)
          post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 5.68, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 5.68))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 2.9% - 7%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 55, 59.9999))
          {
            #Pânico
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "Pânico/Agorafobia", low_coff == 55) %>%
              pull(likelihood_ratio)
            post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 5.68, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 5.68))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 3.4% - 8.3%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 60, 64.9999))
            {
              #Pânico
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "Pânico/Agorafobia", low_coff == 60) %>%
                pull(likelihood_ratio)
              post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 5.68, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 5.68))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 13.2% - 23.4%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 65, 69.9999))
              {
                #Pânico
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "Pânico/Agorafobia", low_coff == 65) %>%
                  pull(likelihood_ratio)
                post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 5.68, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 5.68))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 20.4% - 40.1%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 70, 79.9999))
                {
                  #Pânico
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "Pânico/Agorafobia", low_coff == 70) %>%
                    pull(likelihood_ratio)
                  post_odds <- 5.68 / (100-5.68) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para pânico/agorafobia"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 5.68, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 5.68))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 18.9% - 41.7%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
              }
            }
          }
        }
      }
      }
    }
  }) |> 
    bindEvent(input$calcular)
  
  forestPlot_tept <- reactive({
    if(between(round(score_2(), 2), 35, 39.9999))
    {
      #TEPT
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "TEPT", low_coff == 35) %>%
        pull(likelihood_ratio)
      post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 2.1, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 2.1))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.4% - 2.2%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
    if(between(round(score_2(), 2), 40, 44.9999))
    {
      #TEPT
      likelihood_ratio_value <- diagnostic_measures %>%
        filter(disorder == "TEPT", low_coff == 40) %>%
        pull(likelihood_ratio)
      post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
      post_prob <- post_odds / (1 + post_odds)
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = post_prob * 100,
        number = list(suffix = "%"),
        title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 2.1, increasing = list(color = "red"), 
                     decreasing = list(color = "forestgreen")),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          bar = list(color = "darkblue"),
          threshold = list(
            line = list(color = "red", width = 4),
            hovertext="Prevalência",
            thickness = 0.75,
            value = 2.1))) |> 
        layout(margin = list(l=20,r=30),
               font = list(family = "Arial", color="black"),
               annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.4% - 2.2%)",
                                  hovertext = "IC 95%",
                                  font = list(size=30, color="lightgray"),
                                  showarrow = FALSE))
    }
    else
    {
      if(between(round(score_2(), 2), 45, 49.9999))
      {
        #TEPT
        likelihood_ratio_value <- diagnostic_measures %>%
          filter(disorder == "TEPT", low_coff == 45) %>%
          pull(likelihood_ratio)
        post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
        post_prob <- post_odds / (1 + post_odds)
        
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = post_prob * 100,
          number = list(suffix = "%"),
          title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = 2.1, increasing = list(color = "red"), 
                       decreasing = list(color = "forestgreen")),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            bar = list(color = "darkblue"),
            threshold = list(
              line = list(color = "red", width = 4),
              hovertext="Prevalência",
              thickness = 0.75,
              value = 2.1))) |> 
          layout(margin = list(l=20,r=30),
                 font = list(family = "Arial", color="black"),
                 annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.6% - 3.2%)",
                                    hovertext = "IC 95%",
                                    font = list(size=30, color="lightgray"),
                                    showarrow = FALSE))
      }
      else
      {
        if(between(round(score_2(), 2), 50, 54.9999))
        {
          #TEPT
          likelihood_ratio_value <- diagnostic_measures %>%
            filter(disorder == "TEPT", low_coff == 50) %>%
            pull(likelihood_ratio)
          post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
          post_prob <- post_odds / (1 + post_odds)
          
          plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = post_prob * 100,
            number = list(suffix = "%"),
            title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(reference = 2.1, increasing = list(color = "red"), 
                         decreasing = list(color = "forestgreen")),
            gauge = list(
              axis =list(range = list(NULL, 100)),
              bar = list(color = "darkblue"),
              threshold = list(
                line = list(color = "red", width = 4),
                hovertext="Prevalência",
                thickness = 0.75,
                value = 2.1))) |> 
            layout(margin = list(l=20,r=30),
                   font = list(family = "Arial", color="black"),
                   annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.3% - 2.4%)",
                                      hovertext = "IC 95%",
                                      font = list(size=30, color="lightgray"),
                                      showarrow = FALSE))
        }
        else
        {
          if(between(round(score_2(), 2), 55, 59.9999))
          {
            #TEPT
            likelihood_ratio_value <- diagnostic_measures %>%
              filter(disorder == "TEPT", low_coff == 55) %>%
              pull(likelihood_ratio)
            post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
            post_prob <- post_odds / (1 + post_odds)
            
            plot_ly(
              domain = list(x = c(0, 1), y = c(0, 1)),
              value = post_prob * 100,
              number = list(suffix = "%"),
              title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
              type = "indicator",
              mode = "gauge+number+delta",
              delta = list(reference = 2.1, increasing = list(color = "red"), 
                           decreasing = list(color = "forestgreen")),
              gauge = list(
                axis =list(range = list(NULL, 100)),
                bar = list(color = "darkblue"),
                threshold = list(
                  line = list(color = "red", width = 4),
                  hovertext="Prevalência",
                  thickness = 0.75,
                  value = 2.1))) |> 
              layout(margin = list(l=20,r=30),
                     font = list(family = "Arial", color="black"),
                     annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 0.7% - 3.6%)",
                                        hovertext = "IC 95%",
                                        font = list(size=30, color="lightgray"),
                                        showarrow = FALSE))
          }
          else
          {
            if(between(round(score_2(), 2), 60, 64.9999))
            {
              #TEPT
              likelihood_ratio_value <- diagnostic_measures %>%
                filter(disorder == "TEPT", low_coff == 60) %>%
                pull(likelihood_ratio)
              post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
              post_prob <- post_odds / (1 + post_odds)
              
              plot_ly(
                domain = list(x = c(0, 1), y = c(0, 1)),
                value = post_prob * 100,
                number = list(suffix = "%"),
                title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                type = "indicator",
                mode = "gauge+number+delta",
                delta = list(reference = 2.1, increasing = list(color = "red"), 
                             decreasing = list(color = "forestgreen")),
                gauge = list(
                  axis =list(range = list(NULL, 100)),
                  bar = list(color = "darkblue"),
                  threshold = list(
                    line = list(color = "red", width = 4),
                    hovertext="Prevalência",
                    thickness = 0.75,
                    value = 2.1))) |> 
                layout(margin = list(l=20,r=30),
                       font = list(family = "Arial", color="black"),
                       annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 2% - 7.5%)",
                                          hovertext = "IC 95%",
                                          font = list(size=30, color="lightgray"),
                                          showarrow = FALSE))
            }
            else
            {
              if(between(round(score_2(), 2), 65, 69.9999))
              {
                #TEPT
                likelihood_ratio_value <- diagnostic_measures %>%
                  filter(disorder == "TEPT", low_coff == 65) %>%
                  pull(likelihood_ratio)
                post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
                post_prob <- post_odds / (1 + post_odds)
                
                plot_ly(
                  domain = list(x = c(0, 1), y = c(0, 1)),
                  value = post_prob * 100,
                  number = list(suffix = "%"),
                  title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                  type = "indicator",
                  mode = "gauge+number+delta",
                  delta = list(reference = 2.1, increasing = list(color = "red"), 
                               decreasing = list(color = "forestgreen")),
                  gauge = list(
                    axis =list(range = list(NULL, 100)),
                    bar = list(color = "darkblue"),
                    threshold = list(
                      line = list(color = "red", width = 4),
                      hovertext="Prevalência",
                      thickness = 0.75,
                      value = 2.1))) |> 
                  layout(margin = list(l=20,r=30),
                         font = list(family = "Arial", color="black"),
                         annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 8.8% - 23.1%)",
                                            hovertext = "IC 95%",
                                            font = list(size=30, color="lightgray"),
                                            showarrow = FALSE))
              }
              else
              {
                if(between(round(score_2(), 2), 70, 79.9999))
                {
                  #TEPT
                  likelihood_ratio_value <- diagnostic_measures %>%
                    filter(disorder == "TEPT", low_coff == 70) %>%
                    pull(likelihood_ratio)
                  post_odds <- 2.1 / (100-2.1) * likelihood_ratio_value
                  post_prob <- post_odds / (1 + post_odds)
                  
                  plot_ly(
                    domain = list(x = c(0, 1), y = c(0, 1)),
                    value = post_prob * 100,
                    number = list(suffix = "%"),
                    title = list(text = "Probabilidade pós-teste para estresse pós-traumático"),
                    type = "indicator",
                    mode = "gauge+number+delta",
                    delta = list(reference = 2.1, increasing = list(color = "red"), 
                                 decreasing = list(color = "forestgreen")),
                    gauge = list(
                      axis =list(range = list(NULL, 100)),
                      bar = list(color = "darkblue"),
                      threshold = list(
                        line = list(color = "red", width = 4),
                        hovertext="Prevalência",
                        thickness = 0.75,
                        value = 2.1))) |> 
                    layout(margin = list(l=20,r=30),
                           font = list(family = "Arial", color="black"),
                           annotations = list(x = 0.5, y = 0.015, text = "(IC 95%: 3.7% - 14.7%)",
                                              hovertext = "IC 95%",
                                              font = list(size=30, color="lightgray"),
                                              showarrow = FALSE))
                }
              }
            }
          }
        }
        }
      }
    }
  }) |> 
    bindEvent(input$calcular)
  
  output$barplot <- renderPlot({
    barPlot()
  }) 
  
  output$forestPlot_prev_any <- renderPlotly({
    forestPlot_prev_any()
  })
  
  output$forestPlot_prev_anyint <- renderPlotly({
    forestPlot_prev_anyint()
  })
  
  output$forestPlot_prev_dep <- renderPlotly({
    forestPlot_prev_dep()
  })
  
  output$forestPlot_prev_tag <- renderPlotly({
    forestPlot_prev_tag()
  })
  
  output$forestPlot_prev_tas <- renderPlotly({
    forestPlot_prev_tas()
  })
  
  output$forestPlot_prev_pan <- renderPlotly({
    forestPlot_prev_pan()
  })
  
  output$forestPlot_prev_tept <- renderPlotly({
    forestPlot_prev_tept()
  })
  
  output$forestplot_any <- renderPlotly({
    forestPlot_any()
  })
  
  output$forestplot_anyint <- renderPlotly({
    forestPlot_anyint()
  })
  
  output$forestplot_dep <- renderPlotly({
    forestPlot_dep()
  })
  
  output$forestplot_tag <- renderPlotly({
    forestPlot_tag()
  })
  
  output$forestplot_tas <- renderPlotly({
    forestPlot_tas()
  })
  
  output$forestplot_pan <- renderPlotly({
    forestPlot_pan()
  })
  
  output$forestplot_tept <- renderPlotly({
    forestPlot_tept()
  })
  
  output$show_score_bar <- renderUI({
    # Cria o título e o texto com base no valor do escore
    score_value <- round(score(), 2)
    
    # Define o título
    title <- tags$h2(paste("Escore-T obtido: ", score_value))
    
    # Define o texto com base no valor do escore
    text <- if (score_value >= 60) {
      p("Esse escore está acima do ponto de corte para comorbidades psiquiátricas (qualquer condição, qualquer condição internalizante, Transtorno Depressivo Maior, Transtorno de Ansiedade Social, Transtorno do Pânico ou Agorafobia, Transtorno do Estresse Pós-Traumático e Transtorno de Ansiedade Generalizada). Indica um nível elevado de sintomas.")
    } else if (score_value >= 58) {
      p("Esse escore está acima do ponto de corte para comorbidades psiquiátricas (qualquer condição, qualquer condição internalizante, Transtorno Depressivo Maior, Transtorno de Ansiedade Social, Transtorno do Pânico ou Agorafobia e Transtorno do Estresse Pós-Traumático). Indica um nível alto de sintomas.")
    } else if (score_value >= 56) {
      p("Esse escore está acima do ponto de corte para comorbidades psiquiátricas (qualquer condição, qualquer condição internalizante, Transtorno Depressivo Maior e Transtorno de Ansiedade Social). Indica um nível moderado de sintomas.")
    } else if (score_value >= 54) {
      p("Esse escore está acima do ponto de corte para comorbidades psiquiátricas (qualquer condição e Transtorno de Ansiedade Social). Indica um nível baixo de sintomas.")
    } else {
      p("O valor do Escore T está abaixo do ponto de corte para comorbidades psiquiátricas. Indica ausência ou nível muito baixo de sintomas.")
    }
    
    # Retorna o título e o texto juntos
    tagList(title, text)
  })
  
  output$show_score_gauge <- renderUI({
    tagList(
      tags$h2(tags$strong("Escore-T obtido: ", round(score_2(),2))),
      tags$p("Verifique a aba 'Nível de sintomas' para entender o que esse escore representa")
    )
  }) 
  
  output$text_any <- renderUI({
    tagList(
      tags$h3(tags$strong("Qualquer condição")),
      p("A prevalência (probabilidade pré-teste) é 24.16%")
    )
  }) |> bindEvent(input$calcular)
  
  output$text_anyint <- renderUI({
    tagList(
      tags$h3(tags$strong("Qualquer condição internalizante")),
      p("A prevalência (probabilidade pré-teste) é 18.99%")
    )
  }) |> bindEvent(input$calcular)
  
  output$text_dep <- renderUI({
    tagList(
      tags$h3(tags$strong("Depressão")),
      p("A prevalência (probabilidade pré-teste) é 12.79%")
    )
  }) |> bindEvent(input$calcular)
  
  output$text_tag <- renderUI({
    tagList(
      tags$h3(tags$strong("Ansiedade Generalizada")),
      p("A prevalência (probabilidade pré-teste) é 5.01%")
    )
  }) |> bindEvent(input$calcular)
  
  output$text_tas <- renderUI({
    tagList(
      tags$h3(tags$strong("Ansiedade Social")),
      p("A prevalência (probabilidade pré-teste) é 2.76%")
    )
  }) |> bindEvent(input$calcular)
  
  output$text_pan <- renderUI({
    tagList(
      tags$h3(tags$strong("Pânico/agorafobia")),
      p("A prevalência (probabilidade pré-teste) é 5.68%")
    )
  }) |> bindEvent(input$calcular)
  
  output$text_tept <- renderUI({
    tagList(
      tags$h3(tags$strong("Estresse pós-traumático")),
      p("A prevalência (probabilidade pré-teste) é 2.1%")
    )
  }) |> bindEvent(input$calcular)
  
}

shinyApp(ui = ui, server = server)