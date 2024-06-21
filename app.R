pacman::p_load(shiny, tidyverse, bslib, mirt, mirtCAT, plotly, epiR)

######## Data #################
mfq_params <- readRDS("irt_params.Rds")

diagnostic_measures <- readRDS("case_examples_new.Rds") |> 
  separate_wider_delim(cols = cutoff, delim = " - ", names = c("low_coff", "high_coff")) |> 
  mutate(
    across(c("low_coff", "high_coff"), as.numeric),
    disorder = case_when(
      disorder == "ANY" ~ "Qualquer transtorno",
      disorder == "DEP" ~ "Depressão",
      disorder == "GEN_ANX" ~ "Ansiedade Generalizada",
      disorder == "SOC_PHOB" ~ "Ansiedade Social",
      disorder == "PANIC_AGORAPHOB" ~ "Pânico/Agorafobia",
      disorder == "PTSD" ~ "TEPT",
      disorder == "ADHD" ~ "TDAH",
      disorder == "CD" ~ "Conduta"
    )
  ) |> filter(disorder !=  "TDAH", disorder != "Conduta")

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
ui <- page_navbar(
  title = "MFQ",
  bg = "#2D89C8",
  nav_panel(
    title = "Escore", 
    
    tags$h3("Calculadora"),
    sidebarLayout(
      sidebarPanel(width = 6,
                   fluidRow(
                     column(12, markdown("Para cada pergunta, por favor diga como você esteve se sentindo ou agindo nas últimas duas semanas")),
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
                  tabPanel("Nível de sintomas",
                    fluidRow(
                      column(12, plotOutput("barplot", height = "600px", width = "100%"))
                    ),
                    fluidRow(
                      column(12, textOutput("show_score_bar"))
                    ),
                  ),
                  tabPanel("Probabilidade diagnóstica",
                    fluidRow(
                      column(6, numericInput("prev", "Prevalência (%)", 10, min = 0, max = 100)),
                      column(6, selectInput("transt", "Transtorno", choices = disorder_vec)),
                      column(6, textOutput("show_score_gauge")),
                      column(6, actionButton("calc_prob", "Estimar probabilidade diagnóstica"))
                    ),
                    fluidRow(
                      column(12, plotlyOutput("forestplot", height = "600px", width = "100%"))
                    )
                  )
                )
      )  
    )
  ),
  fluid = T
)

######## Server #############
server <- function(input, output) {
  
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
    bindEvent(input$calc_prob)
  
  barPlot <- reactive({
    if(values$escoreT == 0) {
      ggplot(data.frame(score = "Escore-T", n = 0:100), aes(x = score, y = n)) +
        geom_blank()
    } else(
      ggplot(data.frame(score = "Escore-T", n = values$escoreT), aes(x = score, y = n)) +
        geom_bar(stat = "identity") +
        ylim(0,100)
    )
  })
  
  forestPlot <- reactive({
    
    post_odds <- input$prev / (100-input$prev) * values$df_prob[['likelihood_ratio']] 
    post_prob <- post_odds / (1 + post_odds)
    
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = post_prob * 100,
      number = list(suffix = "%"),
      title = list(text = "Probabilidade pós-teste"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = input$prev, increasing = list(color = "red"), 
                   decreasing = list(color = "forestgreen")),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        bar = list(color = "darkblue"),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = input$prev))) |> 
      layout(margin = list(l=20,r=30))
  }) |> 
    bindEvent(input$calc_prob)
  
  output$barplot <- renderPlot({
    barPlot()
  }) 
  
  output$forestplot <- renderPlotly({
    forestPlot()
  })
  
  output$show_score_bar <- renderText({paste("O valor do Escore T é: ", round(score(),2))})
  
  output$show_score_gauge <- renderText({paste("O valor do Escore T é: ", round(score_2(),2))})
  
  
  observe({
    if(values$escoreT >= 40) {
      values$df_prob <- diagnostic_measures |> 
        filter(disorder == input$transt, values$escoreT >= low_coff, values$escoreT <= high_coff)
    } else {
      values$df_prob <- diagnostic_measures |> 
        filter(disorder == input$transt) |> 
        head(1)
    }
    
    updateNumericInput(
      session = getDefaultReactiveDomain(), 
      "prev", 
      value = values$df_prob[['pre_test_prob']] * 100
    )
  })
}

shinyApp(ui = ui, server = server)