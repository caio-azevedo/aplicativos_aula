library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Teste t - Prof. Caio Azevedo"),
  fluidRow(
    column(width = 4,
           wellPanel(
             # Substituir numericInput por textInput para aceitar vírgula
             textInput("mu0", "Média populacional (H0):", value = "", placeholder = "ex: 100,0"),
             textInput("xbar", "Média amostral:", value = "", placeholder = "ex: 105,2"),
             textInput("s", "Desvio padrão amostral:", value = "", placeholder = "ex: 15,3"),
             textInput("n", "Tamanho da amostra:", value = "", placeholder = "ex: 30"),
             numericInput("alpha", "Nível de significância (0-1):", 
                          value = 0.05, min = 0, max = 1, step = 0.01),
             selectInput("testType", "Tipo de teste:", 
                         choices = c("Bilateral" = "two.sided", 
                                     "Unilateral à direita (maior)" = "greater", 
                                     "Unilateral à esquerda (menor)" = "less")),
             actionButton("calculate", "Calcular")
           )
    ),
    column(width = 8,
           plotOutput("plot", height = "500px")
    )
  ),
  fluidRow(
    column(width = 12,
           h3("Resultados do Teste t", align = "center"),
           div(
             textOutput("results", 
                        container = function(...) {
                          tags$pre(style = "font-size: 24px; font-weight: bold;", ...)
                        }
             )
           )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    # Converter entradas de texto para números, substituindo vírgula por ponto
    mu0_input <- as.numeric(gsub(",", ".", input$mu0))
    xbar_input <- as.numeric(gsub(",", ".", input$xbar))
    s_input <- as.numeric(gsub(",", ".", input$s))
    n_input <- as.numeric(gsub(",", ".", input$n))
    
    # Verifica se as conversões foram bem-sucedidas
    req(!is.na(mu0_input), !is.na(xbar_input), !is.na(s_input), !is.na(n_input))
    
    mu0 <- mu0_input
    xbar <- xbar_input
    s <- s_input
    n <- n_input
    alpha <- input$alpha
    testType <- input$testType
    
    df_val <- n - 1  # graus de liberdade para o teste t
    
    # Cálculo da estatística t
    t_stat <- (xbar - mu0) / (s / sqrt(n))
    
    # Determinação do valor crítico e p-valor usando a distribuição t
    if (testType == "two.sided") {
      t_crit <- qt(1 - alpha/2, df = df_val)
      p_value <- 2 * (1 - pt(abs(t_stat), df = df_val))
    } else if (testType == "greater") {
      t_crit <- qt(1 - alpha, df = df_val)
      p_value <- 1 - pt(t_stat, df = df_val)
    } else { # "less"
      t_crit <- qt(alpha, df = df_val)
      p_value <- pt(t_stat, df = df_val)
    }
    
    reject_null <- ifelse(
      (testType == "two.sided" && abs(t_stat) > t_crit) ||
        (testType == "greater"   && t_stat > t_crit)       ||
        (testType == "less"      && t_stat < t_crit),
      "Rejeitar H0", "Não rejeitar H0")
    
    output$results <- renderText({
      paste0("Estatística t: ", round(t_stat, 4), "\n",
             "Valor-p: ", round(p_value, 4), "\n",
             "Valor crítico: ±", round(t_crit, 4), "\n",
             "Decisão: ", reject_null)
    })
    
    x_min <- min(-4, t_stat - 1)
    x_max <- max(4, t_stat + 1)
    x <- seq(x_min, x_max, length.out = 2000)
    y <- dt(x, df = df_val)  # função de densidade t
    df <- data.frame(x = x, y = y)
    
    if (testType == "two.sided") {
      p_area_left  <- subset(df, x <= -abs(t_stat))
      p_area_right <- subset(df, x >= abs(t_stat))
    } else if (testType == "greater") {
      p_area <- subset(df, x >= t_stat)
    } else {
      p_area <- subset(df, x <= t_stat)
    }
    
    getCritRects <- function(testType, t_crit) {
      if (testType == "two.sided") {
        data.frame(
          xmin = c(x_min,  t_crit),
          xmax = c(-t_crit, x_max),
          ymin = c(-0.04, -0.04),
          ymax = c(-0.02, -0.02),
          label_x = c((x_min + -t_crit)/2, (t_crit + x_max)/2)
        )
      } else if (testType == "greater") {
        data.frame(
          xmin = t_crit,
          xmax = x_max,
          ymin = -0.04,
          ymax = -0.02,
          label_x = (t_crit + x_max)/2
        )
      } else {
        data.frame(
          xmin = x_min,
          xmax = t_crit,
          ymin = -0.04,
          ymax = -0.02,
          label_x = (x_min + t_crit)/2
        )
      }
    }
    
    critData <- getCritRects(testType, t_crit)
    
    output$plot <- renderPlot({
      if(input$calculate < 1) {
        ggplot() + 
          theme_minimal() + 
          labs(title = "Aguardando cálculo...", 
               x = "Estatística t", y = "Densidade")
      } else {
        p <- ggplot(df, aes(x, y)) +
          geom_line() +
          geom_vline(xintercept = t_stat, color = "blue", linetype = "solid") +
          geom_vline(xintercept = if (testType == "two.sided") c(-t_crit, t_crit) else t_crit,
                     linetype = "dashed", color = "red") +
          geom_rect(
            data = critData,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.3,
            inherit.aes = FALSE
          ) +
          geom_text(
            data = critData,
            aes(x = label_x, y = (ymin + ymax) / 2, label = "Região Crítica"),
            color = "red", size = 6, inherit.aes = FALSE
          ) +
          labs(
            title = "Distribuição t de Student",
            x = "Estatística t", y = "Densidade",
            caption = "Área em azul: p-valor | Linha azul: Estatística t | Região Crítica: barras vermelhas abaixo do eixo"
          ) +
          theme_minimal() +
          theme(
            plot.caption = element_text(hjust = 0.5, face = "italic"),
            axis.title.x = element_text(vjust = -1),
            text = element_text(size = 16),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 20, face = "bold")
          )
        
        if (testType == "two.sided") {
          p <- p +
            geom_area(data = p_area_left, aes(x = x, y = y), fill = "blue", alpha = 0.4) +
            geom_area(data = p_area_right, aes(x = x, y = y), fill = "blue", alpha = 0.4)
        } else {
          p <- p + geom_area(data = p_area, aes(x = x, y = y), fill = "blue", alpha = 0.4)
        }
        
        p
      }
    })
  })
}

shinyApp(ui, server)

