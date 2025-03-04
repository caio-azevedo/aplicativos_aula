library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Teste Z - Prof. Caio Azevedo"),
  fluidRow(
    column(width = 4,
           wellPanel(
             # Utiliza textInput para aceitar vírgula como separador decimal
             textInput("mu0", "Média populacional (H0):", value = "", placeholder = "ex: 100,0"),
             textInput("xbar", "Média amostral:", value = "", placeholder = "ex: 105,2"),
             textInput("sigma", "Desvio padrão populacional:", value = "", placeholder = "ex: 15,3"),
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
           h3("Resultados do Teste Z", align = "center"),
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
    sigma_input <- as.numeric(gsub(",", ".", input$sigma))
    n_input <- as.numeric(gsub(",", ".", input$n))
    
    # Verificar se as conversões foram bem-sucedidas
    req(!is.na(mu0_input), !is.na(xbar_input), !is.na(sigma_input), !is.na(n_input))
    
    mu0 <- mu0_input
    xbar <- xbar_input
    sigma <- sigma_input
    n <- n_input
    alpha <- input$alpha
    testType <- input$testType
    
    z_stat <- (xbar - mu0) / (sigma / sqrt(n))
    
    if (testType == "two.sided") {
      z_crit <- qnorm(1 - alpha/2)
      p_value <- 2 * (1 - pnorm(abs(z_stat)))
    } else if (testType == "greater") {
      z_crit <- qnorm(1 - alpha)
      p_value <- 1 - pnorm(z_stat)
    } else {
      z_crit <- qnorm(alpha)
      p_value <- pnorm(z_stat)
    }
    
    reject_null <- ifelse(
      (testType == "two.sided"  && abs(z_stat) > z_crit) ||
        (testType == "greater"    && z_stat > z_crit)       ||
        (testType == "less"       && z_stat < z_crit),
      "Rejeitar H0", "Não rejeitar H0")
    
    output$results <- renderText({
      paste0("Estatística Z: ", round(z_stat, 4), "\n",
             "Valor-p: ", round(p_value, 4), "\n",
             "Valor crítico: ±", round(z_crit, 4), "\n",
             "Decisão: ", reject_null)
    })
    
    x_min <- min(-4, z_stat - 1)
    x_max <- max(4, z_stat + 1)
    x <- seq(x_min, x_max, length.out = 2000)
    y <- dnorm(x)
    df <- data.frame(x = x, y = y)
    
    if (testType == "two.sided") {
      p_area_left  <- subset(df, x <= -abs(z_stat))
      p_area_right <- subset(df, x >= abs(z_stat))
    } else if (testType == "greater") {
      p_area <- subset(df, x >= z_stat)
    } else {
      p_area <- subset(df, x <= z_stat)
    }
    
    getCritRects <- function(testType, z_crit) {
      if (testType == "two.sided") {
        data.frame(
          xmin = c(x_min,  z_crit),
          xmax = c(-z_crit, x_max),
          ymin = c(-0.04, -0.04),
          ymax = c(-0.02, -0.02),
          label_x = c((x_min + -z_crit)/2, (z_crit + x_max)/2)
        )
      } else if (testType == "greater") {
        data.frame(
          xmin = z_crit,
          xmax = x_max,
          ymin = -0.04,
          ymax = -0.02,
          label_x = (z_crit + x_max)/2
        )
      } else {
        data.frame(
          xmin = x_min,
          xmax = z_crit,
          ymin = -0.04,
          ymax = -0.02,
          label_x = (x_min + z_crit)/2
        )
      }
    }
    
    critData <- getCritRects(testType, z_crit)
    
    output$plot <- renderPlot({
      if(input$calculate < 1) {
        ggplot() + 
          theme_minimal() + 
          labs(title = "Aguardando cálculo...", 
               x = "Estatística Z", y = "Densidade")
      } else {
        p <- ggplot(df, aes(x, y)) +
          geom_line() +
          geom_vline(xintercept = z_stat, color = "blue", linetype = "solid") +
          geom_vline(xintercept = if (testType == "two.sided") c(-z_crit, z_crit) else z_crit,
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
            title = "Distribuição Normal Padrão",
            x = "Estatística Z", y = "Densidade",
            caption = "Área em azul: p-valor | Linha azul: Estatística Z | Região Crítica: barras vermelhas abaixo do eixo"
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
