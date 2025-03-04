library(shiny)
library(ggplot2)

ui <- fluidPage(
  # CSS para aumentar fonte de alpha e beta
  tags$style(HTML("
    #alpha {
      font-size: 20px;    
      font-weight: bold;  
    }
    #beta {
      font-size: 20px;
      font-weight: bold;
    }
  ")),
  
  # 1) Primeira linha: gráfico
  fluidRow(
    column(
      width = 12,
      plotOutput("distPlot", height = "600px")
    )
  ),
  
  # 2) Segunda linha: inputs e outputs
  fluidRow(
    column(
      width = 8,
      wellPanel(
        # Nova linha para alinhar os inputs horizontalmente
        fluidRow(
          # Seleção de País
          column(
            width = 4,
            radioButtons(
              inputId = "qualPlot",
              label   = "Qual país?",
              choices = c("País A" = "A", 
                          "País B" = "B",
                          "Ambos"  = "AB"),
              selected = "A"
            )
          ),
          
          # Checkbox para calcular probabilidades dos erros tipo I e II
          column(
            width = 4,
            checkboxInput(
              inputId = "mostrarRegra",
              label   = "Calcular as probabilidades dos erros tipo I e II?",
              value   = FALSE
            )
          ),
          
          # Checkbox para aplicar regra de decisão
          column(
            width = 4,
            checkboxInput(
              inputId = "mostrarLinha150",
              label   = "Aplicar regras de decisão?",
              value   = FALSE
            )
          )
        ),
        
        # Slider do cutoff
        sliderInput(
          "cutoff", 
          label = HTML("<span style='font-size:15px; font-weight:bold;'>
                          Regra de decisão (c): rejeitar a hipótese nula se 
                          <span style='text-decoration: overline;'>x</span> < c
                        </span>"),
          min = 140, max = 160, value = 150, step = 0.25
        ),
        
        # Textos para Erro Tipo I e II
        textOutput("alpha"),
        textOutput("beta")
      )
    ),
    column(
      width = 4,
      tags$div(
        style = "font-size: 20px;",
        h4("Informações adicionais:"),
        helpText("H0: Lote do país B"),
        helpText("H1: Lote do país A"),
        helpText("Amostral: B ~ N(155,4) e A ~ N(145,2.4), com n = 25"),
        br()
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive para aplicar a regra quando "AB", "A" ou "B" estiverem selecionados e a checkbox estiver marcada
  aplicarRegra <- reactive({
    (input$qualPlot %in% c("AB", "A", "B")) && input$mostrarRegra
  })
  
  output$distPlot <- renderPlot({
    
    # Define parâmetros para distribuição amostral
    meanB <- 155
    sdB <- 4  # 20 / sqrt(25)
    meanA <- 145
    sdA <- 2.4  # 12 / sqrt(25)
    x <- seq(120, 180, length.out = 500)
    
    # Densidades
    dB <- dnorm(x, meanB, sdB)  # H0
    dA <- dnorm(x, meanA, sdA)  # H1
    df <- data.frame(x = x, h0 = dB, h1 = dA)
    
    max_dens <- max(dB, dA)
    c <- input$cutoff
    
    # Rótulo do eixo X
    x_label <- expression(bar(X))  # x barra
    
    g <- ggplot(df, aes(x = x)) +
      coord_cartesian(ylim = c(0, 1.05 * max_dens)) +
      theme_classic(base_size = 20) +
      labs(x = x_label, y = "Densidade")
    
    min_x <- floor(min(x) / 10) * 10
    max_x <- ceiling(max(x) / 10) * 10
    g <- g + scale_x_continuous(
      breaks = seq(min_x, max_x, by = 10)
    )
    
    # Picos para interceptos
    peakB <- dnorm(meanB, meanB, sdB)
    peakA <- dnorm(meanA, meanA, sdA)
    
    # Qual distribuição?
    if (input$qualPlot == "A") {
      
      # Apenas A
      g <- g + 
        geom_line(aes(y = h1), color = "red", size = 2) +
        labs(title = "Apenas País A") +
        geom_segment(
          x = meanA, y = 0,
          xend = meanA, yend = peakA,
          color = "red",
          linetype = "dashed",
          size = 1
        )
      
      # Se "A" e "mostrarRegra" marcado => sombrear Erro II
      if (input$mostrarRegra) {
        g <- g +
          geom_area(
            data = subset(df, x >= c),
            aes(y = h1, fill = "β = P(Erro II)"),
            alpha = 0.3
          ) +
          geom_vline(xintercept = c, linetype = "dashed", size = 2) +
          scale_fill_manual(
            name   = "Regiões de Erro",
            values = c("β = P(Erro II)" = "red")
          ) +
          annotate(
            "text", 
            x = c + 3, 
            y = 1.05 * max_dens,
            label = paste("c =", round(c, 2)),
            color = "green4",
            size = 7
          )
      }
      
    } else if (input$qualPlot == "B") {
      
      # Apenas B
      g <- g +
        geom_line(aes(y = h0), color = "blue", size = 2) +
        labs(title = "Apenas País B") +
        geom_segment(
          x = meanB, y = 0,
          xend = meanB, yend = peakB,
          color = "blue",
          linetype = "dashed",
          size = 1
        )
      
      # Se "B" e "mostrarRegra" marcado => sombrear Erro I
      if (input$mostrarRegra) {
        g <- g +
          geom_area(
            data = subset(df, x < c),
            aes(y = h0, fill = "α = P(Erro I)"),
            alpha = 0.3
          ) +
          geom_vline(xintercept = c, linetype = "dashed", size = 2) +
          scale_fill_manual(
            name   = "Regiões de Erro",
            values = c("α = P(Erro I)" = "blue")
          ) +
          annotate(
            "text", 
            x = c - 10,  # Ajuste a posição do texto conforme necessário
            y = 1.05 * max_dens,
            label = paste("c =", round(c, 2)),
            color = "green4",
            size = 7
          )
      }
      
    } else {
      
      # Ambos
      g <- g +
        geom_line(aes(y = h0, color = "País B"), size = 2) +
        geom_line(aes(y = h1, color = "País A"), size = 2) +
        scale_color_manual(
          name = "Distribuições",
          values = c("País B" = "blue", "País A" = "red")
        ) +
        labs(title = "País A e País B") +
        
        # Interceptos
        geom_segment(
          x = meanB, y = 0,
          xend = meanB, yend = peakB,
          color = "blue",
          linetype = "dashed",
          size = 1
        ) +
        geom_segment(
          x = meanA, y = 0,
          xend = meanA, yend = peakA,
          color = "red",
          linetype = "dashed",
          size = 1
        )
      
      # Se "Ambos" e "mostrarRegra" marcado => sombrear Erro I e II
      if (aplicarRegra()) {
        g <- g +
          geom_area(
            data = subset(df, x < c),
            aes(y = h0, fill = "α = P(Erro I)"),
            alpha = 0.3
          ) +
          geom_area(
            data = subset(df, x >= c),
            aes(y = h1, fill = "β = P(Erro II)"),
            alpha = 0.3
          ) +
          geom_vline(xintercept = c, linetype = "dashed", size = 2) +
          scale_fill_manual(
            name   = "Regiões de Erro",
            values = c("α = P(Erro I)" = "blue", 
                       "β = P(Erro II)" = "red")
          ) +
          annotate(
            "text", 
            x = c + 3, 
            y = 1.05 * max_dens,
            label = paste("c =", round(c, 2)),
            color = "green4",
            size = 7
          )
      }
    }
    
    g
  })
  
  # Erro Tipo I: Para seleção "AB" e "B"
  output$alpha <- renderText({
    if (input$qualPlot %in% c("AB", "B") && input$mostrarRegra) {
      c <- input$cutoff
      alpha_val <- pnorm(c, mean = 155, sd = 4) 
      paste0("P(Erro Tipo I) = α = ", round(alpha_val, 4))
    } else {
      "P(Erro Tipo I) = α = --"
    }
  })
  
  # Erro Tipo II: Para seleção "AB" e "A"
  output$beta <- renderText({
    if (input$qualPlot %in% c("AB", "A") && input$mostrarRegra) {
      c <- input$cutoff
      beta_val <- 1 - pnorm(c, mean = 145, sd = 2.4)
      paste0("P(Erro Tipo II) = β = ", round(beta_val, 4))
    } else {
      "P(Erro Tipo II) = β = --"
    }
  })
}

shinyApp(ui, server)