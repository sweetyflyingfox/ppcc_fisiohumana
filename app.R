# ============================================================
# PPCC Fisiologia Humana
# Simulador qualitativo de mergulho e descompressão
# ============================================================

library(shiny)


# ============================================================
# CONFIGURAÇÕES GERAIS
# ============================================================

NIVEL_MAXIMO <- 10# PPCC Fisiologia Humana
# Simulador qualitativo de mergulho e descompressão

library(shiny)

# Configuração
NIVEL_MAXIMO <- 10

# Ajustes visuais
# Controlam apenas a quantidade de símbolos exibidos na tela.
# Não representam concentrações reais.
N2_DESCIDA <- seq(from = 2, to = 22, by = 2)
N2_SUBIDA_LENTA <- seq(from = 2, to = 22, by = 2)

# Valores representativos para subida rápida
N2_RAPIDA_NIVEL_10 <- 22
N2_RAPIDA_NIVEL_5 <- 20
N2_RAPIDA_NIVEL_0 <- 12

# Bolhas no tecido e na circulação (representação)
BOLHAS_TECIDO_NIVEL_10 <- 0
BOLHAS_TECIDO_NIVEL_5 <- 8
BOLHAS_TECIDO_NIVEL_0 <- 12

BOLHAS_VASO_NIVEL_10 <- 0
BOLHAS_VASO_NIVEL_5 <- 8
BOLHAS_VASO_NIVEL_0 <- 12

# Posição vertical do mergulhador (conversão de nível -> posição em %)
calcular_y <- function(nivel) {
  proporcao <- nivel / NIVEL_MAXIMO
  8 + 74 * proporcao
}

# Posição horizontal para os diferentes movimentos
calcular_x_descida <- function(nivel) {
  proporcao <- nivel / NIVEL_MAXIMO
  50 - 30 * sqrt(max(0, 1 - proporcao))
}

calcular_x_lenta <- function(nivel) {
  proporcao <- nivel / NIVEL_MAXIMO
  50 + 30 * sqrt(max(0, 1 - proporcao))
}

calcular_x_rapida <- function(nivel) {
  proporcao <- nivel / NIVEL_MAXIMO
  50 + 17 * sqrt(max(0, 1 - proporcao))
}

# Trajetórias (pontos que desenham os caminhos)
criar_trajetoria_descida <- function() {
  niveis <- seq(0, 10, length.out = 60)
  tagList(
    lapply(niveis, function(n) {
      div(
        class = "ponto-trajetoria descida",
        style = paste0("left:", calcular_x_descida(n), "%;", "top:", calcular_y(n), "%;")
      )
    })
  )
}

criar_trajetoria_lenta <- function() {
  niveis <- seq(0, 10, length.out = 60)
  tagList(
    lapply(niveis, function(n) {
      div(
        class = "ponto-trajetoria lenta",
        style = paste0("left:", calcular_x_lenta(n), "%;", "top:", calcular_y(n), "%;")
      )
    })
  )
}

criar_trajetoria_rapida <- function() {
  niveis <- seq(0, 10, length.out = 45)
  tagList(
    lapply(niveis, function(n) {
      div(
        class = "ponto-trajetoria rapida",
        style = paste0("left:", calcular_x_rapida(n), "%;", "top:", calcular_y(n), "%;")
      )
    })
  )
}

# Linhas dos níveis (rótulos laterais)
criar_linhas_niveis <- function() {
  tagList(
    lapply(0:NIVEL_MAXIMO, function(n) {
      div(
        class = "linha-nivel",
        style = paste0("top:", calcular_y(n), "%;"),
        span(class = "rotulo-nivel", n)
      )
    })
  )
}

# Posições das partículas e bolhas (coordenadas para a representação)
posicoes_n2 <- data.frame(
  left = c(7, 15, 23, 31, 39, 47, 55, 63, 71, 79, 87, 10, 18, 26, 34, 42, 50, 58, 66, 74, 82, 90),
  top = c(18, 29, 16, 34, 21, 31, 17, 35, 20, 32, 18, 78, 68, 81, 70, 79, 67, 80, 69, 78, 66, 76)
)

posicoes_bolhas_tecido <- data.frame(
  left = c(12, 24, 37, 51, 65, 78, 88, 18, 32, 47, 63, 81),
  top = c(24, 37, 19, 35, 23, 38, 21, 72, 82, 70, 80, 71)
)

posicoes_bolhas_vaso <- c(5, 13, 21, 29, 37, 45, 55, 63, 71, 79, 87, 95)

# Monta o painel que mostra N2 e bolhas (tecido e vaso)
criar_painel_fisiologico <- function(n2, bolhas_tecido = 0, bolhas_vaso = 0) {
  elementos_n2 <- lapply(seq_len(n2), function(i) {
    div(class = "particula-n2", style = paste0("left:", posicoes_n2$left[i], "%;", "top:", posicoes_n2$top[i], "%;"))
  })
  
  elementos_tecido <- NULL
  if (bolhas_tecido > 0) {
    elementos_tecido <- lapply(seq_len(bolhas_tecido), function(i) {
      div(class = "bolha-tecido", style = paste0("left:", posicoes_bolhas_tecido$left[i], "%;", "top:", posicoes_bolhas_tecido$top[i], "%;"))
    })
  }
  
  elementos_vaso <- NULL
  if (bolhas_vaso > 0) {
    elementos_vaso <- lapply(seq_len(bolhas_vaso), function(i) {
      div(class = "bolha-vaso", style = paste0("left:", posicoes_bolhas_vaso[i], "%;"))
    })
  }
  
  div(
    class = "painel-fisiologico",
    div(class = "rotulo-tecido", "Tecido"),
    tagList(elementos_n2),
    tagList(elementos_tecido),
    div(
      class = "vaso",
      span(class = "rotulo-vaso", "Ccirculação venosa"),
      tagList(elementos_vaso)
    )
  )
}

# Interface (UI)
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
/* (estilos CSS omitidos aqui, mantidos inalterados) */
      ")
    )
  ),
  
  div(class = "titulo-app", "Mergulho, pressão e nitrogênio"),
  
  fluidRow(
    column(width = 7, uiOutput("cena")),
    column(
      width = 5,
      div(
        class = "painel",
        h4("Movimento do mergulhador"),
        uiOutput("controles"),
        uiOutput("estado"),
        h4("N₂ nos tecidos e na circulação"),
        div(
          class = "legenda",
          div(class = "item-legenda", div(class = "simbolo-n2"), "N₂ dissolvido"),
          div(class = "item-legenda", div(class = "simbolo-tecido"), "Bolha no tecido"),
          div(class = "item-legenda", div(class = "simbolo-vaso"), "Bolha no vaso")
        ),
        uiOutput("painel_fisiologico"),
        uiOutput("alerta"),
        div(class = "interpretacao", textOutput("interpretacao")),
        div(class = "aviso", paste("PPCC Fisiologia Humana 26.2 - Daisy e Sofia B."))
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Estado da simulação
  nivel <- reactiveVal(0)
  
  # Fases: descida, fundo, subida_lenta, superficie_lenta,
  #        subida_rapida_meio, subida_rapida_superficie
  fase <- reactiveVal("descida")
  
  # Descida: avança um nível até o máximo
  observeEvent(input$descer, {
    if (fase() == "descida") {
      novo_nivel <- min(nivel() + 1, NIVEL_MAXIMO)
      nivel(novo_nivel)
      if (novo_nivel == NIVEL_MAXIMO) fase("fundo")
    }
  })
  
  # Escolher subida devagar (quando estiver no fundo)
  observeEvent(input$escolher_lenta, {
    if (fase() == "fundo") fase("subida_lenta")
  })
  
  # Continuar subida devagar
  observeEvent(input$subir_lenta, {
    if (fase() == "subida_lenta") {
      novo_nivel <- max(nivel() - 1, 0)
      nivel(novo_nivel)
      if (novo_nivel == 0) fase("superficie_lenta")
    }
  })
  
  # Escolher subida rápida (do nível 10 para 5)
  observeEvent(input$escolher_rapida, {
    if (fase() == "fundo") {
      nivel(5)
      fase("subida_rapida_meio")
    }
  })
  
  # Continuar subida rápida (do nível 5 para superfície)
  observeEvent(input$continuar_rapida, {
    if (fase() == "subida_rapida_meio") {
      nivel(0)
      fase("subida_rapida_superficie")
    }
  })
  
  # Reiniciar simulação
  observeEvent(input$reiniciar, {
    nivel(0)
    fase("descida")
  })
  
  # Renderiza os controles dependendo da fase atual
  output$controles <- renderUI({
    if (fase() == "descida") {
      return(actionButton(inputId = "descer", label = "↓ Descer um nível", class = "btn-primary botao-grande"))
    }
    if (fase() == "fundo") {
      return(tagList(
        div(style = "margin-bottom:8px; font-weight:600;", "Escolha como o mergulhador vai subir:"),
        div(class = "controles",
            actionButton(inputId = "escolher_lenta", label = "↑ Subir devagar", class = "btn-success"),
            actionButton(inputId = "escolher_rapida", label = "↑↑ Subir rápido", class = "btn-warning"))
      ))
    }
    if (fase() == "subida_lenta") {
      return(actionButton(inputId = "subir_lenta", label = "↑ Continuar subida devagar", class = "btn-success botao-grande"))
    }
    if (fase() == "subida_rapida_meio") {
      return(tagList(
        div(class = "alerta", paste("Parada no nível 5:", "observe o tecido e a circulação.")),
        actionButton(inputId = "continuar_rapida", label = "↑↑ Continuar subida rápida", class = "btn-warning botao-grande")
      ))
    }
    actionButton(inputId = "reiniciar", label = "↺ Reiniciar mergulho", class = "btn-primary botao-grande")
  })
  
  # Posição atual do mergulhador (x, y) para renderização
  posicao_atual <- reactive({
    y <- calcular_y(nivel())
    if (fase() %in% c("descida", "fundo")) {
      x <- calcular_x_descida(nivel())
    } else if (fase() %in% c("subida_lenta", "superficie_lenta")) {
      x <- calcular_x_lenta(nivel())
    } else {
      x <- calcular_x_rapida(nivel())
    }
    list(x = x, y = y)
  })
  
  # Cena principal (desenha mergulhador, rotas, linhas, etc.)
  output$cena <- renderUI({
    p <- posicao_atual()
    texto_movimento <- switch(
      fase(),
      descida = "Descendo ↓",
      fundo = "Fundo",
      subida_lenta = "Subida devagar ↑",
      superficie_lenta = "Superfície",
      subida_rapida_meio = "Subida rápida ↑↑",
      subida_rapida_superficie = "Superfície"
    )
    mostrar_rotas <- fase() != "descida"
    
    div(
      class = "cena",
      div(class = "superficie"),
      criar_linhas_niveis(),
      criar_trajetoria_descida(),
      if (mostrar_rotas) {
        tagList(criar_trajetoria_lenta(), criar_trajetoria_rapida(),
                div(class = "rotulo-caminho rotulo-lenta", "Subida devagar"),
                div(class = "rotulo-caminho rotulo-rapida", "Subida rápida"))
      },
      tags$img(src = "mergulhador.png", class = "mergulhador", style = paste0("left:", p$x, "%;", "top:", p$y, "%;")),
      div(class = "direcao", style = paste0("left:", min(90, p$x + 14), "%;", "top:", max(4, p$y - 5), "%;"), texto_movimento),
      div(class = "fundo")
    )
  })
  
  # Estado exibido no painel lateral
  output$estado <- renderUI({
    caminho <- switch(
      fase(),
      descida = "Descida",
      fundo = "Ponto mais profundo",
      subida_lenta = "Subida devagar",
      superficie_lenta = "Subida devagar concluída",
      subida_rapida_meio = "Subida rápida",
      subida_rapida_superficie = "Subida rápida concluída"
    )
    div(class = "estado",
        div(class = "estado-principal", paste0("Nível ", nivel(), " de 10")),
        div(class = "estado-secundario", caminho)
    )
  })
  
  # Calcula o estado fisiológico a partir da fase atual
  estado_fisiologico <- reactive({
    if (fase() == "descida") {
      return(list(n2 = N2_DESCIDA[nivel() + 1], tecido = 0, vaso = 0))
    }
    if (fase() == "fundo") {
      return(list(n2 = N2_RAPIDA_NIVEL_10, tecido = BOLHAS_TECIDO_NIVEL_10, vaso = BOLHAS_VASO_NIVEL_10))
    }
    if (fase() %in% c("subida_lenta", "superficie_lenta")) {
      return(list(n2 = N2_SUBIDA_LENTA[nivel() + 1], tecido = 0, vaso = 0))
    }
    if (fase() == "subida_rapida_meio") {
      return(list(n2 = N2_RAPIDA_NIVEL_5, tecido = BOLHAS_TECIDO_NIVEL_5, vaso = BOLHAS_VASO_NIVEL_5))
    }
    list(n2 = N2_RAPIDA_NIVEL_0, tecido = BOLHAS_TECIDO_NIVEL_0, vaso = BOLHAS_VASO_NIVEL_0)
  })
  
  output$painel_fisiologico <- renderUI({
    e <- estado_fisiologico()
    criar_painel_fisiologico(n2 = e$n2, bolhas_tecido = e$tecido, bolhas_vaso = e$vaso)
  })
  
  # Alertas dependendo da fase
  output$alerta <- renderUI({
    if (fase() == "subida_rapida_meio") {
      return(div(class = "alerta", paste("No nível 5 após uma subida rápida,", "representamos bolhas tanto no tecido", "quanto na circulação.")))
    }
    if (fase() == "subida_rapida_superficie") {
      return(div(class = "alerta", paste("Após a subida rápida até a superfície,", "a representação mostra uma quantidade", "ainda maior de bolhas.")))
    }
    NULL
  })
  
  # Texto de interpretação para o usuário
  output$interpretacao <- renderText({
    if (fase() == "descida") {
      return(paste("Durante a descida, a pressão aumenta.", "A cada nível acrescentamos duas partículas de N₂", "para representar qualitativamente o aumento do", "nitrogênio dissolvido no organismo."))
    }
    if (fase() == "fundo") {
      return(paste("No nível 10 representamos a maior quantidade", "de N₂ dissolvido.", "Agora escolha uma subida devagar ou rápida", "para comparar as duas situações."))
    }
    if (fase() == "subida_lenta") {
      return(paste("Durante a subida devagar, diminuímos", "progressivamente a quantidade de N₂ dissolvido.", "Nesta representação simplificada não mostramos", "formação de bolhas."))
    }
    if (fase() == "superficie_lenta") {
      return(paste("O mergulhador chegou à superfície após uma", "subida gradual.", "A representação mostra pouca quantidade adicional", "de N₂ dissolvido e nenhuma bolha."))
    }
    if (fase() == "subida_rapida_meio") {
      return(paste("O mergulhador passou rapidamente do nível 10", "para o nível 5.", "Ainda representamos uma grande quantidade de N₂", "dissolvido e agora aparecem várias bolhas", "no tecido e dentro da circulação.", "Compare com o nível 5 durante a subida devagar."))
    }
    paste("O mergulhador chegou rapidamente à superfície.", "A representação mostra mais bolhas no tecido", "e na circulação, além de N₂ que ainda", "permanece representado como dissolvido.")
  })
}

shinyApp(ui = ui, server = server)



# ============================================================
# QUANTIDADES VISUAIS
# ============================================================
#
# Estes números controlam apenas a quantidade de símbolos
# apresentados na tela.
#
# Não correspondem a concentrações ou números reais de
# moléculas/bolhas.
# ============================================================


# ------------------------------------------------------------
# N2 durante a descida
#
# Nível 0  = 2 partículas
# Nível 1  = 4
# Nível 2  = 6
# ...
# Nível 10 = 22
# ------------------------------------------------------------

N2_DESCIDA <- seq(
  from = 2,
  to = 22,
  by = 2
)


# ------------------------------------------------------------
# N2 durante a subida devagar
#
# Ao diminuir o nível, diminui também a representação
# do N2 dissolvido.
# ------------------------------------------------------------

N2_SUBIDA_LENTA <- seq(
  from = 2,
  to = 22,
  by = 2
)


# ------------------------------------------------------------
# N2 durante a subida rápida
# ------------------------------------------------------------

N2_RAPIDA_NIVEL_10 <- 22

# Após a subida rápida de 10 para 5:
# ainda representamos grande quantidade de N2 dissolvido.
N2_RAPIDA_NIVEL_5 <- 20

# Após chegar rapidamente à superfície:
N2_RAPIDA_NIVEL_0 <- 12


# ============================================================
# BOLHAS NA SUBIDA RÁPIDA
# ============================================================


# ------------------------------------------------------------
# Bolhas extravasculares / no tecido
# ------------------------------------------------------------

BOLHAS_TECIDO_NIVEL_10 <- 0

BOLHAS_TECIDO_NIVEL_5 <- 8

BOLHAS_TECIDO_NIVEL_0 <- 12


# ------------------------------------------------------------
# Bolhas intravasculares / circulação
# ------------------------------------------------------------

BOLHAS_VASO_NIVEL_10 <- 0

BOLHAS_VASO_NIVEL_5 <- 8

BOLHAS_VASO_NIVEL_0 <- 12


# ============================================================
# POSIÇÃO VERTICAL
# ============================================================

calcular_y <- function(nivel) {
  
  proporcao <- nivel / NIVEL_MAXIMO
  
  8 + 74 * proporcao
}


# ============================================================
# POSIÇÃO HORIZONTAL - DESCIDA
# ============================================================

calcular_x_descida <- function(nivel) {
  
  proporcao <- nivel / NIVEL_MAXIMO
  
  50 -
    30 *
    sqrt(
      max(
        0,
        1 - proporcao
      )
    )
}


# ============================================================
# POSIÇÃO HORIZONTAL - SUBIDA DEVAGAR
# ============================================================

calcular_x_lenta <- function(nivel) {
  
  proporcao <- nivel / NIVEL_MAXIMO
  
  50 +
    30 *
    sqrt(
      max(
        0,
        1 - proporcao
      )
    )
}


# ============================================================
# POSIÇÃO HORIZONTAL - SUBIDA RÁPIDA
# ============================================================

calcular_x_rapida <- function(nivel) {
  
  proporcao <- nivel / NIVEL_MAXIMO
  
  50 +
    17 *
    sqrt(
      max(
        0,
        1 - proporcao
      )
    )
}


# ============================================================
# TRAJETÓRIA DA DESCIDA
# ============================================================

criar_trajetoria_descida <- function() {
  
  niveis <- seq(
    0,
    10,
    length.out = 60
  )
  
  
  tagList(
    
    lapply(
      
      niveis,
      
      function(n) {
        
        div(
          
          class = "ponto-trajetoria descida",
          
          style = paste0(
            
            "left:",
            calcular_x_descida(n),
            "%;",
            
            "top:",
            calcular_y(n),
            "%;"
          )
        )
      }
    )
  )
}


# ============================================================
# TRAJETÓRIA DA SUBIDA DEVAGAR
# ============================================================

criar_trajetoria_lenta <- function() {
  
  niveis <- seq(
    0,
    10,
    length.out = 60
  )
  
  
  tagList(
    
    lapply(
      
      niveis,
      
      function(n) {
        
        div(
          
          class = "ponto-trajetoria lenta",
          
          style = paste0(
            
            "left:",
            calcular_x_lenta(n),
            "%;",
            
            "top:",
            calcular_y(n),
            "%;"
          )
        )
      }
    )
  )
}


# ============================================================
# TRAJETÓRIA DA SUBIDA RÁPIDA
# ============================================================

criar_trajetoria_rapida <- function() {
  
  niveis <- seq(
    0,
    10,
    length.out = 45
  )
  
  
  tagList(
    
    lapply(
      
      niveis,
      
      function(n) {
        
        div(
          
          class = "ponto-trajetoria rapida",
          
          style = paste0(
            
            "left:",
            calcular_x_rapida(n),
            "%;",
            
            "top:",
            calcular_y(n),
            "%;"
          )
        )
      }
    )
  )
}


# ============================================================
# LINHAS DOS NÍVEIS
# ============================================================

criar_linhas_niveis <- function() {
  
  tagList(
    
    lapply(
      
      0:NIVEL_MAXIMO,
      
      function(n) {
        
        div(
          
          class = "linha-nivel",
          
          style = paste0(
            "top:",
            calcular_y(n),
            "%;"
          ),
          
          span(
            class = "rotulo-nivel",
            n
          )
        )
      }
    )
  )
}


# ============================================================
# POSIÇÕES DAS PARTÍCULAS DE N2
# ============================================================
#
# Há 22 posições disponíveis.
# As partículas ficam acima e abaixo do vaso.
# ============================================================

posicoes_n2 <- data.frame(
  
  left = c(
    7, 15, 23, 31, 39, 47, 55, 63, 71, 79, 87,
    10, 18, 26, 34, 42, 50, 58, 66, 74, 82, 90
  ),
  
  top = c(
    18, 29, 16, 34, 21, 31, 17, 35, 20, 32, 18,
    78, 68, 81, 70, 79, 67, 80, 69, 78, 66, 76
  )
)


# ============================================================
# POSIÇÕES DAS BOLHAS NO TECIDO
# ============================================================
#
# Há espaço para até 12 bolhas.
# ============================================================

posicoes_bolhas_tecido <- data.frame(
  
  left = c(
    12, 24, 37, 51, 65, 78,
    88, 18, 32, 47, 63, 81
  ),
  
  top = c(
    24, 37, 19, 35, 23, 38,
    21, 72, 82, 70, 80, 71
  )
)


# ============================================================
# POSIÇÕES DAS BOLHAS DENTRO DO VASO
# ============================================================

posicoes_bolhas_vaso <- c(
  5,
  13,
  21,
  29,
  37,
  45,
  55,
  63,
  71,
  79,
  87,
  95
)


# ============================================================
# PAINEL FISIOLÓGICO
# ============================================================

criar_painel_fisiologico <- function(
    n2,
    bolhas_tecido = 0,
    bolhas_vaso = 0
) {
  
  
  # ----------------------------------------------------------
  # Partículas de N2 dissolvido
  # ----------------------------------------------------------
  
  elementos_n2 <- lapply(
    
    seq_len(n2),
    
    function(i) {
      
      div(
        
        class = "particula-n2",
        
        style = paste0(
          
          "left:",
          posicoes_n2$left[i],
          "%;",
          
          "top:",
          posicoes_n2$top[i],
          "%;"
        )
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Bolhas no tecido
  # ----------------------------------------------------------
  
  elementos_tecido <- NULL
  
  
  if (bolhas_tecido > 0) {
    
    elementos_tecido <- lapply(
      
      seq_len(bolhas_tecido),
      
      function(i) {
        
        div(
          
          class = "bolha-tecido",
          
          style = paste0(
            
            "left:",
            posicoes_bolhas_tecido$left[i],
            "%;",
            
            "top:",
            posicoes_bolhas_tecido$top[i],
            "%;"
          )
        )
      }
    )
  }
  
  
  # ----------------------------------------------------------
  # Bolhas dentro do vaso
  # ----------------------------------------------------------
  
  elementos_vaso <- NULL
  
  
  if (bolhas_vaso > 0) {
    
    elementos_vaso <- lapply(
      
      seq_len(bolhas_vaso),
      
      function(i) {
        
        div(
          
          class = "bolha-vaso",
          
          style = paste0(
            
            "left:",
            posicoes_bolhas_vaso[i],
            "%;"
          )
        )
      }
    )
  }
  
  
  # ----------------------------------------------------------
  # Painel completo
  # ----------------------------------------------------------
  
  div(
    
    class = "painel-fisiologico",
    
    
    # Tecido
    div(
      class = "rotulo-tecido",
      "Tecido"
    ),
    
    
    # N2 dissolvido
    tagList(
      elementos_n2
    ),
    
    
    # Bolhas no tecido
    tagList(
      elementos_tecido
    ),
    
    
    # Circulação
    div(
      
      class = "vaso",
      
      
      span(
        class = "rotulo-vaso",
        "Ccirculação venosa"
      ),
      
      
      tagList(
        elementos_vaso
      )
    )
  )
}


# ============================================================
# INTERFACE
# ============================================================

ui <- fluidPage(
  
  
  tags$head(
    
    tags$style(
      
      HTML("


/* ==========================================================
   PÁGINA
   ========================================================== */

body {
  background-color: #f4f7fb;
}

.container-fluid {
  padding: 10px 18px;
}

.titulo-app {

  font-size: 27px;

  font-weight: 700;

  margin-bottom: 12px;
}


/* ==========================================================
   CENA DO MERGULHO
   ========================================================== */

.cena {

  position: relative;

  width: 100%;

  height: 620px;

  overflow: hidden;

  border-radius: 14px;

  border:
    1px solid #bfd0dc;

  background:
    linear-gradient(
      to bottom,
      #afe5f7 0%,
      #69bee0 25%,
      #2b83af 60%,
      #0c4d70 100%
    );
}


/* ==========================================================
   SUPERFÍCIE
   ========================================================== */

.superficie {

  position: absolute;

  top: 8%;

  left: 0;

  width: 100%;

  border-top:
    4px solid white;

  z-index: 2;
}


/* ==========================================================
   NÍVEIS
   ========================================================== */

.linha-nivel {

  position: absolute;

  left: 0;

  width: 100%;

  border-top:
    1px dashed
    rgba(255,255,255,0.20);

  z-index: 1;
}


.rotulo-nivel {

  position: absolute;

  left: 8px;

  top: -14px;

  color: white;

  font-size: 11px;

  font-weight: 600;

  background:
    rgba(0,0,0,0.18);

  padding:
    1px 5px;

  border-radius:
    4px;
}


/* ==========================================================
   TRAJETÓRIAS
   ========================================================== */

.ponto-trajetoria {

  position: absolute;

  width: 4px;

  height: 4px;

  border-radius: 50%;

  transform:
    translate(-50%, -50%);
}


/* Descida */

.ponto-trajetoria.descida {

  background:
    rgba(255,255,255,0.42);
}


/* Subida devagar */

.ponto-trajetoria.lenta {

  width: 5px;

  height: 5px;

  background:
    rgba(201,255,211,0.85);
}


/* Subida rápida */

.ponto-trajetoria.rapida {

  width: 5px;

  height: 5px;

  background:
    rgba(255,218,121,0.95);
}


/* ==========================================================
   RÓTULOS DOS CAMINHOS
   ========================================================== */

.rotulo-caminho {

  position: absolute;

  padding:
    4px 7px;

  border-radius:
    6px;

  font-size:
    11px;

  font-weight:
    700;

  background:
    rgba(255,255,255,0.90);

  z-index:
    5;
}


.rotulo-lenta {

  left: 78%;

  top: 13%;
}


.rotulo-rapida {

  left: 55%;

  top: 22%;
}


/* ==========================================================
   MERGULHADOR
   ========================================================== */

.mergulhador {

  position: absolute;

  width: 35%;

  max-width: 225px;

  min-width: 125px;

  height: auto;

  transform:
    translate(-50%, -50%);

  transition:
    left 0.55s ease,
    top 0.55s ease;

  z-index: 8;
}


/* ==========================================================
   TEXTO DO MOVIMENTO
   ========================================================== */

.direcao {

  position: absolute;

  transform:
    translate(-50%, -50%);

  background:
    rgba(255,255,255,0.93);

  padding:
    5px 8px;

  border-radius:
    7px;

  font-size:
    12px;

  font-weight:
    700;

  white-space:
    nowrap;

  z-index:
    10;
}


/* ==========================================================
   FUNDO DO MAR
   ========================================================== */

.fundo {

  position: absolute;

  bottom: 0;

  left: 0;

  width: 100%;

  height: 8%;

  background:
    rgba(76,55,40,0.78);

  z-index: 3;
}


/* ==========================================================
   PAINEL LATERAL
   ========================================================== */

.painel {

  height: 620px;

  background: white;

  border:
    1px solid #d9e2e8;

  border-radius:
    14px;

  padding:
    15px;

  overflow-y:
    auto;
}


/* ==========================================================
   CONTROLES
   ========================================================== */

.controles {

  display: flex;

  gap: 8px;

  margin-bottom: 12px;
}


.controles .btn {
  flex: 1;
}


.botao-grande {

  width: 100%;

  margin-bottom: 10px;
}


/* ==========================================================
   ESTADO
   ========================================================== */

.estado {

  background:
    #f6f9fb;

  border:
    1px solid #e3e9ed;

  border-radius:
    10px;

  padding:
    9px 12px;

  margin-bottom:
    14px;
}


.estado-principal {

  font-size:
    17px;

  font-weight:
    700;
}


.estado-secundario {

  margin-top:
    3px;

  font-size:
    13px;

  color:
    #61727d;
}


/* ==========================================================
   PAINEL FISIOLÓGICO
   ========================================================== */

.painel-fisiologico {

  position: relative;

  width: 100%;

  height: 210px;

  overflow: hidden;

  border:
    1px solid #cfa98e;

  border-radius:
    14px;

  background:
    #EDC9AF;

  margin-bottom:
    10px;
}


/* ==========================================================
   TECIDO
   ========================================================== */

.rotulo-tecido {

  position: absolute;

  top: 7px;

  left: 10px;

  font-size: 12px;

  font-weight: 700;

  color:
    #624b40;
}


/* ==========================================================
   N2 DISSOLVIDO
   ========================================================== */

.particula-n2 {

  position: absolute;

  width: 15px;

  height: 15px;

  border-radius:
    50%;

  background:
    #318ac2;

  border:
    1px solid #17658f;

  transform:
    translate(-50%, -50%);

  z-index:
    4;
}


/* ==========================================================
   BOLHAS NO TECIDO
   ========================================================== */

.bolha-tecido {

  position: absolute;

  width: 20px;

  height: 20px;

  border-radius:
    50%;

  border:
    2.5px solid #F39C12;

  background:
    #FFFFFF;

  transform:
    translate(-50%, -50%);

  z-index:
    5;
}


/* ==========================================================
   VASO / CIRCULAÇÃO
   ========================================================== */

.vaso {

  position: absolute;

  left: 7%;

  top: 45%;

  width: 86%;

  height: 38px;

  border-radius:
    25px;

  background:
    #F58B8B;

  border:
    3px solid #718eae;

  z-index:
    3;
}


.rotulo-vaso {

  position: absolute;

  left: 10px;

  top: -23px;

  font-size:
    11px;

  font-weight:
    600;

  color:
    #526b86;
}


/* ==========================================================
   BOLHAS NO VASO
   ========================================================== */

.bolha-vaso {

  position: absolute;

  top: 50%;

  width: 18px;

  height: 18px;

  border-radius:
    50%;

  border:
    2.5px solid #D85C72;

  background:
    #FFFFFF;

  transform:
    translate(-50%, -50%);

  z-index:
    6;
}


/* ==========================================================
   LEGENDA
   ========================================================== */

.legenda {

  display: flex;

  flex-wrap: wrap;

  gap: 12px;

  align-items: center;

  margin-bottom: 11px;

  font-size: 11px;

  color:
    #596a74;
}


.item-legenda {

  display: flex;

  align-items: center;

  gap: 5px;
}


/* N2 */

.simbolo-n2 {

  width: 13px;

  height: 13px;

  border-radius:
    50%;

  background:
    #318ac2;

  border:
    1px solid #17658f;
}


/* Bolha no tecido */

.simbolo-tecido {

  width: 16px;

  height: 16px;

  border-radius:
    50%;

  border:
    2px solid #F39C12;

  background:
    #FFFFFF;
}


/* Bolha no vaso */

.simbolo-vaso {

  width: 16px;

  height: 16px;

  border-radius:
    50%;

  border:
    2px solid #D85C72;

  background:
    #FFFFFF;
}


/* ==========================================================
   ALERTA
   ========================================================== */

.alerta {

  padding:
    8px 10px;

  border-radius:
    8px;

  background-color:
    #fff3cd;

  border:
    1px solid #ead59a;

  margin-bottom:
    10px;

  font-size:
    13px;

  font-weight:
    600;
}


/* ==========================================================
   INTERPRETAÇÃO
   ========================================================== */

.interpretacao {

  background:
    #f7f9fb;

  border:
    1px solid #e1e8ec;

  border-radius:
    10px;

  padding:
    9px 11px;

  font-size:
    13px;

  line-height:
    1.4;
}


/* ==========================================================
   AVISO
   ========================================================== */

.aviso {

  margin-top:
    8px;

  font-size:
    10px;

  color:
    #64737c;
}

      ")
    )
  ),
  
  
  # ==========================================================
  # TÍTULO
  # ==========================================================
  
  div(
    
    class =
      "titulo-app",
    
    "Mergulho, pressão e nitrogênio"
  ),
  
  
  fluidRow(
    
    
    # ========================================================
    # CENA
    # ========================================================
    
    column(
      
      width =
        7,
      
      uiOutput(
        "cena"
      )
    ),
    
    
    # ========================================================
    # PAINEL LATERAL
    # ========================================================
    
    column(
      
      width =
        5,
      
      
      div(
        
        class =
          "painel",
        
        
        h4(
          "Movimento do mergulhador"
        ),
        
        
        # ----------------------------------------------------
        # CONTROLES
        # ----------------------------------------------------
        
        uiOutput(
          "controles"
        ),
        
        
        # ----------------------------------------------------
        # ESTADO
        # ----------------------------------------------------
        
        uiOutput(
          "estado"
        ),
        
        
        # ----------------------------------------------------
        # PAINEL FISIOLÓGICO
        # ----------------------------------------------------
        
        h4(
          "N₂ nos tecidos e na circulação"
        ),
        
        
        # ----------------------------------------------------
        # LEGENDA
        # ----------------------------------------------------
        
        div(
          
          class =
            "legenda",
          
          
          div(
            
            class =
              "item-legenda",
            
            div(
              class =
                "simbolo-n2"
            ),
            
            "N₂ dissolvido"
          ),
          
          
          div(
            
            class =
              "item-legenda",
            
            div(
              class =
                "simbolo-tecido"
            ),
            
            "Bolha no tecido"
          ),
          
          
          div(
            
            class =
              "item-legenda",
            
            div(
              class =
                "simbolo-vaso"
            ),
            
            "Bolha no vaso"
          )
        ),
        
        
        # ----------------------------------------------------
        # TECIDO + VASO
        # ----------------------------------------------------
        
        uiOutput(
          "painel_fisiologico"
        ),
        
        
        # ----------------------------------------------------
        # ALERTA
        # ----------------------------------------------------
        
        uiOutput(
          "alerta"
        ),
        
        
        # ----------------------------------------------------
        # INTERPRETAÇÃO
        # ----------------------------------------------------
        
        div(
          
          class =
            "interpretacao",
          
          textOutput(
            "interpretacao"
          )
        ),
        
        
        # ----------------------------------------------------
        # AVISO
        # ----------------------------------------------------
        
        div(
          
          class =
            "aviso",
          
          paste(
            "PPCC Fisiologia Humana 26.2 - Daisy e Sofia B."
          )
        )
      )
    )
  )
)


# ============================================================
# SERVIDOR
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # ESTADO DA SIMULAÇÃO
  # ==========================================================
  
  nivel <- reactiveVal(
    0
  )
  
  
  # Fases possíveis:
  #
  # descida
  # fundo
  # subida_lenta
  # superficie_lenta
  # subida_rapida_meio
  # subida_rapida_superficie
  
  fase <- reactiveVal(
    "descida"
  )
  
  
  # ==========================================================
  # DESCIDA
  # ==========================================================
  
  observeEvent(
    input$descer,
    {
      
      if (
        fase() ==
        "descida"
      ) {
        
        novo_nivel <- min(
          nivel() + 1,
          NIVEL_MAXIMO
        )
        
        
        nivel(
          novo_nivel
        )
        
        
        if (
          novo_nivel ==
          NIVEL_MAXIMO
        ) {
          
          fase(
            "fundo"
          )
        }
      }
    }
  )
  
  
  # ==========================================================
  # ESCOLHER SUBIDA DEVAGAR
  # ==========================================================
  
  observeEvent(
    input$escolher_lenta,
    {
      
      if (
        fase() ==
        "fundo"
      ) {
        
        fase(
          "subida_lenta"
        )
      }
    }
  )
  
  
  # ==========================================================
  # CONTINUAR SUBIDA DEVAGAR
  # ==========================================================
  
  observeEvent(
    input$subir_lenta,
    {
      
      if (
        fase() ==
        "subida_lenta"
      ) {
        
        novo_nivel <- max(
          nivel() - 1,
          0
        )
        
        
        nivel(
          novo_nivel
        )
        
        
        if (
          novo_nivel == 0
        ) {
          
          fase(
            "superficie_lenta"
          )
        }
      }
    }
  )
  
  
  # ==========================================================
  # ESCOLHER SUBIDA RÁPIDA
  #
  # Nível 10 -> nível 5
  # ==========================================================
  
  observeEvent(
    input$escolher_rapida,
    {
      
      if (
        fase() ==
        "fundo"
      ) {
        
        nivel(
          5
        )
        
        
        fase(
          "subida_rapida_meio"
        )
      }
    }
  )
  
  
  # ==========================================================
  # CONTINUAR SUBIDA RÁPIDA
  #
  # Nível 5 -> superfície
  # ==========================================================
  
  observeEvent(
    input$continuar_rapida,
    {
      
      if (
        fase() ==
        "subida_rapida_meio"
      ) {
        
        nivel(
          0
        )
        
        
        fase(
          "subida_rapida_superficie"
        )
      }
    }
  )
  
  
  # ==========================================================
  # REINICIAR
  # ==========================================================
  
  observeEvent(
    input$reiniciar,
    {
      
      nivel(
        0
      )
      
      
      fase(
        "descida"
      )
    }
  )
  
  
  # ==========================================================
  # CONTROLES
  # ==========================================================
  
  output$controles <-
    renderUI({
      
      
      # ------------------------------------------------------
      # DESCIDA
      # ------------------------------------------------------
      
      if (
        fase() ==
        "descida"
      ) {
        
        return(
          
          actionButton(
            
            inputId =
              "descer",
            
            label =
              "↓ Descer um nível",
            
            class =
              "btn-primary botao-grande"
          )
        )
      }
      
      
      # ------------------------------------------------------
      # FUNDO
      # ------------------------------------------------------
      
      if (
        fase() ==
        "fundo"
      ) {
        
        return(
          
          tagList(
            
            
            div(
              
              style =
                "margin-bottom:8px; font-weight:600;",
              
              "Escolha como o mergulhador vai subir:"
            ),
            
            
            div(
              
              class =
                "controles",
              
              
              actionButton(
                
                inputId =
                  "escolher_lenta",
                
                label =
                  "↑ Subir devagar",
                
                class =
                  "btn-success"
              ),
              
              
              actionButton(
                
                inputId =
                  "escolher_rapida",
                
                label =
                  "↑↑ Subir rápido",
                
                class =
                  "btn-warning"
              )
            )
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA DEVAGAR
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_lenta"
      ) {
        
        return(
          
          actionButton(
            
            inputId =
              "subir_lenta",
            
            label =
              "↑ Continuar subida devagar",
            
            class =
              "btn-success botao-grande"
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - NÍVEL 5
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_rapida_meio"
      ) {
        
        return(
          
          tagList(
            
            
            div(
              
              class =
                "alerta",
              
              paste(
                "Parada no nível 5:",
                "observe o tecido e a circulação."
              )
            ),
            
            
            actionButton(
              
              inputId =
                "continuar_rapida",
              
              label =
                "↑↑ Continuar subida rápida",
              
              class =
                "btn-warning botao-grande"
            )
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUPERFÍCIE
      # ------------------------------------------------------
      
      actionButton(
        
        inputId =
          "reiniciar",
        
        label =
          "↺ Reiniciar mergulho",
        
        class =
          "btn-primary botao-grande"
      )
    })
  
  
  # ==========================================================
  # POSIÇÃO DO MERGULHADOR
  # ==========================================================
  
  posicao_atual <-
    reactive({
      
      
      y <- calcular_y(
        nivel()
      )
      
      
      # ------------------------------------------------------
      # Descida
      # ------------------------------------------------------
      
      if (
        fase() %in%
        c(
          "descida",
          "fundo"
        )
      ) {
        
        x <- calcular_x_descida(
          nivel()
        )
      }
      
      
      # ------------------------------------------------------
      # Subida devagar
      # ------------------------------------------------------
      
      else if (
        fase() %in%
        c(
          "subida_lenta",
          "superficie_lenta"
        )
      ) {
        
        x <- calcular_x_lenta(
          nivel()
        )
      }
      
      
      # ------------------------------------------------------
      # Subida rápida
      # ------------------------------------------------------
      
      else {
        
        x <- calcular_x_rapida(
          nivel()
        )
      }
      
      
      list(
        x = x,
        y = y
      )
    })
  
  
  # ==========================================================
  # CENA
  # ==========================================================
  
  output$cena <-
    renderUI({
      
      
      p <- posicao_atual()
      
      
      # ------------------------------------------------------
      # Texto de movimento
      # ------------------------------------------------------
      
      texto_movimento <- switch(
        
        fase(),
        
        descida =
          "Descendo ↓",
        
        fundo =
          "Fundo",
        
        subida_lenta =
          "Subida devagar ↑",
        
        superficie_lenta =
          "Superfície",
        
        subida_rapida_meio =
          "Subida rápida ↑↑",
        
        subida_rapida_superficie =
          "Superfície"
      )
      
      
      # ------------------------------------------------------
      # Mostrar rotas de subida somente após chegar ao fundo
      # ------------------------------------------------------
      
      mostrar_rotas <-
        fase() !=
        "descida"
      
      
      # ------------------------------------------------------
      # Cena
      # ------------------------------------------------------
      
      div(
        
        class =
          "cena",
        
        
        # Superfície
        div(
          class =
            "superficie"
        ),
        
        
        # Linhas
        criar_linhas_niveis(),
        
        
        # Descida
        criar_trajetoria_descida(),
        
        
        # ----------------------------------------------------
        # CAMINHOS POSSÍVEIS DE SUBIDA
        # ----------------------------------------------------
        
        if (
          mostrar_rotas
        ) {
          
          tagList(
            
            
            criar_trajetoria_lenta(),
            
            
            criar_trajetoria_rapida(),
            
            
            div(
              
              class =
                "rotulo-caminho rotulo-lenta",
              
              "Subida devagar"
            ),
            
            
            div(
              
              class =
                "rotulo-caminho rotulo-rapida",
              
              "Subida rápida"
            )
          )
        },
        
        
        # ----------------------------------------------------
        # MERGULHADOR
        # ----------------------------------------------------
        
        tags$img(
          
          src =
            "mergulhador.png",
          
          class =
            "mergulhador",
          
          style =
            paste0(
              
              "left:",
              p$x,
              "%;",
              
              "top:",
              p$y,
              "%;"
            )
        ),
        
        
        # ----------------------------------------------------
        # DIREÇÃO
        # ----------------------------------------------------
        
        div(
          
          class =
            "direcao",
          
          style =
            paste0(
              
              "left:",
              min(
                90,
                p$x + 14
              ),
              "%;",
              
              "top:",
              max(
                4,
                p$y - 5
              ),
              "%;"
            ),
          
          texto_movimento
        ),
        
        
        # Fundo
        div(
          class =
            "fundo"
        )
      )
    })
  
  
  # ==========================================================
  # ESTADO
  # ==========================================================
  
  output$estado <-
    renderUI({
      
      
      caminho <- switch(
        
        fase(),
        
        descida =
          "Descida",
        
        fundo =
          "Ponto mais profundo",
        
        subida_lenta =
          "Subida devagar",
        
        superficie_lenta =
          "Subida devagar concluída",
        
        subida_rapida_meio =
          "Subida rápida",
        
        subida_rapida_superficie =
          "Subida rápida concluída"
      )
      
      
      div(
        
        class =
          "estado",
        
        
        div(
          
          class =
            "estado-principal",
          
          paste0(
            "Nível ",
            nivel(),
            " de 10"
          )
        ),
        
        
        div(
          
          class =
            "estado-secundario",
          
          caminho
        )
      )
    })
  
  
  # ==========================================================
  # ESTADO FISIOLÓGICO
  # ==========================================================
  
  estado_fisiologico <-
    reactive({
      
      
      # ------------------------------------------------------
      # DESCIDA
      # ------------------------------------------------------
      
      if (
        fase() ==
        "descida"
      ) {
        
        return(
          
          list(
            
            n2 =
              N2_DESCIDA[
                nivel() + 1
              ],
            
            tecido =
              0,
            
            vaso =
              0
          )
        )
      }
      
      
      # ------------------------------------------------------
      # FUNDO
      # ------------------------------------------------------
      
      if (
        fase() ==
        "fundo"
      ) {
        
        return(
          
          list(
            
            n2 =
              N2_RAPIDA_NIVEL_10,
            
            tecido =
              BOLHAS_TECIDO_NIVEL_10,
            
            vaso =
              BOLHAS_VASO_NIVEL_10
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA DEVAGAR
      # ------------------------------------------------------
      
      if (
        fase() %in%
        c(
          "subida_lenta",
          "superficie_lenta"
        )
      ) {
        
        return(
          
          list(
            
            n2 =
              N2_SUBIDA_LENTA[
                nivel() + 1
              ],
            
            tecido =
              0,
            
            vaso =
              0
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - NÍVEL 5
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_rapida_meio"
      ) {
        
        return(
          
          list(
            
            n2 =
              N2_RAPIDA_NIVEL_5,
            
            tecido =
              BOLHAS_TECIDO_NIVEL_5,
            
            vaso =
              BOLHAS_VASO_NIVEL_5
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - SUPERFÍCIE
      # ------------------------------------------------------
      
      list(
        
        n2 =
          N2_RAPIDA_NIVEL_0,
        
        tecido =
          BOLHAS_TECIDO_NIVEL_0,
        
        vaso =
          BOLHAS_VASO_NIVEL_0
      )
    })
  
  
  # ==========================================================
  # PAINEL FISIOLÓGICO
  # ==========================================================
  
  output$painel_fisiologico <-
    renderUI({
      
      
      e <- estado_fisiologico()
      
      
      criar_painel_fisiologico(
        
        n2 =
          e$n2,
        
        bolhas_tecido =
          e$tecido,
        
        bolhas_vaso =
          e$vaso
      )
    })
  
  
  # ==========================================================
  # ALERTA
  # ==========================================================
  
  output$alerta <-
    renderUI({
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - NÍVEL 5
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_rapida_meio"
      ) {
        
        return(
          
          div(
            
            class =
              "alerta",
            
            paste(
              "No nível 5 após uma subida rápida,",
              "representamos bolhas tanto no tecido",
              "quanto na circulação."
            )
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - SUPERFÍCIE
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_rapida_superficie"
      ) {
        
        return(
          
          div(
            
            class =
              "alerta",
            
            paste(
              "Após a subida rápida até a superfície,",
              "a representação mostra uma quantidade",
              "ainda maior de bolhas."
            )
          )
        )
      }
      
      
      NULL
    })
  
  
  # ==========================================================
  # INTERPRETAÇÃO
  # ==========================================================
  
  output$interpretacao <-
    renderText({
      
      
      # ------------------------------------------------------
      # DESCIDA
      # ------------------------------------------------------
      
      if (
        fase() ==
        "descida"
      ) {
        
        return(
          
          paste(
            
            "Durante a descida, a pressão aumenta.",
            
            "A cada nível acrescentamos duas partículas de N₂",
            
            "para representar qualitativamente o aumento do",
            
            "nitrogênio dissolvido no organismo."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # FUNDO
      # ------------------------------------------------------
      
      if (
        fase() ==
        "fundo"
      ) {
        
        return(
          
          paste(
            
            "No nível 10 representamos a maior quantidade",
            
            "de N₂ dissolvido.",
            
            "Agora escolha uma subida devagar ou rápida",
            
            "para comparar as duas situações."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA DEVAGAR
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_lenta"
      ) {
        
        return(
          
          paste(
            
            "Durante a subida devagar, diminuímos",
            
            "progressivamente a quantidade de N₂ dissolvido.",
            
            "Nesta representação simplificada não mostramos",
            
            "formação de bolhas."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUPERFÍCIE APÓS SUBIDA DEVAGAR
      # ------------------------------------------------------
      
      if (
        fase() ==
        "superficie_lenta"
      ) {
        
        return(
          
          paste(
            
            "O mergulhador chegou à superfície após uma",
            
            "subida gradual.",
            
            "A representação mostra pouca quantidade adicional",
            
            "de N₂ dissolvido e nenhuma bolha."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - NÍVEL 5
      # ------------------------------------------------------
      
      if (
        fase() ==
        "subida_rapida_meio"
      ) {
        
        return(
          
          paste(
            
            "O mergulhador passou rapidamente do nível 10",
            
            "para o nível 5.",
            
            "Ainda representamos uma grande quantidade de N₂",
            
            "dissolvido e agora aparecem várias bolhas",
            
            "no tecido e dentro da circulação.",
            
            "Compare com o nível 5 durante a subida devagar."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # SUBIDA RÁPIDA - SUPERFÍCIE
      # ------------------------------------------------------
      
      paste(
        
        "O mergulhador chegou rapidamente à superfície.",
        
        "A representação mostra mais bolhas no tecido",
        
        "e na circulação, além de N₂ que ainda",
        
        "permanece representado como dissolvido."
      )
    })
}


# ============================================================
# EXECUTAR APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)