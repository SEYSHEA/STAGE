library(shiny)
library(shinyjs)
library(caret) 
library(randomForest)
library(openxlsx)
rf_model <- readRDS("www/rf_model_best_params.rds")

load_history <- function() {
  if (file.exists("history.csv")) {
    return(read.csv("history.csv", stringsAsFactors = FALSE))
  } else {
    return(data.frame(Date = character(),
                      IP = character(),
                      Température = numeric(),
                      SO2a = numeric(),
                      TAV = numeric(),
                      Risque = character(),
                      stringsAsFactors = FALSE))
  }
}

save_history <- function(history) {
  write.csv(history, "history.csv", row.names = FALSE)
}

ui <- fluidPage(
  useShinyjs(),
  includeScript("www/script.js"),
  includeCSS("www/styles.css"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(
      "
      $(document).ready(function(){
        $.getJSON('https://api.ipify.org?format=json', function(data){
          Shiny.setInputValue('user_ip', data.ip);
        });
      });
      "
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.2/css/all.min.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  tags$nav(
    div(class = "logo", "Institut Rhodanien"),
    tags$input(type = "checkbox", id = "click"),
    tags$label(`for` = "click", class = "menu-btn", tags$i(class = "fas fa-bars")),
    tags$ul(
      tags$li(tags$a(id = "home-link", class = "active", href = "#", "Home")),
      tags$li(tags$a(id = "about-link", href = "#", "About"))
    )
  ),
  div(id = "home", class = "content",
      fluidRow(
        column(12, align = "center",
               tags$h1(class = "special", "Cet outil a été réalisé sur la base des travaux de l’Institut Rhodanien et de l’ICV et ne s’applique qu’aux vins secs."),
               div(class = "line-decor-container", div(class = "line-decor"))  
        ),
      
        column(12, div(class = "input-container",
                       div(class = "input-panel",
                           tags$label(class = "input-label", "Température (°C)"),
                           numericInput("temperature", label = NULL, value = NA, step = 0.50),
                           tags$img(src = "logo_temperature.png", class = "input-logo")
                       ),
                       div(class = "input-panel",
                           tags$label(class = "input-label", "SO2 actif (mg/L)"),
                           numericInput("so2a", label = NULL, value = NA, step = 0.01),
                           tags$img(src = "logo_so2a.png", class = "input-logo")
                       ),
                       div(class = "input-panel",
                           tags$label(class = "input-label", "TAV (%)"),
                           numericInput("tav", label = NULL, value = NA, step = 1),
                           tags$img(src = "logo_tav.png", class = "input-logo")
                       )
        ))
      ),
      fluidRow(
        column(12, div(class = "button-container",
                       actionButton("predict", "Prédire", class = "predict-button btn-1")
        )),
        column(12, align = "center",
               textOutput("validation_message")),
        column(12, align = "center",
               div(class = "flex-container",
                   div(class = "result-wrapper",
                       div(class = "result-square green", id = "green-square", "Risque faible"),
                       div(id = "triangle-container-green"),
                       div(id = "advice-box-green", "Même avec un risque faible, Maintenez des pratiques d'hygiène strictes et réalisez des mises au propre régulières pour prévenir toute contamination potentielle.",
                           tags$br(),tags$br(),
                           "Maintenir la cave à la température la plus basse possible (si possible <14°C)",
                           tags$br(),tags$br(),
                           "Effectuez des contrôles microbiologiques périodiques pour confirmer l'absence de germes d'altérations et assurer la stabilité microbiologique du vin."
                           )
                   ),
                   div(class = "result-wrapper",
                       div(class = "result-square orange", id = "orange-square", "Risque fort"),
                       div(id = "triangle-container-orange"),
                       div(id = "advice-box-orange", "il est impératif d'augmenter la concentration de SO2 actif pour combattre les microorganismes nuisibles. Utilisez le calculateur de SO2 actif pour déterminer la quantité exacte nécessaire.",
                           tags$a(href = "https://www.vignevin-occitanie.com/outils-en-ligne/so2-actif-ou-moleculaire/", target = "_blank", "calculateur SO2 actif"), 
                           tags$br(),
                           tags$br(),
                           "Abaissez la température de la cave autant que possible pour ralentir la croissance des micro-organismes. Une température inférieure à 14°C est idéale pour minimiser les risques d'altération du vin."
                       )
                   )
               )
        ),
        
        fluidRow(
          column(12,
                 div(align = "center",
                     div(class = "history-container",
                         div(class = "seven",
                             h1("Historique")
                         ),
                         div(class = "table-responsive",
                             tableOutput("historyTable")
                         ),
                         div(class = "pagination-controls",
                             actionButton("prevPage", tags$i(class = "fas fa-chevron-left"), class = "btn pagination-btn"),
                             span(id = "pagination-info", textOutput("paginationInfo", inline = TRUE)),
                             actionButton("nextPage", tags$i(class = "fas fa-chevron-right"), class = "btn pagination-btn")
                         ),
                         div(class = "clear-history",
                             actionButton("clearHistory", "Vider l'historique", class = "clear-button"),
                             downloadButton("downloadData", label = NULL, icon = icon("download", class = "fa-download"), class = "download-button")
                             
                         )
                     )
                 )
          )
        )
      )
),
  
  div(id = "about", class = "content",
      HTML("
      <h1>Mode d'emploi</h1> <br>
      <p> - A l’aide de votre bulletin d’analyse, renseignez les champs concernant le SO2 actif (mg/L) et le TAV (%) de votre cuve <br>
         - Indiquez également la température (°C) actuelle de votre chai. <br>
         - Cliquez sur Prédire.</p><br>
      <p>Le résultat indique le risque d’une matrice à être phénolée en fonction des paramètres œnologiques.</p>
      <p>Un risque faible indique que le risque de production de phénols volatils est faible si ces paramètres œnologiques sont maintenus.</p>
      <p>Un risque fort indique que le risque de production de phénols volatils est élevé. Cette cuve nécessite donc une surveillance accrue.</p>
      <hr>
      <h1>Informations</h1> <br>
      <p><strong>Les phénols volatils et leur impact sur le vin</strong></p>
      <p>Les principaux phénols volatils responsables d’altérations dans les vins sont l’éthyl-4-phénol (4EP) et l’éthyl-4-gaïacol. Ces molécules sont responsables du caractère phénolé et animal et donnent des arômes désagréables au vin, tels que des notes fumées, épicées ou de sueur de cheval. Le seuil de perception des phénols volatils est de l’ordre de 450 µg/L, au-delà duquel le profil aromatique ainsi que la qualité des vins sont fortement impactés.</p>
      <p><strong>La levure Brettanomyces : un risque majeur</strong></p>
      <p>Le principal microorganisme impliqué dans la production de ces phénols volatils est la levure Brettanomyces. Des études ont montré que cette levure peut se développer dans des conditions drastiques, telles que des concentrations d’alcool relativement élevées, des valeurs de pH faibles et un environnement pauvre en nutriments. De plus, ce microorganisme est capable de persister dans des environnements difficiles, notamment en restant dans un état viable mais non cultivable (VNC) et en adhérant aux surfaces.</p>
      <p><strong>Fonctionnalités et portée de l'outil</strong></p>
       
      <p>Cet outil permet de :<br>
      <p>Tester toutes les cuves dès la fin des fermentations afin de mettre en évidence les cuves à risque pour resserrer le suivi analytique.<br>
      <p>Prédire l’évolution du risque en modifiant les paramètres instables (SO2, T°C).<br>
      Déterminer le paramètre à modifier pour les cuves à risques.</p>
      <hr>
    ")
  ),
  # Modal pour l'identifiant de la cuve
  tags$div(id = "modalOverlay"), # Add this line
  tags$div(id = "cuveModal",
           tags$h2("Entrez le numéro de la cuve"),
           textInput("cuve_id", label = NULL, placeholder = "Numéro de la cuve"),
           div(class = "modal-buttons",
               actionButton("submit_cuve_id", "Soumettre"),
               actionButton("cancel_cuve_id", "Annuler")
           )
  )
  
)

server <- function(input, output, session) {
  # Charger l'historique initial
  rv <- reactiveValues(
    history = load_history(),
    currentPage = 1
  )
  
  # Observateur pour la prédiction
  observeEvent(input$predict, {
    temp <- input$temperature
    so2a <- input$so2a
    tav <- input$tav
    
    if (is.na(temp) || is.na(so2a) || is.na(tav) || temp < -50 || temp > 50 || so2a < 0 || so2a > 100 || tav < 0 || tav > 100) {
      session$sendCustomMessage(type = 'shakeButton', message = list(id = "predict"))
      output$validation_message <- renderText({ "Veuillez entrer des valeurs valides." })
      output$results <- renderText({ "" })
      session$sendCustomMessage(type = 'handlePrediction', message = list(risk_label = ""))
    } else {
      # Show the modal to ask for the cuve ID
      session$sendCustomMessage(type = 'showModal', message = list())
    }
  })
  
  observeEvent(input$submit_cuve_id, {
    temp <- input$temperature
    so2a <- input$so2a
    tav <- input$tav
    user_ip <- input$user_ip
    cuve_id <- input$cuve_id
    
    if (cuve_id == "") {
      session$sendCustomMessage(type = 'shakeButton', message = list(id = "cuve_id"))
    } else {
      new_data <- data.frame(T = temp, SO2a = so2a, TAV = tav)
      prediction <- predict(rf_model, new_data)
      risk_label <- ifelse(prediction == "Risque 0", "Risque 0", ifelse(prediction == "Risque 2", "Risque 2", "Risque 1"))
      
      output$validation_message <- renderText({ "" })
      
      rv$history <- rbind(rv$history, data.frame(Date = format(Sys.time(), "%d/%m/%Y"),
                                                 IP = user_ip,
                                                 CuveID = cuve_id,
                                                 Température = temp,
                                                 SO2a = so2a,
                                                 TAV = tav,
                                                 Risque = risk_label))
      
      save_history(rv$history)
      
      if (rv$currentPage > ceiling(nrow(rv$history) / 5)) {
        rv$currentPage <- ceiling(nrow(rv$history) / 5)
      }
      
      session$sendCustomMessage(type = 'handlePrediction', message = list(risk_label = risk_label))
      session$sendCustomMessage(type = 'hideModal', message = list())
    }
  })
  
  observeEvent(input$prevPage, {
    if (rv$currentPage > 1) {
      rv$currentPage <- rv$currentPage - 1
    }
  })
  
  observeEvent(input$nextPage, {
    if (rv$currentPage * 5 < nrow(rv$history)) {
      rv$currentPage <- rv$currentPage + 1
    }
  })
  
  observeEvent(input$clearHistory, {
    rv$history <- data.frame(Date = character(),
                             IP = character(),
                             CuveID = character(),
                             Température = numeric(),
                             SO2a = numeric(),
                             TAV = numeric(),
                             Risque = character(),
                             stringsAsFactors = FALSE)
    rv$currentPage <- 1
    save_history(rv$history)
  })
  
  output$historyTable <- renderTable({
    if (nrow(rv$history) > 0) {
      start <- (rv$currentPage - 1) * 5 + 1
      end <- min(rv$currentPage * 5, nrow(rv$history))
      history_to_show <- rv$history[start:end, ]
      history_to_show <- history_to_show[, -which(names(history_to_show) == "IP")] # Retirer la colonne IP
      history_to_show$Delete <- sapply(seq_len(nrow(history_to_show)), function(i) {
        as.character(actionButton(paste0("delete_", start + i - 1), label = "🗑️", onclick = sprintf("Shiny.setInputValue('delete_row', %d)", start + i - 1)))
      })
      return(history_to_show)
    } else {
      return(data.frame(Date = character(), CuveID = character(), Température = numeric(), SO2a = numeric(), TAV = numeric(), Risque = character(), Delete = character()))
    }
  }, sanitize.text.function = function(x) x, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$paginationInfo <- renderText({
    total_pages <- ceiling(nrow(rv$history) / 5)
    if (total_pages == 0) {
      total_pages <- 1
    }
    paste(rv$currentPage, "de", total_pages)
  })
  
  observeEvent(input$delete_row, {
    row_to_delete <- as.integer(input$delete_row)
    rv$history <- rv$history[-row_to_delete, ]
    save_history(rv$history)
    if (rv$currentPage > ceiling(nrow(rv$history) / 5)) {
      rv$currentPage <- ceiling(nrow(rv$history) / 5)
    }
  })
  
  observe({
    invalidateLater(500, session)
    session$sendCustomMessage(type = "updateHistory", message = list(history = rv$history))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("historique-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      history_no_ip <- rv$history[, -which(names(rv$history) == "IP")]
      openxlsx::write.xlsx(history_no_ip, file)
    }
  )
  
}


shinyApp(ui = ui, server = server)

