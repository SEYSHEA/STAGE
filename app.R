library(shiny)
library(shinyjs)
library(caret) 
library(randomForest)

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
        column(12, div(class = "input-container",
                       div(class = "input-panel",
                           tags$label(class = "input-label", "Température (°C)"),
                           numericInput("temperature", label = NULL, value = NA, step = 0.50),
                           tags$img(src = "logo_temperature.png", class = "input-logo")
                       ),
                       div(class = "input-panel",
                           tags$label(class = "input-label", "SO2a (mg/L)"),
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
                       div(id = "advice-box-green", "voicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseilles")
                   ),
                   div(class = "result-wrapper",
                       div(class = "result-square orange", id = "orange-square", "Risque fort"),
                       div(id = "triangle-container-orange"),
                       div(id = "advice-box-orange", "voicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseillesvoicie les conseilles")
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
                             actionButton("clearHistory", "Vider l'historique", class = "clear-button")
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
  
  <p>Entrer les données :  </p>
  
  <p>Température (°C) :    <br>
      Saisissez la température actuelle en degrés Celsius dans le champ à gauche. </p>
  
  <p> SO2a (mg/L) : <br>
  Entrez la concentration de SO2 libre en milligrammes par litre dans le champ au centre.
  </p>
  

  <p> TAV (%) : <br>
  Indiquez le taux d'alcool par volume en pourcentage dans le champ à droite. 
  </p>
  
  <p> Le résultat s'affichera en dessous : <br>

   - Un risque faible implique que le taux de brettanomyces est assez raisonnable pour ne pas altérer la qualité du vin <br>
   - Un risque fort implique que le taux de brettanomyces est trop élevé et présente donc un risque d'altération de la qualité du vin <br>
  </p>
        <hr>
        <h1>Informations</h1> <br>
        <p> Qu'est ce que Brettanomyces </p>
        <p> Brettanomyces, souvent appelée Brett, est une levure qui peut contaminer le vin et affecter son goût et son arôme.Cette levure est connue pour produire des composés phénoliques indésirables qui peuvent donner des arômes de cuir, de sueur de cheval, ou de plastique brûlé.</p> 
        <p> Pourquoi est-elle importante </p>
        <p> Bien que certains vins bénéficient d'une petite présence de Brettanomyces, une contamination excessive peut ruiner le vin. </p>
        <p> Comment se propage -t-elle </p>
        <p> Brettanomyces peut se trouver dans les barriques en bois, les équipements de vinification et même dans l'air des caves. Elle prolifère particulièrement bien dans des conditions chaudes et avec une faible présence de SO2 libre. </p>
        <hr>
      ")
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
    user_ip <- input$user_ip
    
    if (is.na(temp) || is.na(so2a) || is.na(tav) || temp < -50 || temp > 50 || so2a < 0 || so2a > 100 || tav < 0 || tav > 100) {
      session$sendCustomMessage(type = 'shakeButton', message = list(id = "predict"))
      output$validation_message <- renderText({ "Veuillez entrer des valeurs valides." })
      output$results <- renderText({ "" })
      session$sendCustomMessage(type = 'handlePrediction', message = list(risk_label = ""))
    } else {
      new_data <- data.frame(T = temp, SO2a = so2a, TAV = tav)
      prediction <- predict(rf_model, new_data)
      risk_label <- ifelse(prediction == "Risque 0", "Risque 0", ifelse(prediction == "Risque 2", "Risque 2", "Risque 1"))
      
      output$validation_message <- renderText({ "" })
      
      rv$history <- rbind(rv$history, data.frame(Date = format(Sys.time(), "%d/%m/%Y"),
                                                 IP = user_ip,
                                                 Température = temp,
                                                 SO2a = so2a,
                                                 TAV = tav,
                                                 Risque = risk_label))
      
      save_history(rv$history)
      
      if (rv$currentPage > ceiling(nrow(rv$history) / 5)) {
        rv$currentPage <- ceiling(nrow(rv$history) / 5)
      }
      
      session$sendCustomMessage(type = 'handlePrediction', message = list(risk_label = risk_label))
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
      return(data.frame(Date = character(), Température = numeric(), SO2a = numeric(), TAV = numeric(), Risque = character(), Delete = character()))
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
}

shinyApp(ui = ui, server = server)
