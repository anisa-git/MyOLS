library(shiny)
library(MyOLS)

ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f7f7f7;
      font-family: 'Segoe UI', sans-serif;
    }
    h4 {
      color: #2c3e50;
      font-weight: bold;
    }
    .shiny-output-error {
      color: red;
    }
    .well {
      background-color: #ffffff;
      border-radius: 10px;
      box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
      padding: 20px;
      margin-bottom: 20px;
    }
  ")),

  titlePanel("Analisis Regresi Linier - MyOLS"),

  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Unggah File CSV (pakai ; sebagai pemisah)", accept = ".csv"),
      uiOutput("var_select_y"),
      uiOutput("var_select_x"),
      actionButton("proses", "🔍 Analisis Sekarang", class = "btn-primary")
    ),

    mainPanel(
      conditionalPanel(
        condition = "output.modelReady == true",

        div(class = "well", h4("Koefisien Regresi:"), verbatimTextOutput("coef")),
        div(class = "well", h4("Sigma^2:"), verbatimTextOutput("sigma2")),

        div(class = "well", h4("Uji Normalitas Residual:"),
            plotOutput("plot_normal"),
            verbatimTextOutput("normal_out")),

        div(class = "well", h4("Uji Homoskedastisitas:"),
            plotOutput("plot_homos"),
            verbatimTextOutput("homos_out")),

        div(class = "well", h4("Uji Autokorelasi (Durbin-Watson):"),
            verbatimTextOutput("dw_out")),

        div(class = "well", h4("Uji Multikolinearitas (VIF):"),
            verbatimTextOutput("vif_out"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactiveVal()
  modelFit <- reactiveVal()

  observeEvent(input$datafile, {
    df <- read.csv(input$datafile$datapath, sep = ";")
    updateSelectInput(session, "response", choices = names(df))
    updateCheckboxGroupInput(session, "predictors", choices = names(df))
    dataInput(df)
  })

  output$var_select_y <- renderUI({
    req(dataInput())
    selectInput("response", "Pilih Variabel Respon (Y):", choices = names(dataInput()))
  })

  output$var_select_x <- renderUI({
    req(dataInput())
    checkboxGroupInput("predictors", "Pilih Variabel Prediktor (X):", choices = names(dataInput()))
  })

  observeEvent(input$proses, {
    req(dataInput(), input$response, input$predictors)

    df <- dataInput()

    if (input$response %in% input$predictors) {
      showNotification("Variabel respon tidak boleh sama dengan prediktor.", type = "error")
      return()
    }

    X_data <- df[, input$predictors, drop = FALSE]
    y <- df[[input$response]]

    # Konversi ke numerik
    X_data[] <- lapply(X_data, as.numeric)
    y <- as.numeric(y)

    # Tangani missing data
    if (any(is.na(X_data)) || any(is.na(y))) {
      showNotification("Data mengandung nilai NA setelah konversi ke numerik.", type = "error")
      return()
    }

    X <- cbind(1, as.matrix(X_data))  # tambahkan intercept
    model <- ols_fit(X, y)
    modelFit(model)
  })

  output$coef <- renderPrint({
    req(modelFit())
    modelFit()$coefficients
  })

  output$sigma2 <- renderPrint({
    req(modelFit())
    modelFit()$sigma2
  })

  output$plot_normal <- renderPlot({
    req(modelFit())
    par(mfrow = c(1, 2))
    normality(modelFit())
    par(mfrow = c(1, 1))
  })

  output$normal_out <- renderPrint({
    req(modelFit())
    res <- modelFit()$residuals
    n <- length(res)
    mean_res <- mean(res)
    sd_res <- sd(res)
    skew <- sum((res - mean_res)^3) / (n * sd_res^3)
    kurt <- sum((res - mean_res)^4) / (n * sd_res^4)
    cat("Skewness:", round(skew, 3), "\n")
    cat("Kurtosis:", round(kurt, 3), "\n")
    if (abs(skew) < 1 && abs(kurt - 3) < 1) {
      cat("✅ Residual cenderung normal.\n")
    } else {
      cat("❌ Residual tidak normal.\n")
    }
  })

  output$plot_homos <- renderPlot({
    req(modelFit())
    homoskedasticity(modelFit())
  })

  output$homos_out <- renderPrint({
    req(modelFit())
    res <- modelFit()$residuals
    spread <- max(res) - min(res)
    if (spread < 4 * sd(res)) {
      cat("✅ Homoskedastisitas terlihat cukup baik.\n")
    } else {
      cat("❌ Ada indikasi heteroskedastisitas.\n")
    }
  })

  output$dw_out <- renderPrint({
    req(modelFit())
    autokorelasi(modelFit())
  })

  output$vif_out <- renderPrint({
    req(dataInput(), input$predictors)
    X_vif <- dataInput()[, input$predictors, drop = FALSE]
    X_vif[] <- lapply(X_vif, as.numeric)
    multikolinearitas(as.matrix(X_vif))
  })
}

shinyApp(ui, server)
