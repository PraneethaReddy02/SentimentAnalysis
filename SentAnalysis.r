# Clear environment
rm(list = ls())

# Install and load packages with feedback
required_packages <- c("shiny", "tidytext", "dplyr", "tidyr", "readr", "ggplot2", "DT", "textdata", "wordcloud", "RColorBrewer")
for (pkg in required_packages) {
  cat(paste("Checking", pkg, "...\n"))
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

cat("All packages loaded successfully.\n")

# Define UI
ui <- fluidPage(
  titlePanel("Business Sentiment Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      helpText("CSV must have a 'text' column"),
      actionButton("analyze", "Analyze Sentiment")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 plotOutput("sentiment_plot", height = "300px"),
                 plotOutput("recommendation_gauge", height = "300px"),
                 DTOutput("summary_table")
        ),
        tabPanel("Detailed Scores", DTOutput("sentiment_table")),
        tabPanel("Key Words", plotOutput("wordcloud_plot", height = "400px")),
        tabPanel("Raw Data", DTOutput("raw_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  cat("Server function initialized.\n")
  
  data <- reactiveVal()
  
  observeEvent(input$file, {
    cat("File upload triggered.\n")
    req(input$file)
    df <- tryCatch({
      readr::read_csv(input$file$datapath)
    }, error = function(e) {
      showNotification(paste("CSV read error:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(df)) return()
    if (!"text" %in% colnames(df)) {
      showNotification("CSV must have a 'text' column", type = "error")
      return()
    }
    
    df$text <- as.character(df$text)
    data(df)
    output$raw_table <- renderDT({
      DT::datatable(df, options = list(pageLength = 10))
    })
    cat("Raw data loaded.\n")
  })
  
  observeEvent(input$analyze, {
    cat("Analyze button clicked.\n")
    req(data())
    df <- data()
    
    if (nrow(df) == 0 || all(is.na(df$text)) || all(trimws(df$text) == "")) {
      showNotification("No valid text data", type = "error")
      return()
    }
    
    afinn <- tryCatch({
      tidytext::get_sentiments("afinn")
    }, error = function(e) {
      showNotification("AFINN lexicon load failed", type = "error")
      return(NULL)
    })
    if (is.null(afinn)) return()
    
    sentiment_df <- tryCatch({
      df_with_id <- df %>% dplyr::mutate(temp_id = dplyr::row_number())
      scores <- df_with_id %>%
        tidytext::unnest_tokens(word, text) %>%
        dplyr::inner_join(afinn, by = "word") %>%
        dplyr::group_by(temp_id) %>%
        dplyr::summarise(sentiment_score = sum(value, na.rm = TRUE))
      
      df_with_id %>%
        dplyr::left_join(scores, by = "temp_id") %>%
        dplyr::mutate(sentiment_score = ifelse(is.na(sentiment_score), 0, sentiment_score)) %>%
        dplyr::select(temp_id, text, sentiment_score)
    }, error = function(e) {
      showNotification(paste("Analysis error:", e$message), type = "error")
      return(df %>% dplyr::mutate(temp_id = dplyr::row_number(), sentiment_score = NA_real_))
    })
    
    summary_stats <- sentiment_df %>%
      dplyr::summarise(
        Avg_Sentiment = round(mean(sentiment_score, na.rm = TRUE), 2),
        Positive_Pct = round(mean(sentiment_score > 0, na.rm = TRUE) * 100, 1),
        Negative_Pct = round(mean(sentiment_score < 0, na.rm = TRUE) * 100, 1),
        Neutral_Pct = round(mean(sentiment_score == 0, na.rm = TRUE) * 100, 1)
      )
    
    recommendation_score <- summary_stats$Positive_Pct - summary_stats$Negative_Pct
    
    word_freq <- df %>%
      tidytext::unnest_tokens(word, text) %>%
      dplyr::inner_join(afinn, by = "word") %>%
      dplyr::group_by(word) %>%
      dplyr::summarise(freq = n()) %>%
      dplyr::arrange(desc(freq))
    
    output$summary_table <- renderDT({
      DT::datatable(summary_stats, options = list(dom = 't'), rownames = FALSE)
    })
    
    output$sentiment_table <- renderDT({
      DT::datatable(sentiment_df, options = list(pageLength = 10))
    })
    
    output$sentiment_plot <- renderPlot({
      ggplot(sentiment_df, aes(x = sentiment_score)) +
        geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
        labs(title = "Sentiment Distribution", x = "Score", y = "Count") +
        theme_minimal()
    })
    
    output$recommendation_gauge <- renderPlot({
      ggplot(data.frame(score = recommendation_score), aes(x = 1, y = score)) +
        geom_bar(stat = "identity", fill = ifelse(recommendation_score >= 0, "green", "red"), width = 0.5) +
        coord_flip() + ylim(-100, 100) +
        labs(title = "Recommendation Score", y = "Score (-100 to 100)", x = "") +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    })
    
    output$wordcloud_plot <- renderPlot({
      set.seed(42)
      wordcloud::wordcloud(
        words = word_freq$word,
        freq = word_freq$freq,
        min.freq = 1,
        max.words = 50,
        colors = RColorBrewer::brewer.pal(8, "Dark2")
      )
      title(main = "Key Sentiment Words")
    })
    
    cat("Analysis completed.\n")
  })
}

# Force browser launch and run app
options(shiny.launch.browser = TRUE)
cat("Launching Shiny app...\n")
shinyApp(ui = ui, server = server)
