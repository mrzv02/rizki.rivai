library(shiny)
library(bslib)
library(thematic)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(DT)

# ================= UI ================= #
ui <- fluidPage(
  
  # DARK THEME
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#4DB8FF"
  ),
  
  titlePanel("Dashboard SOB Performance"),
  
  sidebarLayout(
    
    # ------- SIDEBAR ------- #
    sidebarPanel(
      width = 3,
      h3("Filter Data"),
      
      selectInput("yearInput", "Select Year:",
                  choices = c("All", 2022:2025), selected = "All"),
      
      selectInput("sobInput", "Select SOB:", choices = "All"),
      
      selectInput("cobInput", "Select COB:", choices = "All"),
      
      selectInput("categoryInput", "Select Metric:",
                  choices = c("NETPREMIUM", "NETPREMIUM_ABSOLUTE",
                              "UW_SURPLUS", "UW_SURPLUS_ABSOLUTE"),
                  selected = "NETPREMIUM")
    ),
    
    # ------- MAIN PANEL ------- #
    mainPanel(
      width = 9,
      
      div(
        style = "padding:15px; background:#1a1a1a; border-radius:10px; margin-bottom:20px;",
        h4("Vertical Bar Chart by SOB"),
        plotlyOutput("barPlot", height = "900px")
      ),
      
      div(
        style = "padding:15px; background:#1a1a1a; border-radius:10px;",
        h4("Filtered Data Table"),
        DTOutput("tableData")
      )
    )
  )
)

# ================= SERVER ================= #
server <- function(input, output, session) {
  
  thematic_shiny()  # activate dark mode for ggplot
  
  df_long <- read_excel("DATA/df_long4.xlsx")
  
  # Format Value → Billion
  df_long <- df_long %>%
    mutate(Value = as.numeric(format(Value / 1e9,
                                     scientific = FALSE,
                                     nsmall = 9)))
  
  # ========== DYNAMIC SOB FILTER ========== #
  observe({
    data_filtered <- df_long
    if (input$yearInput != "All") {
      data_filtered <- data_filtered %>% filter(YEAR == input$yearInput)
    }
    
    sob_choices <- c("All", sort(unique(data_filtered$GROUP_SOB)))
    
    updateSelectInput(session, "sobInput",
                      choices = sob_choices,
                      selected = ifelse(input$sobInput %in% sob_choices, input$sobInput, "All"))
  })
  
  # ========== DYNAMIC COB FILTER ========== #
  observe({
    data_filtered <- df_long
    
    if (input$yearInput != "All") {
      data_filtered <- data_filtered %>% filter(YEAR == input$yearInput)
    }
    if (input$sobInput != "All") {
      data_filtered <- data_filtered %>% filter(GROUP_SOB == input$sobInput)
    }
    
    cob_choices <- c("All", sort(unique(data_filtered$COB)))
    
    updateSelectInput(session, "cobInput",
                      choices = cob_choices,
                      selected = ifelse(input$cobInput %in% cob_choices, input$cobInput, "All"))
  })
  
  # ========== FILTERED DATA LOGIC (FULL) ========== #
  filtered_data <- reactive({
    data <- df_long
    
    # Filter 1: Year
    if (input$yearInput != "All") {
      data <- data %>% filter(YEAR == input$yearInput)
    }
    
    # Filter 2: SOB
    if (input$sobInput != "All") {
      data <- data %>% filter(GROUP_SOB == input$sobInput)
    }
    
    # Filter 3: COB + Aggregation Logic
    if (input$cobInput != "All") {
      data <- data %>% filter(COB == input$cobInput)
      
    } else if (input$cobInput == "All" && input$sobInput != "All") {
      
      data <- data %>%
        group_by(GROUP_SOB, YEAR, Metric) %>%
        summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
      
    } else if (input$cobInput == "All" && input$sobInput == "All") {
      
      data <- data %>%
        group_by(GROUP_SOB, YEAR, Metric) %>%
        summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
    }
    
    # ---- Sorting ----
    selected_metric <- input$categoryInput
    
    base_order <- data %>%
      filter(Metric == selected_metric) %>%
      group_by(GROUP_SOB) %>%
      summarise(order_value = sum(Value, na.rm = TRUE)) %>%
      arrange(desc(order_value)) %>%
      pull(GROUP_SOB)
    
    data$GROUP_SOB <- factor(data$GROUP_SOB, levels = base_order)
    
    # Facet ordering
    data$Metric <- factor(
      data$Metric,
      levels = c("NETPREMIUM", "UW_SURPLUS",
                 "NETPREMIUM_ABSOLUTE", "UW_SURPLUS_ABSOLUTE")
    )
    
    # ---- Top 10 SOB (Only when Year = All) ----
    if (input$yearInput == "All") {
      top_sob <- data %>%
        group_by(GROUP_SOB) %>%
        summarise(total_value = sum(Value, na.rm = TRUE)) %>%
        arrange(desc(total_value)) %>%
        slice_head(n = 10) %>%
        pull(GROUP_SOB)
      
      data <- data %>% filter(GROUP_SOB %in% top_sob)
    }
    
    data
  })
  
  # ========== PLOT ========== #
  output$barPlot <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = GROUP_SOB, y = Value, fill = as.factor(YEAR))) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, color = "white") +
      facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
      scale_y_continuous(
        labels = number_format(accuracy = 1, big.mark = ",")
      ) +
      scale_fill_manual(values = c(
        "2022" = "#FF99CC",
        "2023" = "#66CCFF",
        "2024" = "#FFCC33",
        "2025" = "#66CC66"
      )) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
        strip.text = element_text(face = "bold")
      )
    
    ggplotly(p) %>% layout(plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a")
  })
  
  # ========== TABLE ========== #
  output$tableData <- renderDT({
    datatable(
      filtered_data(),
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
