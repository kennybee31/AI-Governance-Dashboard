# 在 app.R 的最頂端加入這幾行
library(shiny)
library(ggplot2)
library(munsell)   # 強制聲明：修復 ggplot2 載入錯誤
library(colorspace) # 預防下一個潛在的依賴報錯
library(dplyr)
library(stringr)
library(bslib)

# 1. 載入數據
data_file <- "top_skills_final.rds" 
all_skills <- if(file.exists(data_file)) readRDS(data_file) else data.frame(skill=paste("Skill", 1:30), N=sample(10:100, 30))

ui <- page_sidebar(
  title = "ECBD: Global AI Risk Governance Intelligence",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#002B5B"),
  
  sidebar = sidebar(
    title = "Analysis Controls",
    p("Strategic Insights from 1.3M+ Postings", style = "font-size: 0.9em;"),
    sliderInput("top_n", "Top Skill Threshold:", min = 5, max = 30, value = 15),
    hr(),
    # 增加 Metadata 標記
    span("📁 Data Source:", style = "font-weight: bold;"), "Kaggle (asaniczka)",
    br(),
    span("📅 Data Year:", style = "font-weight: bold;"), "2024 (Foundation Year)",
    br(),
    span("⚠️ Disclaimer:", style = "font-weight: bold; color: #D9534F;"), 
    p("Historical baseline for 2026 ISO 42001 Strategic Planning.", style = "font-size: 0.8em;"),
    hr(),
    p("Developed by Ken @ ECBD", style = "color: #888; font-size: 0.8em;")
  ),
  
  # 增加頂部關鍵指標 (Value Boxes)
  layout_column_wrap(
    width = 1/3,
    value_box(
      title = "Total Jobs Analyzed",
      value = "1.3M+",
      showcase = bsicons::bs_icon("database"),
      theme = "primary"
    ),
    value_box(
      title = "Top Governance Skill",
      value = "Responsible AI",
      showcase = bsicons::bs_icon("shield-check"),
      theme = "secondary"
    ),
    value_box(
      title = "ISO 42001 Readiness",
      value = "High Demand",
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = "success"
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header("AI Governance Skill Demand Distribution"),
    # 動態高度繪圖區
    uiOutput("dynamic_plot")
  )
)

server <- function(input, output) {
  output$dynamic_plot <- renderUI({
    plotOutput("skillPlot", height = paste0(400 + (input$top_n * 15), "px"))
  })
  
  output$skillPlot <- renderPlot({
    plot_data <- all_skills %>% head(input$top_n) %>%
      mutate(skill_en = str_wrap(skill, width = 20))
    
    ggplot(plot_data, aes(x = reorder(skill_en, N), y = N)) +
      geom_col(fill = "#002B5B", width = 0.7) +
      geom_text(aes(label = N), hjust = -0.3, fontface = "bold", color = "#002B5B", 
                size = if(input$top_n > 20) 3.5 else 5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      theme_minimal(base_size = if(input$top_n > 20) 11 else 14) +
      theme(panel.grid.major.y = element_blank(), axis.title = element_text(color = "#777")) +
      labs(x = NULL, y = "Demand Frequency")
  })
}

shinyApp(ui, server)