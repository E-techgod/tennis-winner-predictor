library(shiny)
library(dplyr)
library(tibble)
library(ranger)
library(here)
library(shinyWidgets)

# Load saved models + template data
DATA_PATH  <- here("data")
MODEL_PATH <- here("models")

matches <- read.csv(file.path(DATA_PATH, "all_atp_matches.csv"), stringsAsFactors = FALSE)
objs <- readRDS(file.path(MODEL_PATH, "saved_models.rds"))

glm_model_full <- objs$glm
rf_full <- objs$rf
data_template <- objs$template

predict_match <- function(
    p1_rank, p2_rank, p1_rank_points, p2_rank_points,
    p1_age, p2_age, surface, tourney_level, best_of, round, year,
    glm_model, rf_model, data_template
) {
  newdata <- tibble(
    year = year,
    surface = factor(surface, levels = levels(data_template$surface)),
    tourney_level = factor(tourney_level, levels = levels(data_template$tourney_level)),
    best_of = factor(best_of, levels = levels(data_template$best_of)),
    round = factor(round, levels = levels(data_template$round)),
    p1_rank = p1_rank, p2_rank = p2_rank,
    p1_rank_points = p1_rank_points, p2_rank_points = p2_rank_points,
    p1_age = p1_age, p2_age = p2_age,
    rank_diff = p1_rank - p2_rank,
    rankpts_diff = p1_rank_points - p2_rank_points,
    age_diff = p1_age - p2_age
  )
  
  glm_prob <- predict(glm_model, newdata = newdata, type = "response")
  rf_pred  <- predict(rf_model, data = newdata)
  rf_prob  <- rf_pred$predictions[, "1"]
  
  summary_tbl <- tibble(
    model = c("Logistic Regression", "Random Forest"),
    prob_p1_win = c(glm_prob, rf_prob),
    prob_p2_win = 1 - prob_p1_win,
    predicted_winner = if_else(prob_p1_win >= 0.5, "Player 1", "Player 2")
  )
  
  list(input_features = newdata, predictions = summary_tbl)
}

ui <- fluidPage(
  # Add Google Fonts
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Poppins:wght@600;700;800&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      * { font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif; }
      
      :root {
        --bg: #0a0f1e;
        --bg-gradient-1: #1a1f3a;
        --bg-gradient-2: #0f1729;
        --card: rgba(255,255,255,0.05);
        --card-hover: rgba(255,255,255,0.08);
        --border: rgba(255,255,255,0.08);
        --text: rgba(255,255,255,0.95);
        --text-muted: rgba(255,255,255,0.65);
        --accent-primary: #00d9ff;
        --accent-secondary: #a78bfa;
        --accent-tertiary: #34d399;
        --accent-warning: #fbbf24;
        --shadow: rgba(0,0,0,0.3);
        --glow-blue: rgba(0,217,255,0.3);
        --glow-purple: rgba(167,139,250,0.2);
      }
      
      body { 
        background: linear-gradient(135deg, var(--bg) 0%, var(--bg-gradient-2) 50%, var(--bg-gradient-1) 100%);
        color: var(--text);
        min-height: 100vh;
        overflow-x: hidden;
      }
      
      /* Animated background particles */
      body::before {
        content: '';
        position: fixed;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: 
          radial-gradient(circle at 20% 20%, var(--glow-purple) 0%, transparent 50%),
          radial-gradient(circle at 80% 80%, var(--glow-blue) 0%, transparent 50%);
        opacity: 0.4;
        animation: float 20s ease-in-out infinite;
        pointer-events: none;
        z-index: 0;
      }
      
      @keyframes float {
        0%, 100% { transform: translateY(0px); }
        50% { transform: translateY(-20px); }
      }
      
      .container-fluid { 
        max-width: 1400px; 
        padding: 24px 20px;
        position: relative;
        z-index: 1;
      }
      
      /* Typography */
      h1, h2, h3, h4, h5 { 
        font-family: 'Poppins', sans-serif;
        color: var(--text); 
        font-weight: 700;
        letter-spacing: -0.02em;
      }
      
      /* Hero Header */
      .hero-header {
        text-align: center;
        margin-bottom: 40px;
        padding: 32px 20px;
        background: linear-gradient(135deg, rgba(0,217,255,0.1) 0%, rgba(167,139,250,0.1) 100%);
        border: 1px solid var(--border);
        border-radius: 24px;
        backdrop-filter: blur(20px);
        position: relative;
        overflow: hidden;
      }
      
      .hero-header::before {
        content: '';
        position: absolute;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.05), transparent);
        transform: translateX(-100%);
        animation: shine 3s infinite;
      }
      
      @keyframes shine {
        0% { transform: translateX(-100%); }
        100% { transform: translateX(100%); }
      }
      
      .hero-title {
        font-size: 48px;
        font-weight: 800;
        background: linear-gradient(135deg, var(--accent-primary), var(--accent-secondary), var(--accent-tertiary));
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 12px;
        text-shadow: 0 0 30px var(--glow-blue);
      }
      
      .hero-subtitle {
        font-size: 18px;
        color: var(--text-muted);
        font-weight: 500;
      }
      
      .badge-group {
        display: flex;
        gap: 12px;
        justify-content: center;
        margin-top: 20px;
        flex-wrap: wrap;
      }
      
      .badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 8px 16px;
        background: rgba(0,217,255,0.1);
        border: 1px solid rgba(0,217,255,0.3);
        border-radius: 20px;
        font-size: 13px;
        font-weight: 600;
        color: var(--accent-primary);
      }
      
      .badge.purple {
        background: rgba(167,139,250,0.1);
        border-color: rgba(167,139,250,0.3);
        color: var(--accent-secondary);
      }
      
      .badge.green {
        background: rgba(52,211,153,0.1);
        border-color: rgba(52,211,153,0.3);
        color: var(--accent-tertiary);
      }
      
      /* Cards */
      .card {
        background: var(--card);
        border: 1px solid var(--border);
        border-radius: 20px;
        padding: 24px;
        margin-bottom: 20px;
        backdrop-filter: blur(10px);
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .card::before {
        content: '';
        position: absolute;
        top: 0; left: 0;
        width: 100%; height: 3px;
        background: linear-gradient(90deg, var(--accent-primary), var(--accent-secondary));
        opacity: 0;
        transition: opacity 0.3s;
      }
      
      .card:hover {
        background: var(--card-hover);
        border-color: rgba(255,255,255,0.15);
        transform: translateY(-2px);
        box-shadow: 0 20px 40px var(--shadow);
      }
      
      .card:hover::before {
        opacity: 1;
      }
      
      .card-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 20px;
        padding-bottom: 16px;
        border-bottom: 1px solid var(--border);
      }
      
      .card-title {
        font-size: 20px;
        font-weight: 700;
        margin: 0;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .card-icon {
        font-size: 24px;
      }
      
      .pill {
        display: inline-block;
        padding: 6px 12px;
        background: rgba(255,255,255,0.05);
        border: 1px solid var(--border);
        border-radius: 12px;
        font-size: 12px;
        font-weight: 600;
        color: var(--text-muted);
        letter-spacing: 0.5px;
        text-transform: uppercase;
      }
      
      /* Form Elements */
      .shiny-input-container {
        margin-bottom: 20px;
      }
      
      .shiny-input-container label {
        color: var(--text-muted) !important;
        font-weight: 600 !important;
        font-size: 13px !important;
        margin-bottom: 8px !important;
        display: block;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .form-control, .selectize-input {
        background: rgba(255,255,255,0.04) !important;
        border: 2px solid var(--border) !important;
        border-radius: 12px !important;
        color: var(--text) !important;
        padding: 12px 16px !important;
        font-size: 15px !important;
        transition: all 0.3s !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        background: rgba(255,255,255,0.06) !important;
        border-color: var(--accent-primary) !important;
        box-shadow: 0 0 0 4px rgba(0,217,255,0.1) !important;
        transform: translateY(-1px);
      }
      
      .selectize-dropdown {
        background: var(--bg-gradient-1) !important;
        border: 1px solid var(--border) !important;
        border-radius: 12px !important;
        margin-top: 4px !important;
        box-shadow: 0 10px 40px var(--shadow) !important;
      }
      
      .selectize-dropdown-content {
        color: var(--text) !important;
      }
      
      .selectize-dropdown .option {
        padding: 10px 16px !important;
        transition: all 0.2s !important;
      }
      
      .selectize-dropdown .option:hover,
      .selectize-dropdown .active {
        background: rgba(0,217,255,0.1) !important;
        color: var(--accent-primary) !important;
      }
      
      .selectize-input .item {
        color: var(--text) !important;  /* White: rgba(255,255,255,0.95) */
        font-weight: 500 !important;
      }
      
      .selectize-input input {
        color: var(--text) !important;  /* White while typing */
      }
      
      .selectize-input input::placeholder {
        color: var(--text-muted) !important;  /* Gray placeholder */
      }
      
      .selectize-dropdown .option {
        color: var(--text) !important;
      }

      /* Buttons */
      .btn-primary {
        background: linear-gradient(135deg, var(--accent-primary), var(--accent-secondary)) !important;
        border: none !important;
        border-radius: 14px !important;
        padding: 16px 32px !important;
        font-size: 16px !important;
        font-weight: 700 !important;
        letter-spacing: 0.5px !important;
        text-transform: uppercase !important;
        cursor: pointer !important;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
        width: 100%;
        position: relative;
        overflow: hidden;
        box-shadow: 0 10px 30px rgba(0,217,255,0.3) !important;
      }
      
      .btn-primary::before {
        content: '';
        position: absolute;
        top: 50%; left: 50%;
        width: 0; height: 0;
        border-radius: 50%;
        background: rgba(255,255,255,0.2);
        transform: translate(-50%, -50%);
        transition: width 0.6s, height 0.6s;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 15px 40px rgba(0,217,255,0.4) !important;
      }
      
      .btn-primary:hover::before {
        width: 300px;
        height: 300px;
      }
      
      .btn-primary:active {
        transform: translateY(0px) !important;
      }
      
      /* Winner Banner */
      .winner-banner {
        background: linear-gradient(135deg, rgba(52,211,153,0.2), rgba(0,217,255,0.2));
        border: 2px solid rgba(52,211,153,0.4);
        border-radius: 20px;
        padding: 24px;
        margin-bottom: 24px;
        position: relative;
        overflow: hidden;
        animation: slideIn 0.6s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      @keyframes slideIn {
        from {
          opacity: 0;
          transform: translateY(-20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .winner-banner::before {
        content: '';
        position: absolute;
        top: -50%; left: -50%;
        width: 200%; height: 200%;
        background: linear-gradient(45deg, transparent, rgba(255,255,255,0.05), transparent);
        animation: rotate 4s linear infinite;
      }
      
      @keyframes rotate {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }
      
      .winner-content {
        position: relative;
        z-index: 1;
      }
      
      .trophy-icon {
        font-size: 48px;
        margin-bottom: 12px;
        animation: bounce 2s infinite;
      }
      
      @keyframes bounce {
        0%, 100% { transform: translateY(0); }
        50% { transform: translateY(-10px); }
      }
      
      .winner-name {
        font-size: 32px;
        font-weight: 800;
        color: var(--accent-tertiary);
        margin-bottom: 8px;
        text-shadow: 0 2px 10px rgba(52,211,153,0.5);
      }
      
      .winner-prob {
        font-size: 16px;
        color: var(--text-muted);
        font-weight: 500;
      }
      
      /* Stats Grid */
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 10px;
        margin-top: 12px;
        margin-bottom: 20px;
      }
      
      .stat-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 12px 12px;
        text-align: center;
        transition: all 0.3s;
      }
      
      .stat-card:hover {
        background: rgba(255,255,255,0.05);
        transform: translateY(-4px);
        box-shadow: 0 5px 15px var(--shadow);
      }
      
      .stat-value {
        font-size: 28px;
        font-weight: 800;
        background: linear-gradient(135deg, var(--accent-primary), var(--accent-secondary));
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-bottom: 6px;
      }
      
      .stat-label {
        font-size: 11px;
        color: var(--text-muted);
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      /* Progress Bars */
      .progress-bar-container {
        margin: 24px 0;
        padding: 16px 0;
      }
      
      .progress-bar-container:not(:last-child) {
        border-bottom: 1px solid var(--border);
      }
      
      .progress-bar-label {
        display: flex;
        justify-content: space-between;
        margin-bottom: 8px;
        font-size: 14px;
        font-weight: 600;
      }
      
      .progress-bar-bg {
        height: 12px;
        background: rgba(255,255,255,0.05);
        border-radius: 10px;
        overflow: hidden;
        position: relative;
      }
      
      .progress-bar-fill {
        height: 100%;
        background: linear-gradient(90deg, var(--accent-primary), var(--accent-secondary));
        border-radius: 10px;
        transition: width 1s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .progress-bar-fill::after {
        content: '';
        position: absolute;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
        animation: shimmer 2s infinite;
      }
      
      @keyframes shimmer {
        0% { transform: translateX(-100%); }
        100% { transform: translateX(100%); }
      }
      
      /* Tables */
      .table {
        background: transparent !important;
        color: var(--text) !important;
        font-size: 14px;
      }
      
      .table thead th {
        background: rgba(255,255,255,0.04) !important;
        color: var(--text-muted) !important;
        border-bottom: 2px solid var(--border) !important;
        font-weight: 700 !important;
        text-transform: uppercase !important;
        font-size: 12px !important;
        letter-spacing: 0.5px !important;
        padding: 14px !important;
      }
      
      .table tbody td {
        background: transparent !important;
        color: var(--text) !important;
        border-top: 1px solid var(--border) !important;
        padding: 14px !important;
      }
      
      .table-striped > tbody > tr:nth-of-type(odd) {
        background: rgba(255,255,255,0.02) !important;
      }
      
      .table-hover > tbody > tr:hover {
        background: rgba(0,217,255,0.08) !important;
        cursor: pointer;
      }
      
      /* Split Layout */
      .split {
        display: flex;
        gap: 24px;
      }
      
      /* Player vs Player Header */
      .vs-header {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 24px;
        margin: 24px 0;
        padding: 20px;
        background: rgba(255,255,255,0.02);
        border-radius: 16px;
        border: 1px solid var(--border);
      }
      
      .vs-player {
        flex: 1;
        text-align: center;
        padding: 16px;
        background: rgba(0,217,255,0.05);
        border-radius: 12px;
        border: 1px solid rgba(0,217,255,0.2);
      }
      
      .vs-player.player2 {
        background: rgba(167,139,250,0.05);
        border-color: rgba(167,139,250,0.2);
      }
      
      .vs-player-name {
        font-size: 20px;
        font-weight: 700;
        margin-bottom: 8px;
      }
      
      .vs-player-stats {
        font-size: 13px;
        color: var(--text-muted);
      }
      
      .vs-divider {
        font-size: 24px;
        font-weight: 800;
        color: var(--accent-primary);
      }
      
      /* Responsive */
      @media(max-width: 992px) {
        .split { flex-direction: column; }
        .hero-title { font-size: 36px; }
        .hero-subtitle { font-size: 16px; }
      }
      
      /* Loading Animation */
      .loading-spinner {
        border: 3px solid rgba(255,255,255,0.1);
        border-top: 3px solid var(--accent-primary);
        border-radius: 50%;
        width: 40px;
        height: 40px;
        animation: spin 1s linear infinite;
        margin: 20px auto;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
  ),
  
  # Hero Header
  div(class = "hero-header",
      div(class = "hero-title", "⚡ ATP Match Predictor Pro"),
      div(class = "hero-subtitle", 
          "AI-Powered Tennis Match Predictions with Advanced Analytics"),
      div(class = "badge-group",
          span(class = "badge", "🤖 Machine Learning"),
          span(class = "badge purple", "📊 Historical Data"),
          span(class = "badge green", "🎾 70%+ Accuracy")
      )
  ),
  
  # Main Layout
  div(class = "split",
      # Left Column: Inputs
      div(style = "flex: 1;",
          # Player Selection
          div(class = "card",
              div(class = "card-header",
                  div(class = "card-title",
                      span(class = "card-icon", "👥"),
                      "Select Players"
                  ),
                  span(class = "pill", "Step 1")
              ),
              
              selectizeInput("p1_name", "🥇 Player 1", 
                             choices = NULL,
                             options = list(
                               placeholder = "Search for a player...",
                               maxOptions = 10
                             )),
              
              selectizeInput("p2_name", "🥈 Player 2", 
                             choices = NULL,
                             options = list(
                               placeholder = "Search for a player...",
                               maxOptions = 10
                             ))
          ),
          
          # Player Stats
          div(class = "card",
              div(class = "card-header",
                  div(class = "card-title",
                      span(class = "card-icon", "📊"),
                      "Player Statistics"
                  ),
                  span(class = "pill", "Step 2")
              ),
              
              tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                       numericInput("p1_rank", "Player 1 Rank", value = 1, min = 1),
                       numericInput("p2_rank", "Player 2 Rank", value = 2, min = 1)
              ),
              tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                       numericInput("p1_pts", "Player 1 Points", value = 11830, min = 0),
                       numericInput("p2_pts", "Player 2 Points", value = 7915, min = 0)
              ),
              tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                       numericInput("p1_age", "Player 1 Age", value = 23, min = 14, max = 60),
                       numericInput("p2_age", "Player 2 Age", value = 21, min = 14, max = 60)
              )
          ),
          
          # Match Context
          div(class = "card",
              div(class = "card-header",
                  div(class = "card-title",
                      span(class = "card-icon", "🏟️"),
                      "Match Context"
                  ),
                  span(class = "pill", "Step 3")
              ),
              
              tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                       selectInput("surface", "Surface", 
                                   choices = levels(data_template$surface), 
                                   selected = "Hard"),
                       selectInput("tourney_level", "Tournament", 
                                   choices = c("Grand Slam" = "G",
                                               "Masters 1000" = "M",
                                               "ATP 500" = "A",
                                               "ATP 250" = "250"),
                                   selected = "G")
              ),
              tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                       selectInput("best_of", "Best of", 
                                   choices = levels(data_template$best_of), 
                                   selected = "5"),
                       selectInput("round", "Round", 
                                   choices = levels(data_template$round), 
                                   selected = "F")
              ),
              numericInput("year", "Year", value = 2024, min = 2000, max = 2035),
              
              tags$br(),
              actionButton("go", "🔮 Predict Winner", class = "btn-primary")
          )
      ),
      
      # Right Column: Results
      div(style = "flex: 1.2;",
          # Winner Banner
          uiOutput("winner_banner"),
          
          # Quick Stats
          uiOutput("quick_stats"),
          
          # Model Predictions with Progress Bars
          div(class = "card",
              div(class = "card-header",
                  div(class = "card-title",
                      span(class = "card-icon", "🤖"),
                      "AI Predictions"
                  ),
                  span(class = "pill", "ML Models")
              ),
              uiOutput("predictions_viz")
          ),
          
          # Head-to-Head
          div(class = "card",
              div(class = "card-header",
                  div(class = "card-title",
                      span(class = "card-icon", "⚔️"),
                      "Head-to-Head Record"
                  ),
                  uiOutput("h2h_badge")
              ),
              uiOutput("h2h_summary"),
              tableOutput("h2h_surface")
          ),
          
          # Match History
          div(class = "card",
              div(class = "card-header",
                  div(class = "card-title",
                      span(class = "card-icon", "📜"),
                      "Recent Matches"
                  ),
                  span(class = "pill", "Last 10")
              ),
              tableOutput("h2h_table")
          )
      )
  )
)

server <- function(input, output, session) {

  player_choices <- sort(unique(c(matches$winner_name, matches$loser_name)))
  
  updateSelectizeInput(session, "p1_name",
                       choices = player_choices,
                       selected = character(0),
                       server = TRUE)
  
  updateSelectizeInput(session, "p2_name",
                       choices = player_choices,
                       selected = character(0),
                       server = TRUE)
  
  observeEvent(input$p1_name, ignoreInit = TRUE, {
    req(input$p1_name)
    current_p2 <- isolate(input$p2_name)
    new_choices <- setdiff(player_choices, input$p1_name)
    new_selected <- if (!is.null(current_p2) && current_p2 %in% new_choices) current_p2 else ""
    updateSelectizeInput(session, "p2_name", choices = new_choices, selected = new_selected, server = TRUE)
  })
  
  h2h_df <- reactive({
    req(input$p1_name, input$p2_name)
    p1 <- input$p1_name
    p2 <- input$p2_name
    req(nzchar(p1), nzchar(p2))
    validate(need(p1 != p2, "Pick two different players."))
    
    matches %>%
      filter((winner_name == p1 & loser_name == p2) | (winner_name == p2 & loser_name == p1)) %>%
      mutate(match_date = as.Date(as.character(tourney_date), format = "%Y%m%d")) %>%
      arrange(desc(match_date))
  })
  
  res <- eventReactive(input$go, {
    validate(
      need(input$p1_rank > 0 && input$p2_rank > 0, "Ranks must be positive."),
      need(input$p1_pts >= 0 && input$p2_pts >= 0, "Rank points must be ≥ 0."),
      need(input$p1_age >= 14 && input$p2_age >= 14, "Ages must be realistic (≥ 14).")
    )
    
    predict_match(
      p1_rank = input$p1_rank, p2_rank = input$p2_rank,
      p1_rank_points = input$p1_pts, p2_rank_points = input$p2_pts,
      p1_age = input$p1_age, p2_age = input$p2_age,
      surface = input$surface, tourney_level = input$tourney_level,
      best_of = as.integer(as.character(input$best_of)),
      round = input$round, year = input$year,
      glm_model = glm_model_full, rf_model = rf_full, data_template = data_template
    )
  })
  
  output$winner_banner <- renderUI({
    req(res())
    preds <- res()$predictions
    rf_row <- preds[preds$model == "Random Forest", ]
    
    winner_name <- ifelse(rf_row$predicted_winner == "Player 1", input$p1_name, input$p2_name)
    win_prob <- round(max(rf_row$prob_p1_win, 1 - rf_row$prob_p1_win) * 100, 1)
    
    div(class = "winner-banner",
        div(class = "winner-content",
            div(class = "trophy-icon", "🏆"),
            div(class = "winner-name", winner_name),
            div(class = "winner-prob", paste0("Predicted to win with ", win_prob, "% confidence"))
        )
    )
  })
  
  output$quick_stats <- renderUI({
    req(res())
    
    div(class = "stats-grid",
        div(class = "stat-card",
            div(class = "stat-value", paste0("#", input$p1_rank)),
            div(class = "stat-label", paste(input$p1_name, "Rank"))
        ),
        div(class = "stat-card",
            div(class = "stat-value", paste0("#", input$p2_rank)),
            div(class = "stat-label", paste(input$p2_name, "Rank"))
        ),
        div(class = "stat-card",
            div(class = "stat-value", input$surface),
            div(class = "stat-label", "Surface")
        ),
        div(class = "stat-card",
            div(class = "stat-value", input$round),
            div(class = "stat-label", "Round")
        )
    )
  })
  
  output$predictions_viz <- renderUI({
    req(res())
    preds <- res()$predictions
    
    tagList(
      lapply(1:nrow(preds), function(i) {
        row <- preds[i, ]
        prob <- round(row$prob_p1_win * 100, 1)
        
        div(class = "progress-bar-container",
            div(class = "progress-bar-label",
                tags$span(row$model),
                tags$span(style = "color: var(--accent-primary);", paste0(prob, "%"))
            ),
            div(class = "progress-bar-bg",
                div(class = "progress-bar-fill", style = paste0("width: ", prob, "%;"))
            )
        )
      })
    )
  })
  
  output$h2h_badge <- renderUI({
    req(h2h_df())
    span(class = "pill", paste(nrow(h2h_df()), "matches"))
  })
  
  output$h2h_summary <- renderUI({
    req(h2h_df())
    df <- h2h_df()
    p1 <- input$p1_name
    p2 <- input$p2_name
    
    p1_wins <- sum(df$winner_name == p1)
    p2_wins <- sum(df$winner_name == p2)
    
    div(class = "vs-header",
        div(class = "vs-player",
            div(class = "vs-player-name", style = "color: var(--accent-primary);", p1),
            div(class = "vs-player-stats", paste(p1_wins, "wins"))
        ),
        div(class = "vs-divider", "VS"),
        div(class = "vs-player player2",
            div(class = "vs-player-name", style = "color: var(--accent-secondary);", p2),
            div(class = "vs-player-stats", paste(p2_wins, "wins"))
        )
    )
  })
  
  output$h2h_surface <- renderTable({
    req(h2h_df())
    df <- h2h_df()
    p1 <- input$p1_name
    p2 <- input$p2_name
    
    df %>%
      group_by(surface) %>%
      summarise(
        `Total Matches` = n(),
        !!paste(p1, "Wins") := sum(winner_name == p1),
        !!paste(p2, "Wins") := sum(winner_name == p2),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Matches`))
  }, bordered = FALSE, striped = TRUE, hover = TRUE, spacing = "s")
  
  output$h2h_table <- renderTable({
    req(h2h_df())
    h2h_df() %>%
      transmute(
        Date = format(match_date, "%Y-%m-%d"),
        Tournament = tourney_name,
        Surface = surface,
        Round = round,
        Winner = winner_name,
        Score = score
      ) %>%
      head(10)
  }, bordered = FALSE, striped = TRUE, hover = TRUE, spacing = "s")
}

shinyApp(ui, server)