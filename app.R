#
# app.R
#

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(ggplot2)
library(lubridate)
library(bsicons)
library(tidyr)

# ----------------------
# 1) RAW DATA CREATION (quarter-logged)
# ----------------------

set.seed(123)
n_samples <- 5000

# Example raw dataset
adverse_events <- data.frame(
  date = sample(seq(as.Date('2008-01-01'), as.Date('2023-12-31'), by="day"), 
                n_samples, replace = TRUE),
  event_outcome = sample(c("Serious","Death","Non-Serious"),
                         n_samples, replace = TRUE, 
                         prob = c(0.5,0.1,0.4)),
  patient_gender = sample(c("Female","Male","Not Specified"),
                          n_samples, replace = TRUE,
                          prob = c(0.45,0.45,0.1)),
  reporter_type = sample(c("Consumer","Healthcare Professional","Not Specified","Other"),
                         n_samples, replace = TRUE),
  drug_name = sample(c("Acetaminophen","Ibuprofen","Aspirin","Lisinopril","Atorvastatin",
                       "Metformin","Omeprazole","Amlodipine","Metoprolol","Simvastatin",
                       "Levothyroxine","Gabapentin","Losartan","Sertraline","Fluoxetine"),
                     n_samples, replace = TRUE),
  drug_indication = sample(c("Pain","Fever","Hypertension","Hyperlipidemia","Diabetes",
                             "Acid Reflux","Hypothyroidism","Anxiety","Depression","Heart Disease"),
                           n_samples, replace = TRUE),
  reaction_term = sample(c("Nausea","Headache","Dizziness","Rash","Vomiting","Abdominal Pain",
                           "Fatigue","Diarrhea","Pruritus","Insomnia","Dyspnea","Chest Pain",
                           "Edema","Anaphylaxis","Liver Injury","Renal Dysfunction"),
                         n_samples, replace = TRUE),
  state = sample(c("Domestic","Not Specified"), n_samples, replace = TRUE, prob=c(0.7,0.3)),
  country = sample(c("Foreign","Not Specified"), n_samples, replace = TRUE, prob=c(0.2,0.8))
)

adverse_events$age_group <- sample(
  c("0-1 Month","2 Months-2 Years","3-11 Years","12-17 Years",
    "18-64 Years","65-85 Years","More than 85 Years","Not Specified"),
  n_samples, replace = TRUE
)

adverse_events$report_type <- sample(
  c("Expedited","Non-Expedited","Direct"),
  n_samples, replace = TRUE,
  prob = c(0.3,0.5,0.2)
)

# For raw data filters
adverse_events$year <- year(adverse_events$date)
adverse_events$quarter <- paste0("Q", quarter(adverse_events$date))

# ----------------------
# 2) CREATE QUARTER-BASED AGGREGATED DATA FRAMES (6 Dimensions)
# ----------------------
# We'll define quarters from 2015 Q1..2024 Q4 => 40 quarters
# row1 => "Total Reports," row2..41 => each quarter

# 1) Create actual Date sequence for quarters
quarter_seq <- seq.Date(as.Date("2015-01-01"), as.Date("2024-10-01"), by="quarter")

# 2) Build quarter labels manually, e.g. "2015 Q1", "2015 Q2", ...
quarter_labels <- sapply(quarter_seq, function(d) {
  paste0(year(d), " Q", quarter(d))
})

# Helper function
make_quarter_mock_df <- function(dimension_name, col_names,
                                 quarter_labels,
                                 seed=123,
                                 min_val=5000,
                                 max_val=100000){
  set.seed(seed)
  n_quarters <- length(quarter_labels)
  n_rows <- n_quarters + 1  # row1 => "Total Reports"
  n_cols <- length(col_names)
  df <- data.frame(matrix(nrow=n_rows, ncol=n_cols, 0), stringsAsFactors=FALSE)
  names(df) <- col_names
  
  df[[1]][1] <- "Total Reports"
  df[[2]][1] <- dimension_name
  
  for(i in seq_len(n_quarters)){
    df[[1]][i+1] <- quarter_labels[i]
    df[[2]][i+1] <- ""
  }
  
  for(r in 2:n_rows){
    for(c in 4:n_cols){
      df[r, c] <- sample(min_val:max_val, 1)
    }
  }
  
  # col3 => sum of columns 4..n_cols
  for(r in 2:n_rows){
    df[r, 3] <- sum(as.numeric(df[r, 4:n_cols]))
  }
  
  # row1 => sum of rows 2..n_rows for columns 3..n_cols
  for(c in 3:n_cols){
    df[1, c] <- sum(as.numeric(df[2:n_rows, c]))
  }
  df
}

# Define columns for each dimension
age_group_cols <- c("Quarter","Age Group","Total Reports",
                    "0-1 Month","2 Months-2 Years","3-11 Years","12-17 Years",
                    "18-64 Years","65-85 Years","More than 85 Years","Not Specified")

report_type_cols <- c("Quarter","Report Type","Total Reports",
                      "Expedited","Non-Expedited","Direct")

reporter_region_cols <- c("Quarter","Reporter Region","Total Reports",
                          "Domestic","Foreign","Not Specified")

reporter_cols <- c("Quarter","Reporter","Total Reports",
                   "Consumer","Healthcare Professional","Not Specified","Other")

seriousness_cols <- c("Quarter","Seriousness","Total Reports",
                      "Serious","Death","Non-Serious")

sex_cols <- c("Quarter","Sex","Total Reports",
              "Female","Male","Not Specified")

# Build the aggregator data frames
age_group_df <- make_quarter_mock_df("Age Group", age_group_cols, quarter_labels, seed=101)
report_type_df <- make_quarter_mock_df("Report Type", report_type_cols, quarter_labels, seed=102, max_val=500000)
reporter_region_df <- make_quarter_mock_df("Reporter Region", reporter_region_cols, quarter_labels, seed=103)
reporter_df <- make_quarter_mock_df("Reporter", reporter_cols, quarter_labels, seed=104)
seriousness_df <- make_quarter_mock_df("Seriousness", seriousness_cols, quarter_labels, seed=105)
sex_df <- make_quarter_mock_df("Sex", sex_cols, quarter_labels, seed=106)

quarter_agg_list <- list(
  "Age Group"       = age_group_df,
  "Report Type"     = report_type_df,
  "Reporter Region" = reporter_region_df,
  "Reporter"        = reporter_df,
  "Seriousness"     = seriousness_df,
  "Sex"             = sex_df
)

# We'll define 4 checkboxes for Q1..Q4
quarter_choices <- c("Q1","Q2","Q3","Q4")

# ----------------------
# 3) USER INTERFACE
# ----------------------
ui <- page_navbar(
  title = span(
    img(
      src="https://www.fda.gov/media/91293/download",
      height="30px",
      style="margin-right:10px;",
      alt="FDA Logo"
    ),
    "FDA Adverse Events Reporting System (FAERS) Dashboard"
  ),
  bg="#2c6aa0",
  theme=bs_theme(bootswatch="flatly", primary="#2c6aa0"),
  window_title="FAERS Dashboard",
  
  nav_spacer(),
  nav_panel("Disclaimer", icon=icon("exclamation-circle"),
            href="https://www.fda.gov/disclaimer", target="_blank"),
  nav_panel("FAQ", icon=icon("question-circle"),
            href="https://www.fda.gov/faqs", target="_blank"),
  nav_menu(
    title="Resources", icon=icon("book"),
    nav_panel("Report a Problem", icon=icon("flag"),
              href="https://www.fda.gov/safety/report-problem", target="_blank"),
    nav_panel("Site Feedback", icon=icon("comment"),
              href="https://www.fda.gov/about-fda/website-policies/site-feedback", target="_blank"),
    nav_panel("User Guide", icon=icon("file-alt"),
              href="https://www.fda.gov/consumers/consumer-updates", target="_blank")
  ),
  
  sidebar=sidebar(
    width=300,
    accordion(
      # PART A: RAW DATA FILTERS
      accordion_panel(
        "Data Filters (Raw)",
        icon=icon("filter"),
        dateRangeInput("dateRange","Select Date Range:",
                       start=as.Date("2008-01-01"), end=as.Date("2023-12-31"),
                       format="mm/dd/yyyy"),
        selectInput("quarter","Select Quarter:",
                    choices=c("All",sort(unique(adverse_events$quarter))),
                    selected="All"),
        selectInput("ageGroup","Age Group:",
                    choices=c("All",sort(unique(adverse_events$age_group))),
                    selected="All"),
        selectInput("gender","Gender:",
                    choices=c("All",sort(unique(adverse_events$patient_gender))),
                    selected="All"),
        selectInput("drugName","Drug/Product:",
                    choices=c("All",sort(unique(adverse_events$drug_name))),
                    selected="All"),
        selectInput("outcome","Event Outcome:",
                    choices=c("All",sort(unique(adverse_events$event_outcome))),
                    selected="All"),
        selectInput("reaction","Reaction:",
                    choices=c("All",sort(unique(adverse_events$reaction_term))),
                    selected="All"),
        selectInput("reporterType","Reporter Type:",
                    choices=c("All",sort(unique(adverse_events$reporter_type))),
                    selected="All"),
        selectInput("country","Country:",
                    choices=c("All",sort(unique(adverse_events$country))),
                    selected="All"),
        selectInput("state","State (US only):",
                    choices=c("All",sort(unique(adverse_events$state))),
                    selected="All")
      ),
      # PART B: QUARTER-BASED DIMENSION
      accordion_panel(
        "Dimension Aggregator (Quarters)",
        icon=icon("project-diagram"),
        selectInput("dimension","Dimension:",
                    choices=names(quarter_agg_list),
                    selected="Report Type"),
        sliderInput("yearRange","Select Year Range (Dimension):",
                    min=2015, max=2024,
                    value=c(2015,2024), step=1, sep=""),
        checkboxGroupInput("quarterFilter","Select Quarters (Dimension):",
                           choices=quarter_choices,
                           selected=quarter_choices, inline=TRUE)
      )
    ),
    hr(),
    actionButton("applyFilters","Apply Filters", class="btn-primary", width="100%"),
    br(), br(),
    actionButton("resetFilters","Reset All Filters", class="btn-outline-secondary", width="100%"),
    hr(),
    downloadButton("downloadData","Download Data", class="btn-info", width="100%"),
    br(), br(),
    p("Data displayed is for demo purposes only and does not represent actual FAERS data.",
      style="font-size:0.8em;color:#666;")
  ),
  
  nav_panel(
    title="Dashboard",
    icon=icon("chart-line"),
    layout_columns(
      value_box(
        title="Total Reports",
        value=textOutput("box_total_reports"),
        showcase=bs_icon("database"),
        theme="primary"
      ),
      value_box(
        title="Serious Reports (Excluding Death)",
        value=textOutput("box_serious_excl_death"),
        showcase=bs_icon("exclamation-circle"),
        theme="warning"
      ),
      value_box(
        title="Death Reports",
        value=textOutput("box_death_reports"),
        showcase=bs_icon("radioactive"),
        theme="danger"
      )
    ),
    layout_columns(
      card(
        card_header("Quarter-based Dimension Breakdown"),
        plotlyOutput("dimensionPlot", height="400px")
      )
    ),
    layout_columns(
      card(
        card_header("Aggregated Data (Preview)"),
        DTOutput("dimensionTable")
      )
    )
  ),
  
  nav_panel(
    title="Search",
    icon=icon("search"),
    card(
      card_header("Advanced Search (Raw Data)"),
      radioButtons("searchType","Search Type:",
                   choices=c("Search by Product","Search by Reaction"), inline=TRUE),
      uiOutput("ui_search_terms"),
      p("(Up to 5 items can be selected)"),
      actionButton("btn_search","Go", class="btn-primary"),
      br(), br(),
      DTOutput("searchResults")
    )
  ),
  
  nav_panel(
    title="Explore Data",
    icon=icon("database"),
    card(
      card_header("Filtered Raw Data"),
      DTOutput("filteredRawData")
    )
  ),
  
  nav_panel(
    title="Help",
    icon=icon("question"),
    card(
      card_header("How to Use This Dashboard"),
      layout_columns(
        card(
          card_header("Filtering Data"),
          p("Use 'Data Filters (Raw)' to filter the raw dataset by quarter, age group, etc. Use 'Dimension Aggregator (Quarters)' to select a dimension, year range, and which quarters to show.")
        ),
        card(
          card_header("Visualizations"),
          p("Value boxes & bar chart on the Dashboard come from the aggregator data (quarter-based). 'Explore Data' & 'Search' come from the raw data.")
        )
      )
    )
  )
)

# ----------------------
# 4) SERVER LOGIC
# ----------------------
server <- function(input, output, session) {
  
  # ---- PART A: RAW DATA FILTERS ----
  
  rv <- reactiveValues(
    dateRange=c(as.Date("2008-01-01"),as.Date("2023-12-31")),
    quarter="All",
    ageGroup="All",
    gender="All",
    drugName="All",
    outcome="All",
    reaction="All",
    reporterType="All",
    country="All",
    state="All"
  )
  
  observeEvent(input$applyFilters,{
    rv$dateRange <- input$dateRange
    rv$quarter   <- input$quarter
    rv$ageGroup  <- input$ageGroup
    rv$gender    <- input$gender
    rv$drugName  <- input$drugName
    rv$outcome   <- input$outcome
    rv$reaction  <- input$reaction
    rv$reporterType <- input$reporterType
    rv$country   <- input$country
    rv$state     <- input$state
  })
  
  observeEvent(input$resetFilters,{
    updateDateRangeInput(session,"dateRange",
                         start=as.Date("2008-01-01"),end=as.Date("2023-12-31"))
    updateSelectInput(session,"quarter",selected="All")
    updateSelectInput(session,"ageGroup",selected="All")
    updateSelectInput(session,"gender",selected="All")
    updateSelectInput(session,"drugName",selected="All")
    updateSelectInput(session,"outcome",selected="All")
    updateSelectInput(session,"reaction",selected="All")
    updateSelectInput(session,"reporterType",selected="All")
    updateSelectInput(session,"country",selected="All")
    updateSelectInput(session,"state",selected="All")
    
    rv$dateRange<-c(as.Date("2008-01-01"),as.Date("2023-12-31"))
    rv$quarter<-"All"
    rv$ageGroup<-"All"
    rv$gender<-"All"
    rv$drugName<-"All"
    rv$outcome<-"All"
    rv$reaction<-"All"
    rv$reporterType<-"All"
    rv$country<-"All"
    rv$state<-"All"
  })
  
  filteredRawData <- reactive({
    df <- adverse_events
    df <- df[df$date>=rv$dateRange[1] & df$date<=rv$dateRange[2],]
    if(rv$quarter!="All"){ df<-df[df$quarter==rv$quarter,] }
    if(rv$ageGroup!="All"){ df<-df[df$age_group==rv$ageGroup,] }
    if(rv$gender!="All"){ df<-df[df$patient_gender==rv$gender,] }
    if(rv$drugName!="All"){ df<-df[df$drug_name==rv$drugName,] }
    if(rv$outcome!="All"){ df<-df[df$event_outcome==rv$outcome,] }
    if(rv$reaction!="All"){ df<-df[df$reaction_term==rv$reaction,] }
    if(rv$reporterType!="All"){ df<-df[df$reporter_type==rv$reporterType,] }
    if(rv$country!="All"){ df<-df[df$country==rv$country,] }
    if(rv$state!="All"){ df<-df[df$state==rv$state,] }
    df
  })
  
  # Download raw data
  output$downloadData <- downloadHandler(
    filename=function(){ paste0("faers_raw_filtered_",Sys.Date(),".csv") },
    content=function(file){ write.csv(filteredRawData(), file, row.names=FALSE) }
  )
  
  output$filteredRawData <- renderDT({
    datatable(filteredRawData(), options=list(pageLength=10, autoWidth=TRUE, scrollX=TRUE))
  })
  
  # ---- PART B: QUARTER-BASED DIMENSION AGGREGATOR ----
  
  selectedAgg <- reactive({
    quarter_agg_list[[ input$dimension ]]
  })
  
  filteredAgg <- reactive({
    df <- selectedAgg()
    # remove row1 => "Total Reports"
    df <- df[df[[1]]!="Total Reports",]
    
    # parse the quarter label => e.g. "2015 Q1"
    # we want to keep only those rows whose year is in input$yearRange
    # and whose quarter is in input$quarterFilter
    parse_label <- function(lbl){
      parts <- strsplit(lbl," Q")[[1]]
      yr <- as.numeric(parts[1])
      q  <- paste0("Q", parts[2])
      list(year=yr, quarter=q)
    }
    
    keep_idx <- sapply(df[[1]], function(lbl){
      info <- parse_label(lbl)
      in_year <- (info$year >= input$yearRange[1]) && (info$year <= input$yearRange[2])
      in_q <- info$quarter %in% input$quarterFilter
      in_year && in_q
    })
    df <- df[keep_idx,]
    df
  })
  
  # 3 Value Boxes
  output$box_total_reports <- renderText({
    sum(as.numeric(filteredAgg()[[3]]))
  })
  output$box_serious_excl_death <- renderText({
    round(0.30 * sum(as.numeric(filteredAgg()[[3]])))
  })
  output$box_death_reports <- renderText({
    round(0.05 * sum(as.numeric(filteredAgg()[[3]])))
  })
  
  # Stacked bar chart => pivot columns 4..ncol
  output$dimensionPlot <- renderPlotly({
    df <- filteredAgg()
    if(nrow(df)==0) return(plot_ly() %>% layout(title="No data"))
    
    brk_cols <- names(df)[4:ncol(df)]
    df_long <- pivot_longer(df, cols=all_of(brk_cols),
                            names_to="Breakdown", values_to="Count")
    # rename column1 => "Quarter"
    names(df_long)[1] <- "Quarter"     # <--- fix
    names(df_long)[3] <- "Total_Reports"
    
    plot_ly(
      data=df_long,
      x=~Quarter,    # use the newly renamed "Quarter" column
      y=~Count,
      color=~Breakdown,
      type="bar",
      customdata=~paste(
        Quarter,         # quarter label
        Total_Reports,   # total
        Breakdown,       # breakdown category
        Count,
        sep="|"
      ),
      hovertemplate=paste0(
        "<b>Quarter: %{customdata.split('|')[0]}</b><br>",
        "Total Reports: %{customdata.split('|')[1]}<br>",
        "Category: %{customdata.split('|')[2]}<br>",
        "Count: %{customdata.split('|')[3]}<extra></extra>"
      )
    ) %>%
      layout(
        barmode="stack",
        title=paste("Quarter-based", input$dimension, "Breakdown"),
        xaxis=list(
          title="Quarter",
          tickangle=-45
        ),
        yaxis=list(title="Report Count")
      )
  })
  
  output$dimensionTable <- renderDT({
    datatable(filteredAgg(), options=list(pageLength=10, autoWidth=TRUE, scrollX=TRUE))
  })
  
  # ---- PART C: SEARCH (RAW DATA) ----
  
  output$ui_search_terms <- renderUI({
    if(input$searchType=="Search by Product"){
      selectizeInput(
        "searchProducts",
        label=NULL,
        choices=sort(unique(adverse_events$drug_name)),
        multiple=TRUE,
        options=list(maxItems=5,placeholder="Type or select products...")
      )
    } else {
      selectizeInput(
        "searchReactions",
        label=NULL,
        choices=sort(unique(adverse_events$reaction_term)),
        multiple=TRUE,
        options=list(maxItems=5,placeholder="Type or select reactions...")
      )
    }
  })
  
  searchResultsData <- reactiveVal(data.frame())
  
  observeEvent(input$btn_search,{
    if(input$searchType=="Search by Product"){
      prods <- input$searchProducts
      if(length(prods)>0){
        srch <- adverse_events %>% filter(drug_name %in% prods)
        searchResultsData(srch)
      } else {
        searchResultsData(data.frame())
      }
    } else {
      reacts <- input$searchReactions
      if(length(reacts)>0){
        srch <- adverse_events %>% filter(reaction_term %in% reacts)
        searchResultsData(srch)
      } else {
        searchResultsData(data.frame())
      }
    }
  })
  
  output$searchResults <- renderDT({
    df <- searchResultsData()
    if(nrow(df)==0){
      datatable(data.frame(Message="No results"), options=list(dom='t'))
    } else {
      datatable(df, options=list(pageLength=10, autoWidth=TRUE, scrollX=TRUE))
    }
  })
}

shinyApp(ui, server)