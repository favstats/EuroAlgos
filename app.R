library(shiny)
library(dplyr)
library(highcharter)
library(glue)
library(forcats)
library(shinythemes)
library(bslib)

# Data preparation (replace this with your actual data source)
dt <- readRDS("data/right_plot_data.rds") %>%
  mutate(party = ifelse(party == "GroenLinks-PvdA", "GL-PvdA", party)) 
age_dat <- readRDS("data/age_dat.rds") %>%
  mutate(party = ifelse(party == "GroenLinks-PvdA", "GL-PvdA", party)) 
gender_dat <- readRDS("data/gender_dat.rds") %>%
  mutate(party = ifelse(party == "GroenLinks-PvdA", "GL-PvdA", party)) 
region_dat <- readRDS("data/region_dat.rds") %>%
  mutate(party = ifelse(party == "GroenLinks-PvdA", "GL-PvdA", party)) 

# Define custom theme using bslib
my_theme <- bs_theme(
  version = 4,
  bootswatch = "flatly",  # Dashboard-like theme
  primary = "#2c3e50",   # Dark Blue
  secondary = "#18bc9c", # Greenish
  success = "#18bc9c",   # Matching primary
  warning = "#f39c12",   # Orange
  danger = "#e74c3c",    # Red
  base_font = "Roboto",    # Replacing Montserrat with Roboto
  heading_font = "Roboto"  # Clean, modern font
)

# Define UI for the app with a navbar
ui <- navbarPage(
  title = "Political Ad Spending Dashboard",  # Navbar title
  theme = my_theme,  # Apply custom theme
  # Custom CSS for extra styling
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #2c3e50; border-bottom: 2px solid #f7f7f7; }
      .navbar-brand { color: white !important; font-weight: bold; }
      body { font-family: 'Roboto', sans-serif; }
      .sidebar { background-color: #f7f7f7; padding: 15px; }
      .main-panel { padding: 20px; }
      .selectInput, .highchartOutput { margin-bottom: 20px; }
      .shiny-input-container { font-size: 16px; }
      .navbar { padding-left: 15px; padding-right: 15px; }
      .h1, .h2, .h3, .h4, .h5, .h6 { color: #2c3e50; }
    "))
  ),
  
  # Sidebar layout with input and output
  tabPanel("Exploration",  # Default tab
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("plot_type", "Select Plot Type", 
                           choices = c("Overall differences", "By age", "By gender", "By geography")),
               selectInput("country", "Select Country", 
                           choices = unique(dt$cntry), 
                           selected = "Germany"),
               uiOutput("party_ui"),  # Dynamic UI for party selection
               uiOutput("age_group_ui"),  # Dynamic UI for age group selection (only visible when By age is selected)
               uiOutput("gender_group_ui"),  # Dynamic UI for gender group selection (only visible when By gender is selected)
               uiOutput("region_group_ui")   # Dynamic UI for region selection (only visible when By geography is selected)
             ),
             
             # Main panel to display the chart
             mainPanel(
               class = "main-panel",  # Add custom class for styling
               highchartOutput("spendingChart", height = "500px")
             )
           )
  )
)

# Define server logic to generate chart based on inputs
server <- function(input, output, session) {
  
  # Reactive expression to filter available parties based on selected country
  filtered_parties <- reactive({
    dt %>%
      filter(cntry == input$country) %>%
      distinct(party)
  })
  
  # Dynamically update the party input based on the selected country
  output$party_ui <- renderUI({
    selectInput("party", "Select BASELINE Party", 
                choices = filtered_parties()$party, 
                selected = filtered_parties()$party[1])
  })
  
  # Dynamically show the age group selector only when "By age" is selected
  output$age_group_ui <- renderUI({
    if (input$plot_type == "By age") {
      selectInput("age_group", "Select Age Group", 
                  choices = unique(age_dat$age), 
                  selected = "18-24")
    }
  })
  
  output$gender_group_ui <- renderUI({
    if (input$plot_type == "By gender") {
      selectInput("gender_group", "Select Gender", 
                  choices = unique(gender_dat$gender), 
                  selected = "female")
    }
  })
  
  output$region_group_ui <- renderUI({
    if (input$plot_type == "By geography") {
      # Filter regions based on the selected country
      available_regions <- region_dat %>%
        filter(cntry == input$country) %>%
        distinct(region)
      
      # Check if any regions are available for the selected country
      if (nrow(available_regions) == 0) {
        return(selectInput("region_group", "Select Region", choices = "No regions available"))
      }
      
      # Render the region selection UI with available regions
      selectInput("region_group", "Select Region", 
                  choices = available_regions$region, 
                  selected = available_regions$region[1])
    }
  })
  
  
  # Function to generate the highchart based on inputs
  create_hc <- function(partei, land, plot_type, age_group = NULL, gender_group = NULL, region_group = NULL) {
    # print(age_group)
    # Get the target party data for comparison
    target_party <- dt %>% 
      filter(party == partei, cntry == land)
    
    # Check if there's any data available for the selected party and country
    if (nrow(target_party) == 0) {
      return(highchart() %>%
               hc_title(text = "No data available for the selected party and country"))
    }
    
    # Define the custom color palette for each audience
    audience_colors <- c(
      "Education ad" = "#1f77b4",  # Blue
      "No Targeting ad" = "#2ca02c",  # Green
      "Politics ad" = "#ff7f0e"  # Orange
    )
    
    # Create the filtered dataset
    filtered_data <- dt %>%
      filter(party != partei, cntry == land)
    
    # Check if filtered data has rows (to prevent mutate errors)
    if (nrow(filtered_data) == 0) {
      return(highchart() %>%
               hc_title(text = "No comparison data available for the selected party and country"))
    }
    
    

    # Generate different charts based on the selected plot type
    if (plot_type == "Overall differences") {
      fluff <- target_party %>%
        select(-party) %>%
        select(-cntry) %>% 
        rename(overall_price = price)
      filtered_data <- filtered_data %>%
        group_by(party, audience, cntry) %>% 
        summarize(price = mean(price)) %>% 
        dplyr::left_join(fluff,by = "audience") %>%
        mutate(price_diff = round((overall_price - price) / overall_price * 100, 2)) %>%
        mutate(price_diff_abs = abs(price_diff)) %>%
        mutate(more_or_less = ifelse(price_diff < 0, "less", "more")) %>%
        mutate(party = fct_reorder(party, price_diff)) %>%
        arrange(party)
      
      # Create a base chart
      base_chart <- highchart() %>%
        hc_chart(inverted = TRUE) %>%
        hc_xAxis(
          type = "category",
          title = list(text = "Political Parties")
        ) %>%
        hc_yAxis(
          title = list(text = glue::glue("Price Difference Compared to '{partei}' (%)")),
          labels = list(format = "{value}%"),
          plotLines = list(list(
            value = 0,
            color = "black",
            dashStyle = "Dash",
            width = 2
          ))
        ) %>%
        hc_title(text = glue::glue("Price Differences for Ads by Party in {land}")) %>%
        hc_tooltip(
          useHTML = TRUE,
          pointFormat = paste0(
            "<b>", partei, "</b> spent <b>{point.price_diff}%</b> {point.more_or_less} on <b>{series.name}</b> compared to <b>{point.party}</b>.<br>",
            "Absolute price difference: <b>{point.price_diff_abs}%</b><br>",
            "Average price difference: <b>", round(mean(abs(filtered_data$price_diff)), 2), "%</b>"
          )
        )
      
      # Add series for each audience type
      for (aud in unique(filtered_data$audience)) {
        audience_data <- filtered_data %>% filter(audience == aud)
        
        base_chart <- base_chart %>%
          hc_add_series(
            data = audience_data,
            type = "lollipop",
            hcaes(x = party, low = price_diff),
            name = aud,
            color = audience_colors[[aud]]
          )
      }
      
      return(base_chart %>%
               hc_legend(enabled = TRUE))
      # age_dat %>%
      #   filter(age == "18-24" & party == "CDU")
    } else if (plot_type == "By age") {
      
      
      create_age_chart <- function(partei, land, age_group) {
        
        # Filter data for the target party, country, and age group
        target_party_age <- age_dat %>%
          filter(party == partei & cntry == land & age == age_group)
        
        # Check if there's any data available for the selected combination
        if (nrow(target_party_age) == 0) {
          return(highchart() %>%
                   hc_title(text = "No data available for the selected age group, party, and country"))
        }
        
        # Define the custom color palette for each audience
        audience_colors <- c(
          "Education ad" = "#1f77b4",  # Blue
          "No Targeting ad" = "#2ca02c",  # Green
          "Politics ad" = "#ff7f0e"  # Orange
        )
        
        # Prepare fluff for joining with the filtered data
        fluff <- target_party_age %>%
          group_by(party, audience, cntry, age) %>%
          summarize(price = mean(price)) %>%
          ungroup() %>% 
          select(-party, -cntry, -age) %>%
          rename(overall_price = price)
        
        # Filter the rest of the dataset excluding the target party and age group
        filtered_data <- age_dat %>%
          filter(party != partei & cntry == land & age == age_group) %>%
          group_by(party, audience, cntry, age) %>%
          summarize(price = mean(price)) %>%
          ungroup() %>% 
          dplyr::left_join(fluff, by = "audience") %>%
          mutate(price_diff = round((overall_price - price) / overall_price * 100, 2)) %>%
          mutate(price_diff_abs = abs(price_diff)) %>%
          mutate(more_or_less = ifelse(price_diff < 0, "less", "more")) %>%
          mutate(party = fct_reorder(party, price_diff)) %>%
          arrange(party)
        
        # Check if filtered data has rows (to prevent mutate errors)
        if (nrow(filtered_data) == 0) {
          return(highchart() %>%
                   hc_title(text = "No comparison data available for the selected age group"))
        }
        
        # Create the base chart similar to the overall chart logic
        base_chart <- highchart() %>%
          hc_chart(inverted = TRUE) %>%
          hc_xAxis(
            type = "category",
            title = list(text = "Political Parties by Age Group")
          ) %>%
          hc_yAxis(
            title = list(text = glue::glue("Price Difference Compared to '{partei}' (%) for Age Group {age_group}")),
            labels = list(format = "{value}%"),
            plotLines = list(list(
              value = 0,
              color = "black",
              dashStyle = "Dash",
              width = 2
            ))
          ) %>%
          hc_title(text = glue::glue("Price Differences for Ads by Party in {land} for Age Group {age_group}")) %>%
          hc_tooltip(
            useHTML = TRUE,
            pointFormat = paste0(
              "<b>", partei, "</b> spent <b>{point.price_diff}%</b> {point.more_or_less} on <b>{series.name}</b> for the age group <b>",age_group ,"</b> compared to <b>{point.party}</b>.<br>",
              "Absolute price difference: <b>{point.price_diff_abs}%</b><br>",
              "Average price difference: <b>", round(mean(abs(filtered_data$price_diff)), 2), "%</b>"
            )
          )
        
        # Add series for each audience type, same as overall logic
        for (aud in unique(filtered_data$audience)) {
          audience_data <- filtered_data %>% filter(audience == aud)
          
          base_chart <- base_chart %>%
            hc_add_series(
              data = audience_data,
              type = "lollipop",
              hcaes(x = party, low = price_diff),
              name = aud,
              color = audience_colors[[aud]]
            )
        }
        
        return(base_chart %>%
                 hc_legend(enabled = TRUE))
      }
      
      base_chart <- create_age_chart(partei, land, age_group)
      
      return(base_chart)
      
    } else if (plot_type == "By gender") {

      
      # Define the gender-based chart logic
      create_gender_chart <- function(partei, land, gender_group) {
        
        # Filter data for the target party, country, and gender group
        target_party_gender <- gender_dat %>%
          filter(party == partei & cntry == land & gender == gender_group)
        
        # Check if there's any data available for the selected combination
        if (nrow(target_party_gender) == 0) {
          return(highchart() %>%
                   hc_title(text = "No data available for the selected gender group, party, and country"))
        }
        
        # Define the custom color palette for each audience
        audience_colors <- c(
          "Education ad" = "#1f77b4",  # Blue
          "No Targeting ad" = "#2ca02c",  # Green
          "Politics ad" = "#ff7f0e"  # Orange
        )
        
        # Prepare fluff for joining with the filtered data
        fluff <- target_party_gender %>%
          group_by(party, audience, cntry, gender) %>%
          summarize(price = mean(price)) %>% 
          ungroup() %>% 
          select(-party, -cntry, -gender) %>%
          rename(overall_price = price) 
        
        # Filter the rest of the dataset excluding the target party and gender group
        filtered_data <- gender_dat %>%
          filter(party != partei & cntry == land & gender == gender_group) %>%
          group_by(party, audience, cntry, gender) %>%
          summarize(price = mean(price)) %>%
          dplyr::left_join(fluff, by = "audience") %>%
          mutate(price_diff = round((overall_price - price) / overall_price * 100, 2)) %>%
          mutate(price_diff_abs = abs(price_diff)) %>%
          mutate(more_or_less = ifelse(price_diff < 0, "less", "more")) %>%
          mutate(party = fct_reorder(party, price_diff)) %>%
          arrange(party)
        
        # Check if filtered data has rows (to prevent mutate errors)
        if (nrow(filtered_data) == 0) {
          return(highchart() %>%
                   hc_title(text = "No comparison data available for the selected gender group"))
        }
        
        # Create the base chart similar to the overall chart logic
        base_chart <- highchart() %>%
          hc_chart(inverted = TRUE) %>%
          hc_xAxis(
            type = "category",
            title = list(text = "Political Parties by Gender Group")
          ) %>%
          hc_yAxis(
            title = list(text = glue::glue("Price Difference Compared to '{partei}' (%) for Gender Group {gender_group}")),
            labels = list(format = "{value}%"),
            plotLines = list(list(
              value = 0,
              color = "black",
              dashStyle = "Dash",
              width = 2
            ))
          ) %>%
          hc_title(text = glue::glue("Price Differences for Ads by Party in {land} for Gender Group {gender_group}")) %>%
          hc_tooltip(
            useHTML = TRUE,
            pointFormat = paste0(
              "<b>", partei, "</b> spent <b>{point.price_diff}%</b> {point.more_or_less} on <b>{series.name}</b> for the gender <b>",gender_group, "</b> compared to <b>{point.party}</b>.<br>",
              "Absolute price difference: <b>{point.price_diff_abs}%</b><br>",
              "Average price difference: <b>", round(mean(abs(filtered_data$price_diff)), 2), "%</b>"
            )
          )
        
        # Add series for each audience type, same as overall logic
        for (aud in unique(filtered_data$audience)) {
          audience_data <- filtered_data %>% filter(audience == aud)
          
          base_chart <- base_chart %>%
            hc_add_series(
              data = audience_data,
              type = "lollipop",
              hcaes(x = party, low = price_diff),
              name = aud,
              color = audience_colors[[aud]]
            )
        }
        
        return(base_chart %>%
                 hc_legend(enabled = TRUE))
      }
      
      base_chart <- create_gender_chart(partei, land, gender_group)
      
      return(base_chart)
      
    } else if (plot_type == "By geography") {

      
      
      # Define the region-based chart logic
      create_region_chart <- function(partei, land, region_group) {
        
        # Filter data for the target party, country, and region group
        target_party_region <- region_dat %>%
          filter(party == partei & cntry == land & region == region_group)
        
        # Check if there's any data available for the selected combination
        if (nrow(target_party_region) == 0) {
          return(highchart() %>%
                   hc_title(text = "No data available for the selected region group, party, and country"))
        }
        
        # Define the custom color palette for each audience
        audience_colors <- c(
          "Education ad" = "#1f77b4",  # Blue
          "No Targeting ad" = "#2ca02c",  # Green
          "Politics ad" = "#ff7f0e"  # Orange
        )
        
        # Prepare fluff for joining with the filtered data
        fluff <- target_party_region %>%
          group_by(party, audience, cntry, region) %>%
          summarize(price = mean(price)) %>% 
          ungroup() %>% 
          select(-party, -cntry, -region) %>%
          rename(overall_price = price) 
        
        # Filter the rest of the dataset excluding the target party and region group
        filtered_data <- region_dat %>%
          filter(party != partei & cntry == land & region == region_group) %>%
          group_by(party, audience, cntry, region) %>%
          summarize(price = mean(price)) %>%
          dplyr::left_join(fluff, by = "audience") %>%
          mutate(price_diff = round((overall_price - price) / overall_price * 100, 2)) %>%
          mutate(price_diff_abs = abs(price_diff)) %>%
          mutate(more_or_less = ifelse(price_diff < 0, "less", "more")) %>%
          mutate(party = fct_reorder(party, price_diff)) %>%
          arrange(party)
        
        # Check if filtered data has rows (to prevent mutate errors)
        if (nrow(filtered_data) == 0) {
          return(highchart() %>%
                   hc_title(text = "No comparison data available for the selected region group"))
        }
        
        # Create the base chart similar to the overall chart logic
        base_chart <- highchart() %>%
          hc_chart(inverted = TRUE) %>%
          hc_xAxis(
            type = "category",
            title = list(text = "Political Parties by region Group")
          ) %>%
          hc_yAxis(
            title = list(text = glue::glue("Price Difference Compared to '{partei}' (%) for region Group {region_group}")),
            labels = list(format = "{value}%"),
            plotLines = list(list(
              value = 0,
              color = "black",
              dashStyle = "Dash",
              width = 2
            ))
          ) %>%
          hc_title(text = glue::glue("Price Differences for Ads by Party in {land} for region Group {region_group}")) %>%
          hc_tooltip(
            useHTML = TRUE,
            pointFormat = paste0(
              "<b>", partei, "</b> spent <b>{point.price_diff}%</b> {point.more_or_less} on <b>{series.name}</b> for the region <b>", region_group, "</b> compared to <b>{point.party}</b>.<br>",
              "Absolute price difference: <b>{point.price_diff_abs}%</b><br>",
              "Average price difference: <b>", round(mean(abs(filtered_data$price_diff)), 2), "%</b>"
            )
          )
        
        # Add series for each audience type, same as overall logic
        for (aud in unique(filtered_data$audience)) {
          audience_data <- filtered_data %>% filter(audience == aud)
          
          base_chart <- base_chart %>%
            hc_add_series(
              data = audience_data,
              type = "lollipop",
              hcaes(x = party, low = price_diff),
              name = aud,
              color = audience_colors[[aud]]
            )
        }
        
        return(base_chart %>%
                 hc_legend(enabled = TRUE))
      }
      
      base_chart <- create_region_chart(partei, land, region_group)
      
      return(base_chart)
      
      
    }
  }
  
  # Render the chart in the main panel based on selected inputs
  output$spendingChart <- renderHighchart({
    create_hc(input$party, input$country, input$plot_type, input$age_group, input$gender_group, input$region_group)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
