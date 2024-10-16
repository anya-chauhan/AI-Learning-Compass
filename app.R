# Core Shiny and dashboard libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

# Data manipulation and graph libraries
library(dplyr)
library(igraph)
library(visNetwork)

# Visualization libraries
library(ggplot2)
library(plotly)

# Color libraries
library(RColorBrewer)
library(viridis)
library(scales)

# gpt libraries
library(httr)
library(jsonlite)
library(stringr)

#step 1: Set up openai
# Set your OpenAI API key
api_key <- "api-key-goes-here"

# Function to send a request to the OpenAI API
get_gpt_response <- function(prompt) {
  url <- "https://api.openai.com/v1/chat/completions"
  
  # Create the body of the request
  body <- list(
    model = "gpt-3.5-turbo",  
    messages = list(list(role = "user", content = prompt)),
    max_tokens = 300  # Adjust token limit as needed
  )
  
  # Make the API request
  response <- POST(
    url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  # Check if the request was successful
  if (response$status_code != 200) {
    print(paste("Error: GPT API request failed with status", response$status_code))
    return("Error: GPT API request failed.")
  }
  
  # Print the raw response before parsing it
  raw_response <- content(response, as = "text", encoding = "UTF-8")
  
  # Clean up the raw response using jsonlite::prettify
  pretty_response <- jsonlite::prettify(raw_response)
  
  pattern <- '"content":\\s*"(.*?)",'
  match <- str_match(pretty_response, pattern)
  
  if (!is.na(match[1,2])) {
    message_content <- match[1,2]
    
    # Unescape the content (replace \" with ")
    message_content <- gsub('\\\\"', '"', message_content)
    
    # Print the extracted message
    cat("Extracted message:\n\n", message_content, "\n")
  } else {
    cat("No match found. Check if the JSON structure is as expected.\n")
  }
  return(message_content)
}

get_related_nodes <- function(kg_data, node) {
  # This is a placeholder - implement based on your KG structure
  # For example, if kg_data is a data frame with 'from' and 'to' columns:
  related <- kg_data$to[kg_data$from == node]
  return(unique(related))
}


# Step 2: Loading Our own datasets for graph
df <- read.csv('ML-data.csv', sep=",", header=TRUE, stringsAsFactors=FALSE)
kg <- read.csv('ML_Relationships.csv', sep=",", header=TRUE, stringsAsFactors=FALSE)

create_graph <- function(df, kg) {
  # Create an empty graph
  g <- make_empty_graph(directed = TRUE)
  
  # Add nodes and edges from kg DataFrame
  for (i in 1:nrow(kg)) {
    source <- kg[i, "Source.Node"]
    target <- kg[i, "Target.Node"]
    relationship <- kg[i, "Relationship"]
    source_attr <- kg[i, "Source.Attribute"]
    target_attr <- kg[i, "Target.Attribute"]
    
    # Add vertices only if they do not already exist in the graph
    if (!(source %in% V(g)$name)) {
      g <- g + vertex(source, type = source_attr)
    }
    if (!(target %in% V(g)$name)) {
      g <- g + vertex(target, type = target_attr)
    }
    
    # Add an edge if it does not already exist
    if (!(are_adjacent(g, source, target))) {
      g <- g + edge(source, target, relationship = relationship)
    }
  }
  
  # Optionally, integrate additional attributes from ML-final.csv
  for (i in 1:nrow(df)) {
    skill <- df[i, "Skill"]
    if (skill %in% V(g)$name) {
      V(g)[skill]$description <- df[i, "Description"]
      V(g)[skill]$top_companies <- df[i, "TopCompanies"]
    }
  }
  
  # Return the constructed graph
  return(g)
}

# Create the graph
graph_data <- create_graph(df, kg)

# Define the function to extract node names
get_node_names <- function(g) {
  return(V(g)$name)  # Extract and return node names from the graph
}

# Extract node names from the graph
node_names <- get_node_names(graph_data)  # Get the node names


# Step 3: UI
ui <- dashboardPage(
  dashboardHeader(title = "AI Learning Compass"),
  dashboardSidebar(
    tags$style(HTML("
      .sidebar-menu { padding-top: 20px; }
      .sidebar-toggle:hover { background-color: #1A5F7A !important; }
    ")),
    sidebarMenu(
      selectInput("graph_source", "Select Source:",
                  choices = c("Machine Learning Skills", "Custom Text Input", "URL Input")),
      conditionalPanel(
        condition = "input.graph_source == 'Custom Text Input'",
        textAreaInput("text_input", NULL, placeholder="Work in progress: exciting updates coming soon!", rows = 5),
        actionButton("process_text", "Process Text", class = "btn-success")
      ),
      conditionalPanel(
        condition = "input.graph_source == 'URL Input'",
        textInput("url_input", "Enter URL:"),
        actionButton("process_url", "Process URL", class = "btn-info")
      ),
      tags$div(style = "margin-top: 40px;"),
      tags$hr(),  # Add a horizontal line to create a break
      
      # Task selection
      radioButtons("info_options", "Select Information to Display:",
                   choices = c(
                     "Overview" = "overview",
                     "Article Links" = "article_links",
                     "Project Links" = "project_links",
                     "Generate Project" = "generate_project",
                     "Test Skill" = "test_skill",
                     "Market Insights" = "market_insights"
                   ),
                   selected = "overview"
      ),
      tags$div(style = "margin-top: 20px;"),
      
      # Customization options per task using conditionalPanel
      
      # For 'Overview' task
      conditionalPanel(
        condition = "input.info_options == 'overview'",
        selectInput("detail_level", "Level of Detail:",
                    choices = c("Concise" = "brief",
                                "Intermediate" = "moderate",
                                "Detailed" = "detailed"),
                    selected = "brief")
      ),
      
      # For 'Article Links' task
      conditionalPanel(
        condition = "input.info_options == 'article_links'",
        selectInput("article_source", "Select Source:",
                    choices = c("All Sources" = "all",
                                "Medium" = "medium",
                                "arXiv" = "arxiv",
                                "IEEE" = "ieee",
                                "Academic Journals" = "journals"),
                    selected = "all"),
        selectInput("time_frame", "Time Frame:",
                    choices = c("Last Month" = "1m",
                                "Last Year" = "1y",
                                "All Time" = "all"),
                    selected = "all"),
        numericInput("num_articles", "Number of Articles:",
                     value = 2, min = 1, max = 5)
      ),
      
      # For 'Project Links' task
      conditionalPanel(
        condition = "input.info_options == 'project_links'",
        selectInput("project_type", "Project Source:",
                    choices = c("Kaggle" = "kaggle",
                                "STEM-Away" = "stemaway"),
                                selected = "kaggle"),
        selectInput("project_difficulty", "Project Difficulty Level:",
                    choices = c("All Levels" = "all",
                                "Beginner" = "beginner",
                                "Intermediate" = "intermediate",
                                "Advanced" = "advanced"),
                    selected = "all"),
      ),
      
      # For 'Generate Project' task
      conditionalPanel(
        condition = "input.info_options == 'generate_project'",
        selectInput("generate_project_difficulty", "Project Difficulty Level:",
                    choices = c("Beginner" = "beginner",
                                "Intermediate" = "intermediate",
                                "Advanced" = "advanced"),
                    selected = "intermediate"),
        selectInput("time_commitment", "Time Commitment:",
                    choices = c("Less than a week" = "short",
                                "1-2 weeks" = "medium",
                                "1 month or more" = "long"),
                    selected = "medium")
      ),
      
      # For 'Test Skill' task
      conditionalPanel(
        condition = "input.info_options == 'test_skill'",
        selectInput("quiz_difficulty_level", "Quiz Difficulty Level:",
                    choices = c("Easy" = "easy",
                                "Medium" = "medium",
                                "Hard" = "hard"),
                    selected = "medium"),
        numericInput("num_quiz_questions", "Number of Questions:",
                     value = 3, min = 1, max = 6)
      ),
      
      # For 'Market Insights' task
      conditionalPanel(
        condition = "input.info_options == 'market_insights'",
        selectInput("industry", "Select Industry:",
                    choices = c("All Industries" = "all",
                                "Healthcare" = "healthcare",
                                "Finance" = "finance",
                                "Technology" = "technology",
                                "Manufacturing" = "manufacturing"),
                    selected = "all"),
        selectInput("geographic_region", "Geographic Region:",
                    choices = c("Global" = "global",
                                "North America" = "na",
                                "Europe" = "europe",
                                "Asia" = "asia"),
                    selected = "global"),
        selectInput("experience_level", "Experience Level:",
                    choices = c("Entry-Level" = "entry",
                                "Mid-Level" = "mid",
                                "Senior" = "senior"),
                    selected = "entry")
      ),
      
      tags$div(style = "margin-top: 40px;"),
      tags$hr(),  # Separator line
      
      # Legend with adjusted font size and spacing
      h4("Legend", style = "font-size: 16px; margin-left: 20px;"),
      tags$div(style = "margin-bottom: 10px;"),  # Add spacing between sections
      tags$div(
        tags$ul(
          tags$li(tags$span(style = "color:#8e63ff; font-size:14px;", "Main Category")),
          tags$li(tags$span(style = "color:#e6c200; font-size:14px;", "Core Concepts")),
          tags$li(tags$span(style = "color:#e07b3f; font-size:14px;", "Algorithms & Models")),
          tags$li(tags$span(style = "color:#1eb5f0; font-size:14px;", "Programming")),
          tags$li(tags$span(style = "color:#00b04f; font-size:14px;", "Metrics")),
          tags$li(tags$span(style = "color:#ff6666; font-size:14px;", "Applications"))
        ),
        style = "padding-left: 10px; font-size:14px;"  # Added padding and font size for the legend
      )
    )
  ),
    
  dashboardBody(
    tags$head(
      tags$style(HTML('
        /* Custom CSS for dark blue gradient theme */
        .skin-blue .main-header .logo { background-color: #000 !important; }
        .skin-blue .main-header .navbar { 
          background: linear-gradient(120deg, #000, #2c2c2c) !important;
        }
        .skin-blue .main-sidebar { 
          background: linear-gradient(180deg, #000, #2c2c2c) !important;
        }
        .content-wrapper, .right-side { background-color: #fff; }
        
        /* Cool box shadow effect */
        .box {
          box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
          transition: 0.3s;
          border-radius: 4px;
          background-color: #FFFFFF;
        }
        .box:hover {
          box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
        }
        
        /* Cool button effects */
        .btn {
          transition: all 0.3s ease;
          border-radius: 25px;
          color: #dedede;
        }
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        
        /* Sleek input styling */
        .form-control {
          border-radius: 20px;
          border: 1px solid #B3C8D5;
          transition: all 0.3s ease;
        }
        .form-control:focus {
          border-color: #1A5F7A;
          box-shadow: 0 0 8px rgba(26,95,122,0.6);
        }
        .shiny-input-container .js-range-slider {
          height: 30px;
          padding: 0;
        }
        .selectize-input {
           font-size: 14px;
        }
        .form-group {
           margin-bottom: 0px;
        }
        .dropdown {
          background-color: #e9f5fb;
          border: 1px solid #7cc0de;
          border-radius: 5px;
          font-size: 12px;
          font-weight: bold;
          color: #1a5f7a;
          box-shadow: 0 0 5px rgba(0, 0, 0, 0.1);
        }
        
        .dropdown:hover {
          background-color: #d3edf8;
          border-color: #66a9cf;
        }
        
        .rating-spacer { margin-top: 20px; }
        .button-spacer { margin-top: 5px; }
        
        .response-box {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 5px;
          padding: 15px;
          margin-top: 10px;
        }
        
        .response-box h3 {
          color: #007bff;
          margin-top: 0;
          margin-bottom: 15px;
        }
        
        .response-box h4 {
          color: #28a745;
          margin-top: 15px;
          margin-bottom: 10px;
        }
        
        .response-box p, .response-box li {
          margin-bottom: 10px;
        }
        
        .response-box ol {
          padding-left: 20px;
          margin-top: 0;
        }
      '))
    ),
    useShinyjs(),
    tags$head(tags$script("
      $(document).on('shiny:busy', function() {
        $('#loading').show();
      });
      $(document).on('shiny:idle', function() {
        $('#loading').hide();
      });
    ")),
    div(id = "loading", "Loading...", style = "display: none;"),
    fluidRow(
      column(12,
               tags$div(
                 style = "display: flex; justify-content: space-between; align-items: center; background: rgba(230, 242, 255, 0.9); border-radius: 8px; padding: 30px 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 15px; max-width: 100%; margin-left: auto; margin-right: auto; height: 60px;", 
                 
                 # Search and Set button
                 tags$div(
                   style = "display: flex; align-items: center;",
                   tags$span(icon("search"), style = "margin-right: 10px; color: #1A5F7A; font-size: 14px;"),
                   div(style = "width: 250px; display: flex; align-items: center;",  # Vertically center dropdown
                       selectizeInput("set_central_node", NULL, choices = NULL, 
                                      options = list(placeholder = "Search skill...", 
                                                     onInitialize = I('function() { this.setValue(""); }'),
                                                     allowEmptyOption = TRUE,   # Allow placeholder to remain
                                                     persist = FALSE))),          # Prevent auto-selection on load
                   actionButton("set_central_node_btn", "Set as Center", class = "btn-primary", 
                                style = "margin-left: 10px; height: 36px; padding: 0 15px; font-size: 13px;")
                 ),
                 
                 # Slider for number of hops
                 tags$div(
                   style = "display: flex; align-items: center;",
                   tags$span(icon("sitemap"), style = "margin-right: 10px; color: #1A5F7A; font-size: 14px;"),
                   sliderInput("num_hops", NULL, min = 1, max = 4, value = 2, step = 1, width = "150px")
                 )
               )
      )
    )
    ,
    
    # Graph FluidRow 
    fluidRow(
      column(12, 
             tags$div(
               style = "margin-top: 20px;",
               tags$style(HTML("
        @media (max-width: 768px) {
          #plot_output {
            overflow-x: auto;
            -webkit-overflow-scrolling: touch;
            overflow-scrolling: touch;
          }
          #plot_output .vis-network {
            width: 100% !important;
            height: 60vh !important; /* Adjust as needed */
          }
        }
      ")),
               uiOutput("plot_output")
             )
      )
    ),
    fluidRow(
      column(12, 
             tags$div(
               style = "margin-top: 20px;",  # Added top margin
               box(
                 title = "Skill Information", 
                 status = "primary", 
                 solidHeader = TRUE,
                 uiOutput("info_box"),
                 uiOutput("rating_ui"),
                 width = 12, 
                 class = "custom-box-header",
                 style = "background-color: rgba(230, 242, 255, 0.7);"  # Light blue background
               )
             )
      )
    )
  )
)


server <- function(input, output, session) {

  
  # Reactive values
  central_node <- reactiveVal("Machine Learning")
  text_to_process <- reactiveVal(NULL)
  processed_graph <- reactiveVal(NULL)
  
  # Graph creation functions
  create_graph_from_text <- function(text) {
    # Return an empty graph as per your requirement
    g <- make_empty_graph(directed = TRUE)
    
    # Optionally, add a placeholder node
    # g <- add_vertices(g, 1, name = "No Data", type = "Placeholder")
    
    return(g)
  }
  
  create_vis_network <- function(graph, central_node) {
    if (vcount(graph) == 0) {
      # Return a simple message if the graph is empty
      return(NULL)
    }
    
    type_colors <- c(
      "Top Category" = "#8e63ff",        # Slightly less vibrant Purple
      "Core Concepts" = "#e6c200",       # Slightly less vibrant Gold
      "Algorithms & Models" = "#e07b3f", # Slightly less vibrant Orange
      "Programming" = "#1eb5f0",         # Slightly less vibrant Blue
      "Metrics" = "#00b04f",             # Slightly less vibrant Green
      "Applications" = "#ff6666"          # Slightly less vibrant Red
    )
    
    
    
    
    # Create nodes data frame
    nodes <- data.frame(
      id = V(graph)$name,
      label = V(graph)$name,
      group = V(graph)$type,
      stringsAsFactors = FALSE
    )
    
    # Assign colors based on node type, with a default color
    nodes <- nodes %>%
      mutate(
        color = case_when(
          id == "Machine Learning" ~ "#4B0082",  # Deeper Purple for "Machine Learning"
          TRUE ~ type_colors[group]               # Group-based color for other nodes
        ),
        size = ifelse(id == central_node, 30, 20),             # Larger size for central node
        title = paste0("<p><strong>", label, "</strong><br>Type: ", group, "</p>"),  # Tooltip
        shadow = ifelse(id == central_node, "TRUE", "FALSE")   # Enable shadow for central node
      )
    
    
    # Replace any NA colors with a default color
    nodes$color[is.na(nodes$color)] <- "#00b04f"
    
    # Create edges data frame
    edges <- data.frame(
      from = as.character(as_edgelist(graph)[,1]),
      to = as.character(as_edgelist(graph)[,2]),
      label = E(graph)$relationship,
      stringsAsFactors = FALSE
    )
  
    # Assign edge colors based on the source node's color
    edges_colored <- edges %>%
      # Join to get the source node's color
      left_join(nodes %>% select(id, color), by = c("from" = "id")) %>%
      rename(color_from = color) %>%
      # Join to get the target node's color
      left_join(nodes %>% select(id, color), by = c("to" = "id")) %>%
      rename(color_to = color) %>%
      # Assign edge color and highlight color
      mutate(
        color = color_from,           # Default edge color (source node's color)
        highlight.color = color_to,   # Highlight color (target node's color)
        width = 2,                    # Edge width
        title = label,                 # Tooltip for edges
        label = NA
      )
    
    # Create visNetwork object using gradient edge segments
    visNetwork(nodes, edges_colored, height = "600px", width = "100%") %>%
      
      # Define groups with specific colors (optional if groups are styled via nodes)
      visGroups(groupname = "Top Category", color = type_colors["Top Category"], 
                shape = "diamond", shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "Core Concepts", color = type_colors["Core Concepts"]) %>%
      visGroups(groupname = "Algorithms & Models", color = type_colors["Algorithms & Models"]) %>%
      visGroups(groupname = "Programming", color = type_colors["Programming"]) %>%
      visGroups(groupname = "Metrics", color = type_colors["Metrics"]) %>%
      visGroups(groupname = "Applications", color = type_colors["Applications"]) %>%
      
      # Customize nodes
      visNodes(
        shape = "dot",
        font = list(size = 16, color = "black")
      ) %>%
      
      # Customize edges
      visEdges(
        color = list(
          color = edges_colored$color,             # Default edge color
          highlight = edges_colored$highlight.color # Highlight color on hover
        ),
        smooth = TRUE,
        shadow = c(FALSE, TRUE, FALSE, TRUE), 
        width = 2,
        font = list(size = 8),
      ) %>%
      
      # Enable interactivity
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
        nodesIdSelection = FALSE,
        selectedBy = "group"  # Allow selection by group
      ) %>%
      
      # Define events to capture node selection and send to Shiny
      visEvents(
        select = "function(nodes) {
          if(nodes.nodes.length > 0) {
            Shiny.setInputValue('selected_node', nodes.nodes[0], {priority: 'event'});
          }
        }"
      ) %>%
      
      # Physics settings for better layout
      visPhysics(
        stabilization = FALSE,
        barnesHut = list(gravitationalConstant = -4000, centralGravity = 0.4)
      )
  }
  
  
  
  # Reactive graph
  graph <- reactive({
    if (input$graph_source == "Custom Text Input" && !is.null(processed_graph())) {
      return(processed_graph())
    } else {
      full_g <- graph_data
      num_hops <- input$num_hops
      current_central_node <- central_node()
      
      if (!(current_central_node %in% V(full_g)$name)) {
        current_central_node <- V(full_g)$name[which.max(degree(full_g))]
        central_node(current_central_node)
      }
      
      central_node_index <- which(V(full_g)$name == current_central_node)
      ego_nodes <- unlist(ego(full_g, order = num_hops, nodes = central_node_index, mode = "all"))
      
      if (length(ego_nodes) == 0) {
        return(full_g)
      }
      
      return(induced_subgraph(full_g, ego_nodes))
    }
  })
  
  # Add a reactive value to trigger plot updates
  plot_trigger <- reactiveVal(0)
  # Render the appropriate plot based on user selection
  output$plot_output <- renderUI({
    g <- graph()
    vis_net <- create_vis_network(g, central_node())
  })
  

  
  # Observers
  observeEvent(input$set_central_node_btn, {
    new_central_node <- input$set_central_node
    if (!is.null(new_central_node) && new_central_node != "" && new_central_node %in% V(graph_data)$name) {
      central_node(new_central_node)
      showNotification(paste("Central node set to:", new_central_node), type = "message")
    } else {
      showNotification("Invalid central node. Please enter a valid node name.", type = "warning")
    }
  })
  
  observeEvent(input$process_text, {
    if (nchar(input$text_input) > 0) {
      text_to_process(input$text_input)
      g <- create_graph_from_text(text_to_process()) 
      #g <- create_demo_bioinformatics_graph()  
      processed_graph(g)
    } else {
      showNotification("Please enter some text to process.", type = "warning")
    }
  })
  
  observeEvent(input$process_url, {
    if (nchar(input$url_input) > 0) {
      tryCatch({
        webpage <- read_html(input$url_input)
        text <- webpage %>% html_nodes("p") %>% html_text() %>% paste(collapse = " ")
        text_to_process(text)
        g <- create_graph_from_text(text_to_process())
        processed_graph(g)
      }, error = function(e) {
        showNotification("Error processing URL. Please check the URL or paste the text directly.", type = "error")
      })
    } else {
      showNotification("Please enter a valid URL.", type = "warning")
    }
  })
  
  # Update the selectize input choices with the node names
  updateSelectizeInput(session, 'set_central_node', choices = node_names, server = TRUE)
  
  # Observe and handle the selected node
  observeEvent(input$set_central_node, {
    selected_node <- input$set_central_node
    print(paste("Selected node:", selected_node))  # Debug or handle node selection
  })
  
  
  observeEvent(list(
    input$info_options,
    input$selected_node,
    input$detail_level,
    input$article_source,
    input$time_frame,
    input$num_articles,
    input$project_type,
    input$project_difficulty,
    input$generate_project_difficulty,
    input$time_commitment,
    input$quiz_difficulty_level,
    input$num_quiz_questions,
    input$industry,
    input$geographic_region,
    input$experience_level
  ), {
    # Debugging output to see the selected node
    print(paste("Selected node:", input$selected_node))  # Debugging print statement
    
    # Show a loading message while the API call is being processed
    output$info_box <- renderUI({
      HTML("<strong>Loading... Please wait.</strong>")
    })
    
    # Check if a node is selected before proceeding
    if (is.null(input$selected_node) || input$selected_node == "") {
      output$info_box <- renderUI({
        HTML("<strong>Please select a node to view its information.</strong>")
      })
      return()  # Exit if no node is selected
    }
    
    # Get related nodes from the knowledge graph
    related_nodes <- get_related_nodes(graph_data, input$selected_node)
    
    # Define the base prompt based on the selected option
    # Define the base prompt based on the selected option
    base_prompt <- switch(input$info_options,
                          
                          "overview" = {
                            # Map the detail level to descriptive text
                            detail_level_text <- switch(input$detail_level,
                                                        "brief" = "a concise",
                                                        "moderate" = "an intermediate",
                                                        "detailed" = "a comprehensive")
                            paste("Provide", detail_level_text, "overview of", input$selected_node, "including its importance, key concepts, and real-world applications.")
                          },
                          
                          "article_links" = {
                            # Build the prompt with the selected number of articles, source, and time frame
                            source_text <- if (input$article_source == "all") {
                              "from reputable sources"
                            } else {
                              paste("from", input$article_source)
                            }
                            
                            time_frame_text <- switch(input$time_frame,
                                                      "1m" = "published in the last month",
                                                      "1y" = "published in the last year",
                                                      "all" = "")
                            
                            articles_text <- paste("Suggest", input$num_articles, "recent, high-quality articles about", input$selected_node, source_text)
                            
                            if (time_frame_text != "") {
                              articles_text <- paste(articles_text, time_frame_text)
                            }
                            
                            paste(articles_text, "Include a brief description for each.")
                          },
                          
                          "project_links" = {
                            # Build the prompt with selected project type and difficulty
                            difficulty_text <- if (input$project_difficulty == "all") {
                              ""
                            } else {
                              paste("at the", input$project_difficulty, "level")
                            }
                            
                            project_source_text <- switch(input$project_type,
                                                          "kaggle" = "from Kaggle",
                                                          "stemaway" = "from stemaway.com")
                            
                            paste("Propose 3 practical project ideas for", input$selected_node, difficulty_text, project_source_text, ". Outline key objectives and potential challenges for each.")
                          },
                          
                          "generate_project" = {
                            time_commitment_text <- switch(input$time_commitment,
                                                           "short" = "less than a week",
                                                           "medium" = "1-2 weeks",
                                                           "long" = "1 month or more")
                            
                            paste("Generate a detailed project plan related to", input$selected_node,
                                  "at an", input$generate_project_difficulty, "level, suitable for a time commitment of",
                                  time_commitment_text, ". Include objectives, required skills, a step-by-step implementation guide, and potential extensions.")
                          },
                          
                          "test_skill" = {
                            paste("Create a quiz with", input$num_quiz_questions, "questions to test understanding of", input$selected_node,
                                  "at a", input$quiz_difficulty_level, "difficulty level.")
                          },
                          
                          "market_insights" = {
                            industry_text <- if (input$industry == "all") {
                              ""
                            } else {
                              paste("in the", input$industry, "industry")
                            }
                            
                            region_text <- if (input$geographic_region == "global") {
                              ""
                            } else {
                              paste("in", switch(input$geographic_region,
                                                 "na" = "North America",
                                                 "europe" = "Europe",
                                                 "asia" = "Asia"))
                            }
                            
                            experience_text <- switch(input$experience_level,
                                                      "entry" = "entry-level",
                                                      "mid" = "mid-level",
                                                      "senior" = "senior-level")
                            
                            paste("Provide the latest market insights on", input$selected_node, industry_text, region_text,
                                  "for", experience_text, "professionals, including current trends, job market demands, and future projections. Cite recent statistics or reports if possible.")
                          }
    )
    
    
    # Add knowledge graph context to the prompt
    kg_context <- if (length(related_nodes) > 0) {
      paste("Consider its relationships with", paste(related_nodes, collapse=", "), "in your response.")
    } else {
      "Consider its place in the broader field of study."
    }
    
    prompt <- paste(base_prompt, kg_context)
    
    # Print the prompt to ensure it's correct
    print(paste("GPT prompt:", prompt))  # Debugging print statement
    
    # Call the GPT API function with the selected prompt inside a try-catch block for error handling
    tryCatch({
      gpt_reply <- get_gpt_response(prompt)
      
      # Debugging output to check the GPT reply
      print(paste("GPT reply:", gpt_reply))  # Debugging print statement
      
      # Update the info box with the GPT response
      output$info_box <- renderUI({
        
        selected_info <- gsub('\"', '', input$info_options)  # Remove extra quotes if any
        print(paste("Selected info option after cleaning:", selected_info))  # Debugging print
        
        # Replace all newline characters with <br>
        cleaned_reply <- gsub("\\\\n", "<br>", gpt_reply)
        
        # Determine the title based on user selection
        title <- switch(selected_info,
                        overview = "Overview",
                        article_links = "Articles",
                        project_links = "Projects",
                        generate_project = "Generated Project",
                        test_skill = "Skill Test",
                        market_insights = "Market Insights")  # Default fallback
        
        # Render the formatted response with line breaks in the info box
        HTML(paste0(
          "<div class='response-box'>",
          "<h3>", title, ":</h3>",
          cleaned_reply,
          "</div>"
        ))
      })
      
      output$rating_ui <- renderUI({
        tagList(
          div(class = "rating-spacer",
            sliderInput(
              inputId = "response_rating",
              label = "How helpful was this information?",
              min = 1,
              max = 5,
              value = 3,
              step = 1,
              ticks = TRUE
             )
          ),
          textAreaInput("feedback_text", "Additional feedback (optional):"),
          div(class = "button-spacer",
              actionButton("submit_feedback", "Submit Feedback")
          )
        )
      })
      
      
    }, error = function(e) {
      # Handle errors and display a message in the info box
      output$info_box <- renderUI({
        HTML("<strong>Error: Unable to retrieve a response. Please try again.</strong>")
      })
      print(paste("Error:", e$message))  # Print the error message for debugging
    })
  })
  
  observeEvent(input$submit_feedback, {
    rating <- input$response_rating
    feedback <- input$feedback_text
    print(paste("User rated the response:", rating, "out of 5"))
    print(paste("User feedback:", feedback))
    # Here you can add code to save the rating and feedback
    
    showNotification("Thank you for your feedback!", type = "message")
    
    # Reset the inputs
    updateSliderInput(session, "response_rating", value = 3)
    updateTextAreaInput(session, "feedback_text", value = "")
  })
  
}

shinyApp(ui, server)
