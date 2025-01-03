# RShiny Plot 2

# Install and load libraries 
library(shiny)
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("~/Desktop/DCDM_GROUP7/outputs/clean_final_data.csv")


# Use the pipe function to transform the p-value to -10log scale, and then filter by significance
# We apply the -log10 scale for better visualisation of small p-values, emphasising statistically significant p-values
data <- data %>%
  mutate(
    log_p_value = -log10(pvalue),  # Transform p-value to -log10 scale
    Significant = pvalue <= 0.05  # Add significance threshold
  )


# Define UI
ui <- fluidPage(
  titlePanel("P-Values by Gene Symbol"),
  sidebarLayout(
    sidebarPanel(
      selectInput("parameter_name", "Select Phenotype:",
                  choices = unique(data$parameter_name),  # Dynamically populate choices for parameter names
                  selected = unique(data$parameter_name)[1])  # Default to the first phenotype
    ),
    mainPanel(
      plotOutput("pvalplot",  height = "700px") # Adjust height for better fit
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$pvalplot <- renderPlot({
    # Filter the data based on the selected parameter name
    filtered_data <- data %>%
      filter(Significant == TRUE & parameter_name == input$parameter_name)
    
    # Generate the plot
    plot_volcano(filtered_data)
  })
}


# Define a function to plot significant p-values
plot_volcano <- function(filtered_data) {
  ggplot(filtered_data, aes(x = gene_symbol, y = pvalue)) +
    
    # Add vertical segments representing the -log10(p-value) from y=0 to the log_p_value
    geom_segment(aes(x = gene_symbol, y = 0, yend = pvalue), 
                 size = 1, color = "lightpink") +
    
    # Add points on the plot representing the -log10(p-value) for each phenotype
    geom_point(size = 4, color = "deeppink") +
    
    # Apply a minimal theme
    theme_minimal() +
    
    # Add title and axis labels
    labs(
      title = "Statistical Scores of Knockout Mice for a Selected Phenotype",
      x = "Gene Symbol",
      y = "-log(p-Value)"
    ) +
    
    # Adjust plot theme for better fit
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better fitting
     
    )
  
}

# Run the Shiny App
shinyApp(ui = ui, server=server)