###########################################
### CREATE HAB CATEGORIES & HEATMAP ######
###########################################
library(dplyr)
library(ggplot2)
library(scales)  
library(gridExtra)
str(RawData)
safe_numeric_convert <- function(x) {
  result <- as.numeric(as.character(x))
  return(result)
}

# Process the data and add the HAB_Category column
processed_data <- RawData %>%
  mutate(
    # Convert values to numeric
    Chla_num = as.numeric(Chla),
    Phyco_num = safe_numeric_convert(Phyco),
    pMicr_num = safe_numeric_convert(pMicr),
    
    # Add HAB category based on thresholds
    HAB_Category = case_when(
      is.na(Chla_num) | is.na(Phyco_num) | is.na(pMicr_num) ~ "No visit",
      Chla_num > 15 & Phyco_num > 30 & pMicr_num > 1.6 ~ "High Chl-a, High Phyco, High Micr",
      Chla_num > 15 & Phyco_num > 30 & pMicr_num <= 1.6 ~ "High Chl-a, High Phyco, Low Micr",
      Chla_num > 15 & Phyco_num <= 30 & pMicr_num > 1.6 ~ "High Chl-a, Low Phyco, High Micr",
      Chla_num > 15 & Phyco_num <= 30 & pMicr_num <= 1.6 ~ "High Chl-a, Low Phyco, Low Micr",
      Chla_num <= 15 & Phyco_num > 30 & pMicr_num > 1.6 ~ "Low Chl-a, High Phyco, High Micr",
      Chla_num <= 15 & Phyco_num > 30 & pMicr_num <= 1.6 ~ "Low Chl-a, High Phyco, Low Micr",
      Chla_num <= 15 & Phyco_num <= 30 & pMicr_num > 1.6 ~ "Low Chl-a, Low Phyco, High Micr",
      TRUE ~ "Low Chl-a, Low Phyco, Low Micr"
    )
  )


# Replace RawData with processed data that includes the new category
RawData <- processed_data

# Define consistent color scheme for both plots with new colors
hab_colors <- c(
  "High Chl-a, High Phyco, High Micr" = alpha("#8B0000", 1.0),    
  "High Chl-a, Low Phyco, High Micr" = alpha("#CD5C5C", 1.0),     
  "Low Chl-a, High Phyco, High Micr" = alpha("#8B0000", 0.3),     
  "Low Chl-a, Low Phyco, High Micr" = alpha("#CD5C5C", 0.3),      
  
  
  "High Chl-a, High Phyco, Low Micr" = alpha("#00008B", 1.0),     
  "High Chl-a, Low Phyco, Low Micr" = alpha("#4169E1", 1.0),      
  "Low Chl-a, High Phyco, Low Micr" = alpha("#00008B", 0.3),      
  "Low Chl-a, Low Phyco, Low Micr" = alpha("#4169E1", 0.3),       
  
  "No visit" = "grey90"                                           
)

# Create heatmap
# Filter for weeks between April and October
sampling_weeks <- processed_data %>%
  distinct(week_of_year) %>%
  arrange(week_of_year)

# Create complete grid only for actual sampling weeks
complete_weeks <- expand.grid(
  week_of_year = sampling_weeks$week_of_year,
  Year = unique(processed_data$Year),
  Site = unique(processed_data$Site)
)

hab_plot_data <- left_join(complete_weeks, processed_data, 
                           by = c("week_of_year", "Year", "Site"))

# Create ordered factor for Sites
site_order <- paste0("WE", rev(2:16))
hab_plot_data$Site <- factor(hab_plot_data$Site, 
                             levels = site_order,
                             ordered = TRUE)

# Order the factor levels of HAB_Category to match our color scheme
hab_plot_data$HAB_Category <- factor(hab_plot_data$HAB_Category,
                                     levels = names(hab_colors))

# Create the heatmap with two rows
heatmap_plot <- ggplot(hab_plot_data, aes(x = week_of_year, y = Site)) + 
  geom_tile(aes(fill = HAB_Category), color = "white") +
  scale_fill_manual(values = hab_colors, drop = FALSE, na.value = "grey95") +
  facet_wrap(~ Year, nrow = 2) +  # Changed from facet_grid to facet_wrap with nrow = 2
  scale_x_continuous(
    breaks = c(14, 18, 22, 27, 31, 35, 40),
    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"),
    limits = c(14, 44)
  ) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Site",  # Changed from "Year" to "Site" to be more accurate
    fill = "HAB Category"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.text.x = element_text(angle = 0),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9)
  )

heatmap_plot

# Create pie charts excluding NA and "No visit" data
# Function to create pie chart for a specific year
create_hab_pie_chart <- function(data, year) {
  # Filter data for the specific year and remove NA and "No visit" categories
  year_data <- data %>%
    filter(Year == year, 
           !is.na(HAB_Category),
           HAB_Category != "No visit") %>%
    # Count occurrences of each category
    group_by(HAB_Category) %>%
    summarise(count = n()) %>%
    # Calculate percentage
    mutate(percentage = count / sum(count) * 100)
  
  # Create pie chart
  ggplot(year_data, aes(x = "", y = percentage, fill = HAB_Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = hab_colors, drop = TRUE) +
    # Add percentage labels
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(title = paste("HAB Categories Distribution -", year),
         fill = "HAB Category") +
    theme_void() +
    theme(legend.position = "right")
}

# Create pie charts for each year
years <- unique(processed_data$Year)
pie_charts <- lapply(years, function(yr) create_hab_pie_chart(processed_data, yr))

# Arrange multiple pie charts in a grid

grid.arrange(grobs = pie_charts, ncol = 2)

# Additionally, create a combined pie chart for all years
all_years_pie <- processed_data %>%
  filter(!is.na(HAB_Category),
         HAB_Category != "No visit") %>%
  group_by(HAB_Category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = "", y = percentage, fill = HAB_Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = hab_colors, drop = TRUE) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "HAB Categories Distribution - All Years",
       fill = "HAB Category") +
  theme_void() +
  theme(legend.position = "right")

all_years_pie

