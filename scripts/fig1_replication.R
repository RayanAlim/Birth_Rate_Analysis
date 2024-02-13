# Step 1: Import Data
library(readr)
data_dir <- "../data/fig_1.csv"
fig_1_data <- read_delim(file.path(data_dir, "fig_1.csv"), delim = ",", col_types = cols())

# If converting columns which are read as character to numeric 
fig_1_data <- type.convert(fig_1_data, as.is = TRUE)

# Step 2: Plotting Data
library(ggplot2)
p <- ggplot(fig_1_data, aes(x = year, y = brate_all)) +
  geom_line(color = "blue") + 
  labs(title = "Figure 1",
       subtitle = "Trend in US Birth Rates",
       x = "Year", y = "Births per 1,000 women age 15-44",
       caption = "Source: Birth Rates collected from CDC Vital Statistics Births Reports for 2015, 2019 and 2020. See Data Appendix for additional details.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), # Center the title
        plot.subtitle = element_text(hjust = 0.5), # Center the subtitle
        plot.caption = element_text(hjust = 0, vjust = 1)) + # Left-align the caption
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black") +
  annotate("text", x = 2007, y = min(fig_1_data$brate_all), label = "2007", vjust = -1)

# Step 3: Export the Graph
output_dir <- "../output"
ggsave(file.path(output_dir, "fig_1.png"), plot = p, width = 10, height = 8, dpi = 300)



