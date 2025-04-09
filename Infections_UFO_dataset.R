#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("GGally")

# Infection Data Visualizations in R

# Create the data frame
infections <- c(245, 215, 2076, 5023, 189, 195, 123, 116, 3298, 430, 502, 126, 112, 67, 52, 39, 54, 2356, 6781, 120, 2389, 279, 257, 290, 234, 5689, 261, 672, 205)
ufo2010 <- c(2, 6, 2, 59, 0, 1, 1, 0, 115, 0, 0, 0, 0, 0, 0, 0, 6, 4, 2, 7, 2, 9, 2, 29, 10, 169, 1, 40, 16)
pop <- c(25101, 61912, 33341, 409061, 7481, 18675, 25581, 22286, 459598, 3915, 67197, 34365, 3911, 32122, 31459, 2311, 28350, 101482, 19005, 20679, 36745, 162812, 15927, 251417, 153920, 1554720, 16148, 305455, 37276)

df <- data.frame(infections, ufo2010, pop)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# --- 1. Bar Graph: Comparing Infections and UFO Sightings ---
ggplot(df, aes(x = 1:nrow(df))) +
  geom_bar(aes(y = infections, fill = "Infections"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = ufo2010, fill = "UFO Sightings (2010)"), stat = "identity", position = "dodge", alpha = 0.7) +
  scale_fill_manual("Variables", values = c("Infections" = "skyblue", "UFO Sightings (2010)" = "salmon")) +
  labs(x = "Data Point Index", y = "Count", title = "Comparison of Infections and UFO Sightings") +
  theme_minimal() +
  theme(legend.position = "top")

# Observation: This bar graph compares the number of infections and UFO sightings for each
# data point. The scale of infections is significantly higher than UFO sightings in most cases.
# There are few instances where UFO sightings are non-zero, but their counts are low relative
# to the infection numbers.

# --- 2. Line Chart: Trends in Infections and Population ---
ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = infections, color = "Infections"), linewidth = 1) +
  geom_line(aes(y = pop, color = "Population"), linewidth = 1, linetype = "dashed") +
  scale_color_manual("Variables", values = c("Infections" = "green", "Population" = "purple")) +
  labs(x = "Data Point Index", y = "Count", title = "Trends in Infections and Population") +
  theme_minimal() +
  theme(legend.position = "top")

# Observation: This line chart shows the trends of infections and population across the data points.
# The population values are on a much larger scale than infection counts, making it difficult to
# observe detailed changes in infections on the same plot. However, we can see the overall
# fluctuations of both variables.

# --- 3. Scatter Plot: Relationship between Population and Infections ---
ggplot(df, aes(x = pop, y = infections)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(x = "Population", y = "Number of Infections", title = "Relationship between Population and Number of Infections") +
  theme_minimal()

# Observation: This scatter plot explores the relationship between population size and the number
# of infections. There doesn't appear to be a strong linear correlation. While some high-population
# areas have high infection counts, this is not consistently the case.


# --- 4. Box Plot: Distribution of Infections ---
ggplot(df, aes(y = infections)) +
  geom_boxplot(fill = "lightcoral") +
  labs(y = "Number of Infections", title = "Distribution of Number of Infections") +
  theme_minimal()

# Observation: This box plot summarizes the distribution of the 'infections' variable. It shows the
# median, quartiles, and potential outliers. The plot indicates that the majority of infection
# counts are relatively low, with some higher values identified as outliers.

# --- 5. Histogram: Frequency Distribution of UFO Sightings ---
ggplot(df, aes(x = ufo2010)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  labs(x = "Number of UFO Sightings (2010)", y = "Frequency", title = "Frequency Distribution of UFO Sightings (2010)") +
  theme_minimal()

# Observation: This histogram shows the frequency distribution of UFO sightings in 2010. The
# distribution is heavily skewed towards zero, indicating that most data points have very few or
# no reported UFO sightings.

# --- 6. Scatter Plot: Relationship between Population and UFO Sightings ---
ggplot(df, aes(x = pop, y = ufo2010)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(x = "Population", y = "Number of UFO Sightings (2010)", title = "Relationship between Population and UFO Sightings (2010)") +
  theme_minimal()

# Observation: This scatter plot examines the relationship between population size and the number
# of UFO sightings. There doesn't seem to be a clear linear relationship between these two variables.

# --- 7. Scatter Plot: Infections vs. UFOs with Population Size ---
ggplot(df, aes(x = ufo2010, y = infections, size = pop)) +
  geom_point(alpha = 0.6, color = "maroon") +
  scale_size_continuous(name = "Population Size") +
  labs(x = "Number of UFO Sightings (2010)", y = "Number of Infections", title = "Infections vs. UFO Sightings, Size by Population") +
  theme_minimal()

# Observation: This scatter plot shows the relationship between infections and UFO sightings, with
# the size of each point representing the population size. It helps to visualize if areas with higher
# infections or UFO sightings also tend to have larger populations. No strong pattern is immediately
# apparent.

# --- 9. Pair Plot: Overview of Relationships ---
library(GGally)
ggpairs(df) +
  ggtitle("Pair Plot of Infections, UFO Sightings, and Population") +
  theme_minimal()

# Observation: The pair plot provides a matrix of scatter plots for each pair of variables and
# density plots for the distribution of each individual variable. This gives a quick overview of
# potential linear relationships and the shape of the distributions. The distributions of
# infections and UFO sightings appear skewed, and the scatter plots reiterate the lack of strong
# linear correlations observed in the individual plots.

# --- 8. Bubble Chart: Overview of Relationships ---
# Sort the data by infections
df_sorted <- df %>%
  mutate(index = row_number()) %>%
  arrange(desc(infections))

# Create the bubble chart without the lines
ggplot(df_sorted, aes(x = reorder(index, -infections), y = infections)) +
  geom_point(aes(color = ufo2010), size = 6) +  # Increase the size of the bubbles for better visibility
  scale_color_gradient(low = "lightgreen", high = "red") +
  labs(
    x = "Data Point",
    y = "Infections",
    color = "UFO Sightings",
    title = "Bubble Chart: Infections Ranked with UFO Sightings Color"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  # Hide x-axis labels for better visuals

# Observation: The bubble chart reveals that the highest UFO sightings occur in the higher mid-range
# infection areas (3,500-6,000 cases), shown by deep red points. However, the very highest infection
# point (6,781 cases) shows only moderate UFO activity, demonstrating that extreme infection levels 
# don't consistently correlate with increased UFO reports. This aligns with the pair plot's finding of
# weak overall correlation between these variables.

