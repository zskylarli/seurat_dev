# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("output/mismatching_celltypes.csv")

# Extract the two columns of interest
col1 <- data$celltype1
col2 <- data$celltype2

# Create an empty data frame to store the counts
pairwise_counts <- data.frame(word1 = character(0), word2 = character(0), count = integer(0))

# Loop through all unique words in the first column
for (word1 in unique(col1)) {
  # Loop through all unique words in the second column
  for (word2 in unique(col2)) {
    # Skip if word1 and word2 are the same
    if (word1 == word2) next
    
    # Count the number of times word1 and word2 appear in the same row
    count <- sum((col1 == word1 & col2 == word2) | (col1 == word2 & col2 == word1))
    
    # Skip if count is zero
    if (count == 0) next
    
    # Sort the words to create a unique identifier for each pair
    sorted_words <- sort(c(word1, word2))
    
    # Check if this combination already exists in the data frame
    existing_row <- which(pairwise_counts$word1 == sorted_words[1] & pairwise_counts$word2 == sorted_words[2])
    
    if (length(existing_row) == 0) {
      # Append the count to the data frame
      pairwise_counts <- rbind(pairwise_counts, data.frame(word1 = sorted_words[1], word2 = sorted_words[2], count = count))
    } else {
      # Update the existing count
      pairwise_counts[existing_row, "count"] <- pairwise_counts[existing_row, "count"] + count
    }
  }
}
pairwise_counts$count <- pairwise_counts$count / 2
# Save the result to a CSV file (optional)
write.csv(pairwise_counts, "output/pairwise_counts.csv", row.names = FALSE)

--------

# Read the CSV file
data <- read.csv("output/mismatching_celltypes.csv")

# Extract the two columns of interest (replace 'column1' and 'column2' with the actual column names)
col1 <- data$celltype1
col2 <- data$celltype2

# Create an empty data frame to store the counts
pairwise_counts <- data.frame(word1 = character(0), word2 = character(0), count = integer(0))

# Loop through all unique words in the first column
for (word1 in unique(col1)) {
  # Loop through all unique words in the second column
  for (word2 in unique(col2)) {
    # Count the number of times word1 and word2 appear in the same row
    count <- sum(col1 == word1 & col2 == word2)
    
    # Append the count to the data frame
    pairwise_counts <- rbind(pairwise_counts, data.frame(word1 = word1, word2 = word2, count = count))
  }
}

# Remove rows where count is zero (optional)
pairwise_counts <- pairwise_counts %>% filter(count > 0)

# Print the result
print(pairwise_counts)

-------

# Read the CSV file
data <- read.csv("output/pairwise_counts.csv")  # Replace 'input_file.csv' with the actual file name

# Sort the data frame by the 'count' column in descending order
sorted_data <- data %>% arrange(desc(count))

# Save the sorted data frame back to a CSV file
write.csv(sorted_data, "output/pairwise_counts.csv", row.names = FALSE)


------
# Load ggplot2 package
library(ggplot2)
library(dplyr)

# Read the CSV file
pairwise_counts <- read.csv("output/pairwise_counts.csv")

# Create a symmetrical data frame
symmetric_counts <- pairwise_counts %>%
  bind_rows(select(pairwise_counts, word1 = word2, word2 = word1, count)) %>%
  group_by(word1, word2) %>%
  summarise(count = sum(count))

# Create the plot
p <- ggplot(data = symmetric_counts, aes(x = word1, y = word2)) +
  geom_point(aes(size = count, color = count)) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(size = "Count", color = "Count") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(p)

# Save the plot to a file
ggsave("output/symmetric_heatmap_dot_plot.png", plot = p, width = 10, height = 10)