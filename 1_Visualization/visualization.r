library(ggplot2)
library(dplyr)
library(patchwork)

# Load the dataset
wine <- read.csv("1_Visualization\\winequality-all.csv", header = TRUE, comment.char = "#")

# -----1-----
# Investigating the distribution of alcohol variable
ggplot(wine, aes(x = alcohol)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3, alpha = 0.5,
                 color = "black", fill = "white") +
  geom_density(alpha = .7, fill = "lightblue") +
  ggtitle("Density of Alcohol Variable")
ggsave("r1.pdf", path = "1_Visualization\\plots")

# -----2-----
# Comparing the distribution of alcohol variable between two types of wine i.e.
# red and white
hist <- ggplot(wine, aes(x = alcohol, fill = color)) +
  geom_histogram(binwidth=.5, position = "dodge", color = "black", alpha = .7) +
  scale_fill_manual(values = c("brown1", "lightblue")) +
  ggtitle("Histogram of Alcohol Variable by wine color")

dens <- ggplot(wine, aes(x = alcohol, fill = color)) +
  geom_density(color="black", alpha = 0.7) +
  scale_fill_manual(values = c("brown1", "lightblue")) +
  ggtitle("Density of Alcohol Variable by wine color")

hist + dens + plot_layout(ncol = 1)

ggsave("r2.pdf", path = "1_Visualization\\plots")

# -----3-----
# Comparing the distribution of alcohol variable in each of possible quality 
# group defined by response variable
box <- ggplot(wine, aes(x = response, y = alcohol, fill = as.factor(response))) +
  geom_boxplot() +
  guides(fill=FALSE) +
  coord_flip()

dens <- ggplot(wine, aes(x = alcohol, fill = as.factor(response))) +
  geom_density(color="black", alpha=0.3) +
  guides(fill=FALSE)

box + ggtitle("Density of Alcohol Variable by assessed quality") + dens + plot_layout(ncol = 1)

ggsave("r3.pdf", path = "1_Visualization\\plots")

# -----4-----
# Percentage of red and white wines within each quality group
df <- wine %>%
  group_by(response, color) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

new_row <- data.frame(response=c(7), color=c("red"), count=c(0), percent=c(0))
new_row$response <- as.integer(new_row$response)
new_row$count <- as.integer(new_row$count)

df <- bind_rows(df, new_row)
  
ggplot(df, aes(x = response, y = percent, fill = color)) +
  geom_bar(stat = "identity", position = "dodge", color="black", alpha=.7) +
  scale_fill_manual(values = c("red", "lightblue")) +
  ggtitle("Percentage of Red and White Wines within each Quality Group")

ggsave("r4.pdf", path = "1_Visualization\\plots")

# -----5-----
# Investigating the relationship between variables describing acidity of the wines
ggplot(wine, aes(x = fixed.acidity, y = volatile.acidity, color = color)) +
  geom_point(alpha=.7) +
  scale_color_manual(values = c("red", "lightblue")) +
  ggtitle("Relationship between Acidity Variables by Wine Type")

ggsave("r5.pdf", path = "1_Visualization\\plots")
