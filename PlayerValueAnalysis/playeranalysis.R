library(tidyverse)

#importing the data
df <- read_csv("Desktop/Top 500 Players 2024.csv")
head(df)

#columns in the data
colnames(df)

#shape of the data
dim(df)

#datatype of the columns 
sapply(df,class)

#check number of nan values in data
na_count <- colSums(is.na(df))
print(na_count)


#Analysis 

# Who are the top players with the highest market value in each position?


df = df %>% group_by(Position) %>% mutate(rank_value = dense_rank(desc(`Market Value`))) 
                                                         
filtered_data <- df %>% filter(rank_value == 1) %>% select(c(Name,`Market Value`,rank_value)) %>% arrange(desc(`Market Value`)) 
filtered_data <- filtered_data %>%
  mutate(Name = factor(Name, levels = Name[order(-`Market Value`)]))

ggplot(filtered_data, aes(x = Name, y = `Market Value`, fill = Position)) +
  geom_bar(stat = "identity") +
  labs(title = "Market Value of Top Ranked Player By Position",
       x = "Players",
       y = "Market Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Which players have contributed the most goals and assists combined?

df <- df %>% mutate(glsast = Goals + Assists)
top10GA <- df %>% mutate(rnk_glast = dense_rank(desc(glsast))) %>% select(c(Name,glsast,rnk_glast)) %>% arrange(rnk_glast) %>% filter(rnk_glast <= 10)

ggplot(top10GA, aes(x = reorder(Name,-glsast),y= glsast)) +
    geom_bar(stat = "identity") +
    labs(title = 'Top 10 Players by goals/assist(combined)',
        x = 'Players',
        y = 'Goals & Assist(Combined)') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Who has the best goal-to-match ratio top 10?

df <- df %>% mutate(gm_ratio = Goals/`Matches Played`) 
df["gm_ratio"][is.na(df["gm_ratio"])] <- 0

ratiodf <- df %>% select(Name,gm_ratio) %>% arrange(desc(gm_ratio)) %>% head(10)


ggplot(ratiodf, aes(x = reorder(Name,-gm_ratio),y = gm_ratio)) +
    geom_bar(stat = "identity") +
    labs(title = 'Goal to Match ratio By Players',
         x = 'Players',
         y = 'Ratio') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# What is the correlation between Age and gm_ratio? Do younger players tend to score more often?


correl <- df %>% group_by(Age) %>% summarise(avg_ratio = mean(gm_ratio)) %>% arrange(Age)

ggplot(correl, aes(x = Age,y = avg_ratio)) +
     geom_point() 


# Which player has received the most yellow cards, and what is their gm_ratio compared to others?

  
df <- df %>% mutate(total_yellow = `Yellow Cards` + `Second Yellow Cards`)
 
yellow_players <- df %>% filter(`total_yellow` == max(`total_yellow`)) %>% select(Name,`total_yellow`,gm_ratio) %>% slice(1)


player_name <- yellow_players$Name
gm_yellow <- yellow_players$gm_ratio


comparison_plot <- df %>%
  ggplot(aes(x = gm_ratio)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = gm_yellow, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("Distribution of gm_ratio with", player_name, "highlighted"),
    x = "gm_ratio",
    y = "Count"
  ) +
  theme_minimal()


comparison_plot

# Which players are substituted in the most (Substituted In) and how does it impact their performance?

top_sub <- df %>% arrange(desc(`Substituted In`)) %>% select(Name,`Substituted In`) %>% head(10)

comparison_plot <- df %>% mutate(Top_Substituted = ifelse(Name %in% top_sub$Name, "Top Substituted", "Others")) %>%
  ggplot(aes(x = gm_ratio, fill = Top_Substituted)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Performance Comparison: Top Substituted Players vs. Others",
    x = "gm_ratio",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Top Substituted" = "red", "Others" = "blue"))


comparison_plot

                  
# How does the Market Value distribution differ between younger players (under 25) and older players?

df <- df %>% mutate(category = ifelse(Age < 25 , "younger","older"))

ggplot(df, aes(x = category,y = `Market Value`,fill = category)) +
    geom_boxplot() +
    labs(title = "Market Value Distribution: Under 25 vs 25 and Older",
         x = 'Age Group',
         y = 'Market Value') +
    theme_minimal() +
    scale_fill_manual(values = c("Under 25" = "lightblue", "25 and Older" = "lightgreen"))
    