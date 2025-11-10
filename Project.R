# Install and load libraries
install.packages(c("ggplot2", "dplyr", "corrplot", "GGally"))
install.packages("ggmosaic")
install.packages(c("tm", "wordcloud", "RColorBrewer"))

# Load libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(stringr)
library(ggalluvial)
library(ggmosaic)
library(vcd)


# Load dataset (replace with your CSV file path)
data <- read.csv("C:/Users/Hp/Downloads/Study_Sphere.csv")

 # ----- BASIC STRUCTURE -----
head(data)
summary(data)


# Convert relevant columns to factors
data <- data %>%
  mutate(
    Gender = as.factor(Gender),
    Branch = as.factor(Branch),
    Home.State = as.factor(Home.State),
    Study_Hours = as.factor(`How.many.hours.do.you.study.daily.outside.class.`),
    Preferred_Time = as.factor(`What.time.of.day.do.you.prefer.studying.`),
    Study_Method = as.factor(`Which.study.method.do.you.use.most.`),
    Motivation = as.factor(`What.motivates.you.to.study.the.most.`)
  )

data <- data %>%
  mutate(
    Branch = str_squish(Branch),               # remove all extra inner spaces
    Branch = str_replace_all(Branch, "[^[:alnum:] ]", ""),  # remove hidden chars
    Branch = toupper(Branch),                  # uppercase all
    Branch = trimws(Branch)                    # final clean-up of outer spaces
  )


# ----- 1️⃣ Bar Chart: Study hours by Gender -----
ggplot(data, aes(x = Study_Hours, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Study Hours by Gender", x = "Daily Study Hours", y = "Count") +
  theme_minimal()

# ----- 2️⃣ Pie Chart: Preferred Study Time -----
library(dplyr)
library(ggplot2)

#  Summarize counts and percentages
pie_data <- data %>%
  count(Preferred_Time) %>%                  # count each category
  mutate(perc = round(100 * n / sum(n), 1),  # calculate percentages
         label = paste0(Preferred_Time, " (", perc, "%)"))  # add label text

#  Create pie chart with labels
ggplot(pie_data, aes(x = "", y = n, fill = Preferred_Time)) +
  geom_bar(stat = "identity", width = 1, color = "white") +   # bar for each category
  coord_polar("y", start = 0) +                               # convert to pie chart
  geom_text(aes(label = paste0(perc, "%")),                   # show percentages
            position = position_stack(vjust = 0.5),           # center in slice
            color = "white", size = 4, fontface = "bold") +
  labs(title = "Preferred Study Time of Day",
       x = NULL, y = NULL, fill = "Study Time") +
  theme_void() +                                               # clean background
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) # center title



# ----- 3️⃣ Study Method by Branch -----


heatmap_data <- data %>%
  count(Branch, Study_Method)

ggplot(heatmap_data, aes(x = Branch, y = Study_Method, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Heatmap: Study Method Usage by Branch",
    x = "Branch", 
    y = "Study Method", 
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10)
  )


# Motivation VS Study Hours
ggplot(data, aes(x = Study_Hours, fill = `What.motivates.you.to.study.the.most.`)) +
  geom_bar(position = "fill") +
  labs(
    title = "Motivation vs Study Hours",
    x = "Study Hours per Day",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ----- 5️⃣ Motivation vs Confidence -----
flow_data <- data %>%
  group_by(`What.motivates.you.to.study.the.most.`, `How.confident.are.you.in.your.exam.preparation.`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  rename(
    Motivation = `What.motivates.you.to.study.the.most.`,
    Confidence = `How.confident.are.you.in.your.exam.preparation.`
  )

# Plot
flow_data <- data %>%
  group_by(Study_Method, `What.motivates.you.to.study.the.most.`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  rename(
    Motivation = `What.motivates.you.to.study.the.most.`
  )

# ---- 2️⃣ Create the alluvial flow diagram ----
ggplot(flow_data,
       aes(axis1 = Study_Method, axis2 = Motivation, y = Count)) +
  geom_alluvium(aes(fill = Study_Method), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey85", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Study Method", "Motivation"), expand = c(.1, .05)) +
  labs(
    title = "Flow Diagram: Study Method vs Motivation",
    x = "",
    y = "Number of Students",
    fill = "Study Method"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



# Asociation Plot 
assoc_data <- table(data$`How.often.do.you.procrastinate.study.tasks.`,
                    data$`How.confident.are.you.in.your.exam.preparation.`)
assocplot(assoc_data,
          main = "Association Plot: Procrastination vs Confidence",
          xlab = "Procrastination",
          ylab = "Confidence")


# ---------- Install packages (run once) ----------
install.packages(c("tm", "wordcloud", "RColorBrewer"))


library(tm)
library(wordcloud)
library(RColorBrewer)

# ---------- Make sure your column names match your dataset ----------
# Run this first to verify names:
colnames(data)

# Replace these with exact column names from your dataset
motivation_col <- "What.motivates.you.to.study.the.most."
challenge_col  <- "What.is.your.biggest.challenge.in.studying.effectively."

# ---------- Handle missing values ----------
data[[motivation_col]][is.na(data[[motivation_col]])] <- ""
data[[challenge_col]][is.na(data[[challenge_col]])] <- ""

# ---------- Combine all text into one string ----------
motivation_text <- paste(data[[motivation_col]], collapse = " ")
challenge_text  <- paste(data[[challenge_col]], collapse = " ")

# ---------- Define reusable function ----------
create_wordcloud <- function(text_data, title_text) {
  if (nchar(text_data) == 0) {
    message("⚠️ No text data found for: ", title_text)
    return(NULL)
  }
  
  corpus <- Corpus(VectorSource(text_data))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  tdm <- TermDocumentMatrix(corpus)
  matrix_data <- as.matrix(tdm)
  
  if (nrow(matrix_data) == 0) {
    message("⚠️ No valid words found for: ", title_text)
    return(NULL)
  }
  
  word_freq <- sort(rowSums(matrix_data), decreasing = TRUE)
  print(head(word_freq, 10))  # show top words
  
  set.seed(123)
  wordcloud(
    words = names(word_freq),
    freq = word_freq,
    min.freq = 1,
    max.words = 100,
    random.order = FALSE,
    scale = c(4, 0.7),
    colors = brewer.pal(8, "Dark2")
  )
  title(main = title_text)
}

# ---------- Plot both word clouds ----------
par(mfrow = c(1, 2))  # display side-by-side
create_wordcloud(motivation_text, "What Motivates You to Study the Most")
create_wordcloud(challenge_text, "Biggest Challenge in Studying Effectively")

library(tm)
library(wordcloud)
library(RColorBrewer)


# Replace these with exact column names from your dataset
motivation_col <- "What.motivates.you.to.study.the.most."
challenge_col  <- "What.is.your.biggest.challenge.in.studying.effectively."

# ---------- Handle missing values ----------
data[[motivation_col]][is.na(data[[motivation_col]])] <- ""
data[[challenge_col]][is.na(data[[challenge_col]])] <- ""

# ---------- Combine all text into one string ----------
motivation_text <- paste(data[[motivation_col]], collapse = " ")
challenge_text  <- paste(data[[challenge_col]], collapse = " ")

# ---------- Define reusable function ----------
create_wordcloud <- function(text_data, title_text) {
  if (nchar(text_data) == 0) {
    message(" No text data found for: ", title_text)
    return(NULL)
  }
  
  corpus <- Corpus(VectorSource(text_data))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  tdm <- TermDocumentMatrix(corpus)
  matrix_data <- as.matrix(tdm)
  
  if (nrow(matrix_data) == 0) {
    message("⚠️ No valid words found for: ", title_text)
    return(NULL)
  }
  
  word_freq <- sort(rowSums(matrix_data), decreasing = TRUE)
  print(head(word_freq, 10))  # show top words
  
  set.seed(123)
  wordcloud(
    words = names(word_freq),
    freq = word_freq,
    min.freq = 1,
    max.words = 100,
    random.order = FALSE,
    scale = c(4, 0.7),
    colors = brewer.pal(8, "Dark2")
  )
  title(main = title_text)
}

# ---------- Plot both word clouds ----------
par(mfrow = c(1, 2))  # display side-by-side
create_wordcloud(motivation_text, "What Motivates You to Study the Most")
create_wordcloud(challenge_text, "Biggest Challenge in Studying Effectively")


motivation_col <- "What.motivates.you.to.study.the.most."
challenge_col  <- "What.is.your.biggest.challenge.in.studying.effectively."

# ---------- Handle missing values ----------
data[[motivation_col]][is.na(data[[motivation_col]])] <- ""
data[[challenge_col]][is.na(data[[challenge_col]])] <- ""

# ---------- Combine all text into one string ----------
motivation_text <- paste(data[[motivation_col]], collapse = " ")
challenge_text  <- paste(data[[challenge_col]], collapse = " ")

# ---------- Define reusable function ----------
create_wordcloud <- function(text_data, title_text) {
  if (nchar(text_data) == 0) {
    message("⚠️ No text data found for: ", title_text)
    return(NULL)
  }
  
  corpus <- Corpus(VectorSource(text_data))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  tdm <- TermDocumentMatrix(corpus)
  matrix_data <- as.matrix(tdm)
  
  if (nrow(matrix_data) == 0) {
    message("⚠️ No valid words found for: ", title_text)
    return(NULL)
  }
  
  word_freq <- sort(rowSums(matrix_data), decreasing = TRUE)
  print(head(word_freq, 10))  # show top words
  
  set.seed(123)
  wordcloud(
    words = names(word_freq),
    freq = word_freq,
    min.freq = 1,
    max.words = 100,
    random.order = FALSE,
    scale = c(4, 0.7),
    colors = brewer.pal(8, "Dark2")  # nice color palette
  )
  title(main = title_text)
}

# ---------- Plot both word clouds ----------
par(mfrow = c(1, 2), bg = "white")  # white background
create_wordcloud(motivation_text, "What Motivates You to Study the Most")
create_wordcloud(challenge_text, "Biggest Challenge in Studying Effectively")



