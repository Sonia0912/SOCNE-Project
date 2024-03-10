# Sonia Nicoletti
# A12922110
# a12922110@ntu.edu.tw

if(!require(tuber)){devtools::install_github("adrauc/tubernew", build_vignettes = TRUE, force=T)}
library(tuber)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(vader)){install.packages("vader")}
library(vader)
if(!require(tidyr)){install.packages("tidyr")}
library(tidyr)
if(!require(data.table)){install.packages("data.table")}
library(data.table)
if(!require(igraph)){install.packages("igraph")}
library(igraph)
if(!require(semnet)){install.packages("semnet")}
library(semnet)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
if(!require(stm)){install.packages("stm")}
library(stm)
if(!require(scales)){install.packages("scales")}
library(scales)
if(!require(ggthemes)){install.packages("ggthemes")}
library(ggthemes)
if(!require(oolong)){install.packages("oolong")}
library(oolong)
if(!require(tm)){install.packages("tm")}
library(tm)
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)

# Authentication
yt_oauth(app_id = "xxxxxx", app_secret = "xxxxxx", token="")
yt_token()
yt_authorized()

# ********************************
# ***** Collecting AI videos *****
# ********************************

# Collect videos about AI from 2020 to 2023 for each month
keyword <- "artificial intelligence"
years <- c(2020, 2021, 2022, 2023)
ai_list <- list()
x <- 1
for (i in 1:length(years)) {
  for (j in 1:12) {
    pa <- paste(years[[i]], sprintf("%02i", j), "01T00:00:00Z", sep = "-")
    pb <- ifelse(j != 12, paste(years[[i]], sprintf("%02i", j + 1), "01T00:00:00Z", sep = "-"), paste(years[[i]] + 1, "01-01T00:00:00Z", sep = "-"))
    ai_list[[x]] <- yt_search(keyword, type = "video", published_after = pa, published_before = pb, get_all=F)
    x <- x + 1
  }
  x <- x + 1
}
ai_df <- bind_rows(ai_list)

# Check for duplicates
ai_df <- ai_df %>% distinct(video_id, .keep_all = TRUE)
# Export the videos to .csv
write.csv(ai_df, "ai_videos.csv", row.names=FALSE)

# Get all comments from the videos
comments_ai_df <- data.frame()
for (i in 1:nrow(ai_df)) {
  tryCatch({
    current_comments <- get_all_comments(video_id = ai_df$video_id[[i]])
    comments_ai_df <- bind_rows(comments_ai_df, current_comments)},
  error=function(e) {
    message('Error occurred for video: ', ai_df$video_id[[i]], ' #', i)
    message(e)
  })
}

# Shuffle data and export to .csv
comments_ai_df <- comments_ai_df[sample(1:nrow(comments_ai_df)), ] 
write.csv(comments_ai_df, "ai_comments.csv", row.names=FALSE)

# **********************************************
# ***** Collecting AI videos with contexts *****
# **********************************************

contexts <- c("Healthcare", "Business", "Transportation", "Education")
keywords_1 <- list("Healthcare AI",
                   "Medical Artificial Intelligence",
                   "AI Applications in Medicine",
                   "Health Tech Innovation",
                   "Machine Learning in Healthcare",
                   "Digital Health AI",
                   "Healthcare Data Analytics",
                   "AI Diagnosis and Treatment",
                   "Clinical Decision Support Systems",
                   "Predictive Analytics in Healthcare",
                   "AI-driven Healthcare Solutions",
                   "Healthcare Automation",
                   "Smart Hospitals",
                   "Telehealth and AI",
                   "Wearables in Healthcare",
                   "Personalized Medicine with AI",
                   "Robotics in Healthcare",
                   "AI Imaging in Medicine",
                   "Patient-Centric AI",
                   "Ethical AI in Healthcare")
keywords_2 <- list("Business AI",
                   "Artificial Intelligence in Business",
                   "AI Applications in Enterprise",
                   "Corporate AI Strategy",
                   "Machine Learning for Business",
                   "Data-driven Decision Making",
                   "AI-driven Innovation",
                   "Business Process Automation with AI",
                   "Predictive Analytics in Business",
                   "AI-powered Productivity",
                   "Digital Transformation with AI",
                   "AI in Marketing and Sales",
                   "Customer Experience AI",
                   "Supply Chain AI",
                   "AI-driven Finance",
                   "Human Resources AI",
                   "Ethical AI in Business",
                   "Business Intelligence with AI",
                   "AI and Enterprise Efficiency",
                   "Strategic AI Implementation")
keywords_3 <- list("Transportation AI",
                   "AI in Autonomous Vehicles",
                   "Self-Driving Cars",
                   "Autonomous Driving Technology",
                   "AI in Transportation Systems",
                   "Connected Vehicles",
                   "Intelligent Traffic Management",
                   "AI for Fleet Management",
                   "Automated Transportation Solutions",
                   "Smart Mobility with AI",
                   "Predictive Maintenance for Vehicles",
                   "AI in Logistics",
                   "Urban Mobility AI",
                   "Robotic Vehicles",
                   "AI for Traffic Safety",
                   "Drones in Transportation",
                   "AI for Public Transport",
                   "Smart City Transportation",
                   "AI-driven Navigation",
                   "Ethical AI in Transportation")
keywords_4 <- list("Education AI",
                   "Artificial Intelligence in Education",
                   "AI for Learning Enhancement",
                   "Personalized Learning with AI",
                   "Adaptive Learning Systems",
                   "Intelligent Tutoring Systems",
                   "AI in Educational Technology",
                   "Smart Classrooms with AI",
                   "Automated Grading and Assessment",
                   "AI for Student Engagement",
                   "Educational Data Analytics",
                   "Virtual Learning Environments",
                   "AI-driven Educational Apps",
                   "Gamification in Education with AI",
                   "Robotics in Education",
                   "AI for Specialized Learning Needs",
                   "Language Learning with AI",
                   "Ethical AI in Education",
                   "AI for Educational Research",
                   "Inclusive AI Education")
keywords_contexts <- list(keywords_1, keywords_2, keywords_3, keywords_4)

# Get the videos for each context
ai_contexts_df_list <- list()

for (i in 1:length(contexts)) {
  ai_context_curr_df <- data.frame()
  for (j in 1:length(keywords_contexts[[i]])) {
    ai_context_curr_videos <- yt_search(keywords_contexts[[i]][[j]], type = "video", published_after = pa, get_all=F)
    ai_context_curr_df <- bind_rows(ai_context_curr_df, ai_context_curr_videos)
  }
  ai_contexts_df_list[[i]] <- ai_context_curr_df
}

# Check for duplicates
contexts_df_unique <- list()
for (i in 1:length(contexts)) {
  contexts_df_unique[[i]] <- ai_contexts_df_list[[i]] %>% distinct(video_id, .keep_all = TRUE)
}
# Export the videos to .csv
for (i in 1:length(contexts)) {
  write.csv(contexts_df_unique[[i]], paste(i, "_context_videos.csv"), row.names=FALSE)
}

# Get all comments from the videos
df1 <- df2 <- df3 <- df4 <- data.frame()
ai_contexts_comments_list <- list(df1, df2, df3, df4)

for (i in 1:length(contexts)) {
  curr_comment_list <- data.frame()
  for (j in 1:nrow(contexts_df_unique[[i]])) {
    tryCatch(
      {
        curr_ai_contexts_comments <- get_all_comments(video_id = contexts_df_unique[[i]]$video_id[[j]])
        curr_comment_list <- bind_rows(curr_comment_list, curr_ai_contexts_comments)},
      error=function(e) {
        message('Error occurred for video: ', contexts_df_unique[[i]]$video_id[[j]], ' Context:', i, ' #', j)
        message(e, "\n")
      }
    )
  }
  ai_contexts_comments_list[[i]] <- bind_rows(ai_contexts_comments_list[[i]], curr_comment_list)
}

# Export the comments to .csv
for (i in 1:length(contexts)) {
  write.csv(ai_contexts_comments_list[[i]], paste(i, "_context.csv"), row.names=FALSE)
}

# **************************************
# ***** Collecting AI risks videos *****
# **************************************

# Collect videos related to "AI risks" from after 2020
keywords <- c("AI risks", "Artificial intelligence risks", "AI dangers", "Artificial intelligence dangers", 
              "Disadvantages of artificial intelligence", "Risks of machine learning", "ML dangers", 
              "Ethical concerns in AI", "Autonomous systems risks", "AI safety challenges", "Unintended consequences of AI", 
              "Future of AI and risks", "Responsible AI development", "Ethical AI guidelines", "Impact of AI on society", 
              "AI concerns", "AI accountability", "AI governance", "Superintelligent AI risks", "Implications of AI")
pa <- "2020-01-01T00:00:00Z"

risks_list <- list()
for (i in 1:length(keywords)) {
  risks_list[[i]] <- yt_search(keywords[[i]], type = "video", published_after = pa, get_all=F)
}
risks_df <- bind_rows(risks_list)

# Check for duplicates
risks_df <- risks_df %>% distinct(video_id, .keep_all = TRUE)
# Export the videos to .csv
write.csv(risks_df, "risks_videos.csv", row.names=FALSE)

# Get all comments from the videos
comments_risks_df <- data.frame()
for (i in 1:nrow(risks_df)) {
  tryCatch({
    print(paste('Collecting comments for video #', i))
    current_comments <- get_all_comments(video_id = risks_df$video_id[[i]])
    comments_risks_df <- bind_rows(comments_risks_df, current_comments)
    print(paste('Successfully collected comments from video #', i))},
  error=function(e) {
    message('Error occurred for video: ', risks_df$video_id[[i]], ' #', i)
    message(e)
  })
}

# Shuffle data and export to .csv
comments_risks_df <- comments_risks_df[sample(1:nrow(comments_risks_df)), ] 
write.csv(comments_risks_df, "risks_comments.csv", row.names=FALSE)

# ************************************
# ***** Analysis of labeled data *****
# ************************************

# Import the labeled datasets
file_path_ai <- 'ai_comments_labeled.csv'
file_path_contexts <- c('1_context_labeled.csv', 
                        '2_context_labeled.csv', 
                        '3_context_labeled.csv',
                        '4_context_labeled.csv')

df_ai <- read.csv(file_path_ai)
df_ai <- df_ai[complete.cases(df_ai$Bertweet_score), ]

df_contexts <- list()
for (i in 1:length(contexts)) {
  df_contexts[[i]] <- read.csv(file_path_contexts[[i]])
  df_contexts[[i]] <- df_contexts[[i]][complete.cases(df_contexts[[i]]$Bertweet_score), ] 
}

# *********************
# ***** Bar plots *****
# *********************

# AI videos - Bar plot for the sentiment distribution
ggplot(df_ai, aes(x = Bertweet_sentiment)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal()+
  labs(title = "Sentiment Distribution", x = "Sentiment Label", y = "Count")

# Context videos - Separate bar plots for the sentiment distribution
plot_list <- c()
for (i in 1:length(contexts)) {
  plot_list[[i]] <- ggplot(df_contexts[[i]], aes(x = Bertweet_sentiment)) +
    geom_bar(fill = "skyblue", color = "black") +
    theme_minimal()+
    labs(title = "Sentiment Distribution", x = "Sentiment Label", y = "Count") +
    ggtitle(paste("Context ", i, ": ", contexts[[i]]))
}
figure <- ggarrange(plotlist = plot_list, ncol = length(contexts)/2, nrow = 2)
figure

# Comparison between number of positive comments and negative comments
difference = c(0, 0, 0, 0)
for (i in 1:length(contexts)) {
  difference[[i]] = (sum(df_contexts[[i]]$Bertweet_sentiment == 'POS')*100)/nrow(df_contexts[[i]]) - (sum(df_contexts[[i]]$Bertweet_sentiment == 'NEG')*100)/nrow(df_contexts[[i]])
}

df <- data.frame(Bars = contexts, Values = difference)
ggplot(df, aes(x = Bars, y = Values, fill = Bars)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal()+
  labs(title = "Difference between positive and negative comments (in %)", y = "%", x = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# ***********************
# ***** Time series *****
# ***********************

df_ai$publishedAt <- as.Date(df_ai$publishedAt)

# Group data by month and calculate the percentage of POS and NEG comments
sentiments_monthly <- df_ai %>%
  group_by(month = floor_date(publishedAt, "month")) %>%
  summarise(
    Percentage_POS = sum(Bertweet_sentiment == 'POS') / n() * 100,
    Percentage_NEG = sum(Bertweet_sentiment == 'NEG') / n() * 100
  )

# Create a time series line plot with a step of one month
ggplot(sentiments_monthly, aes(x = month)) +
  geom_line(aes(y = Percentage_POS), color = "green", linetype = "solid") +
  geom_line(aes(y = Percentage_NEG), color = "red", linetype = "solid") +
  scale_x_date(date_breaks = "7 month", date_labels = "%b %Y") +
  labs(title = "Time Series of % POS and % NEG Comments (Grouped by Month)",
       x = "Date",
       y = "% of Comments") +
  theme_minimal()

# ************************************
# ***** Topic Model Risks videos *****
# ************************************

# Load the dataset 
data <- read.csv(file= "risks_comments.csv", stringsAsFactors = F, encoding = "UTF-8")

# Filter comments that explicitly mention AI
phrases_to_match <- c('AI', 'A.I.', 'A.I', 'artificial intelligence', 'AIs')
pattern <- paste(phrases_to_match, collapse = '|')
pattern <- paste0('(?i)', pattern)  # Adding (?i) for case-insensitivity
data <- data[grepl(pattern, data$textOriginal), ]

# Preprocess the content of the comments
processed <- textProcessor(data$textOriginal, metadata=data, customstopwords=c('from', 'to', 'subject', 're', 'one', 'like', 'video', 'comment', 'much',
                                                                               'please', 'lol', 'really', 'first', 'would', 'could', 'should', 'going', 'get',
                                                                               'take', 'are', 'is', 'what', 'know', 'even', 'something', 'way', 'really', 'say',
                                                                               'thing', 'anything', 'talk', 'actually', 'still', 'also', 'yet', 'let', 'make',
                                                                               'set', 'more', 'other', 'yes', 'no', 'im', 'thanks', 'thank', 'oh', 'ah', 'gonna', 
                                                                               'yeah', 'ok', 'thought', 'tho', 'though', 'okay', 'look', 'much', 'looks', 'looking',
                                                                               'imma', 'hey', 'hi', 'likes', 'views', 'that', 'cant', 'doesnt', 'does', 'keep', 'tell',
                                                                               'dont', 'take', 'etc', 'say', 'says', 'said', 'told', 'well', 'just', 'come', 'came',
                                                                               'do', 'not', 'isnt', 'thing', 'can', 'use', 'need', 'many', 'lot', 'made', 'want', 'think', 'will'))

# Determine the appropriate thresholds for removing words
plotRemoved(processed$documents, lower.thresh=seq(1, 200, by=100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=1000, upper.thresh = 20000)

# Load the processed data
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Fit the Structural Topic Model
stm_fit <- stm(out$documents, out$vocab, K=10, 
               max.em.its=75, data=out$meta, init.type="Spectral", 
               seed=8458159)

# Use oolong to validate the topic model
oolong_test <- create_oolong(stm_fit)
oolong_test$do_word_intrusion_test()
oolong_test$lock()
oolong_test

# Validate the model for topics and documents
oolong_test_topic <- create_oolong(stm_fit, data$content)
oolong_test_topic$do_topic_intrusion_test()
oolong_test_topic$lock()
oolong_test_topic

# Plot a summary of topics
plot(stm_fit, type="summary", xlim=c(0,.4))
