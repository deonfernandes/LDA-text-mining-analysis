library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(tm)
library(tidyverse)
library(topicmodels)
library(ggplot2)
library(reshape2)

#Import the books
book_filenames <- c("A_Tale_of_Two_Cities.txt",
                    "Adventures_of_Huckleberry_Finn.txt",
                    "Alices_Adventures_in_Wonderland.txt",
                    "Great_Expectations.txt",
                    "The_Adventures_of_Tom_Sawyer.txt",
                    "Through_the_Looking-Glass.txt")

# Read lines for each book and create a list of texts
books <- lapply(book_filenames, readLines)

#############################################

# Select the first book from the list
read_book1 <- books[[1]]
print(read_book1)

book_title <- "A Tale of Two Cities"  

# Create a dataframe with text and line numbers
df <- tibble(
  text = read_book1,
  title = rep(book_title, length(read_book1))) %>%
  mutate(line_number = row_number())

# Identify lines that contain the 'CHAPTER' followed roman numerals to enter the chapter ID
df <- df %>%
  mutate(is_chapter_start = if_else(str_detect(text, regex("^CHAPTER\\s+[IVXLCDM0-9]+", ignore_case = TRUE)), TRUE, FALSE),
         chapter_id = cumsum(is_chapter_start))

# Extract only the chapter lines and their IDs
chapter_lines <- df %>%
  filter(is_chapter_start) %>%
  select(chapter_line = line_number, chapter_id)

# Join to the original dataframe to fill in the chapter line number for all rows
df <- df %>%
  select(-is_chapter_start) %>%
  left_join(chapter_lines, by = "chapter_id") %>%
  arrange(line_number) %>%
  fill(chapter_line, .direction = "down")

# remove NA
df <- df %>%
  filter(!is.na(chapter_line))

# Exclude extra rows from row number 15826 till the end
df <- df %>%
  slice(1:15825)

#Eliminate the line number
df <- df %>%
  select(-line_number)

#eliminate the column
df <- df %>%
  select(-chapter_line)

# print dataframe
print(df)

############

# Select the second book from the list
read_book2 <- books[[2]]
print(read_book1)

book_title <- "Adventures of Huckleberry Finn" 

# Create a dataframe with text and line numbers
df2 <- tibble(
  text = read_book2,
  title = rep(book_title, length(read_book2))) %>%
  mutate(line_number = row_number())

# Identify lines that contain the 'CHAPTER' followed roman numerals to enter the chapter ID
df2 <- df2 %>%
  mutate(is_chapter_start = if_else(str_detect(text, regex("^CHAPTER\\s+[IVXLCDM0-9]+", ignore_case = TRUE)), TRUE, FALSE),
         chapter_id = cumsum(is_chapter_start))

# extract only the chapter lines and their IDs
chapter_lines <- df2 %>%
  filter(is_chapter_start) %>%
  select(chapter_line = line_number, chapter_id)

# Join this back to the original dataframe to fill in the chapter line number for all rows
df2 <- df2 %>%
  select(-is_chapter_start) %>%
  left_join(chapter_lines, by = "chapter_id") %>%
  arrange(line_number) %>%
  fill(chapter_line, .direction = "down")

#remove NA
df2 <- df2 %>%
  filter(!is.na(chapter_line))

# Exclude all other extra rows
df2 <- df2 %>%
  slice(372:11949)

#Eliminate the line number
df2 <- df2 %>%
  select(-line_number)

#eliminate the column
df2 <- df2 %>%
  select(-chapter_line)

# print dataframe
print(df2)



###########


# Select the first book from the list
read_book3 <- books[[3]]
print(read_book1)

book_title <- "Alices Adventures in Wonderland"  

# Create a dataframe with text and line numbers
df3<- tibble(
  text = read_book3,
  title = rep(book_title, length(read_book3))) %>%
  mutate(line_number = row_number())

# Identify lines that contain the 'CHAPTER' followed roman numerals to enter the chapter ID
df3 <- df3 %>%
  mutate(is_chapter_start = if_else(str_detect(text, regex("^CHAPTER\\s+[IVXLCDM0-9]+", ignore_case = TRUE)), TRUE, FALSE),
         chapter_id = cumsum(is_chapter_start))

# extract only the chapter lines and their IDs
chapter_lines <- df3 %>%
  filter(is_chapter_start) %>%
  select(chapter_line = line_number, chapter_id)

# Join this back to the original dataframe to fill in the chapter line number for all rows
df3 <- df3 %>%
  select(-is_chapter_start) %>%
  left_join(chapter_lines, by = "chapter_id") %>%
  arrange(line_number) %>%
  fill(chapter_line, .direction = "down")

# remove NA
df3 <- df3 %>%
  filter(!is.na(chapter_line))

# Exclude rows from row number 3352 till the end
df3 <- df3 %>%
  slice(1:3351)

#Eliminate the line number
df3 <- df3 %>%
  select(-line_number)

#eliminate the column
df3 <- df3 %>%
  select(-chapter_line)

# Print dataframe
print(df3)


##############################################

# Select the first book from the list
read_book4 <- books[[4]]
print(read_book1)

book_title <- "Great Expectations"  

# Create a dataframe with text and line numbers
df4 <- tibble(
  text = read_book4,
  title = rep(book_title, length(read_book4))) %>%
  mutate(line_number = row_number())

# Identify lines that contain the 'CHAPTER' followed roman numerals to enter the chapter ID
df4 <- df4 %>%
  mutate(is_chapter_start = if_else(str_detect(text, regex("^CHAPTER\\s+[IVXLCDM0-9]+", ignore_case = TRUE)), TRUE, FALSE),
         chapter_id = cumsum(is_chapter_start))

#extract only the chapter lines and their IDs
chapter_lines <- df4 %>%
  filter(is_chapter_start) %>%
  select(chapter_line = line_number, chapter_id)

# Join this back to the original dataframe to fill in the chapter line number for all rows
df4 <- df4 %>%
  select(-is_chapter_start) %>%
  left_join(chapter_lines, by = "chapter_id") %>%
  arrange(line_number) %>%
  fill(chapter_line, .direction = "down")

#remove NA
df4 <- df4 %>%
  filter(!is.na(chapter_line))

# Exclude rows from row number 20319 till the end
df4 <- df4 %>%
  slice(1:20318)

#Eliminate the line number
df4 <- df4 %>%
  select(-line_number)

#eliminate the column
df4 <- df4 %>%
  select(-chapter_line)

# Print dataframe
print(df4)



#############


# Select the first book from the list
read_book5 <- books[[5]]
print(read_book1)

book_title <- "The Adventures of Tom Sawyer" 

# Create a dataframe with text and line numbers
df5 <- tibble(
  text = read_book5,
  title = rep(book_title, length(read_book5))) %>%
  mutate(line_number = row_number())

# Identify lines that contain the 'CHAPTER' followed roman numerals to enter the chapter ID
df5 <- df5 %>%
  mutate(is_chapter_start = if_else(str_detect(text, regex("^CHAPTER\\s+[IVXLCDM0-9]+", ignore_case = TRUE)), TRUE, FALSE),
         chapter_id = cumsum(is_chapter_start))

# extract only the chapter lines and their IDs
chapter_lines <- df5 %>%
  filter(is_chapter_start) %>%
  select(chapter_line = line_number, chapter_id)

# Join this back to the original dataframe to fill in the chapter line number for all rows
df5 <- df5 %>%
  select(-is_chapter_start) %>%
  left_join(chapter_lines, by = "chapter_id") %>%
  arrange(line_number) %>%
  fill(chapter_line, .direction = "down")

#remove NA
df5 <- df5 %>%
  filter(!is.na(chapter_line))

# Exclude all extra rows 
df5 <- df5 %>%
  slice(447:8871)

#Eliminate the line number
df5 <- df5 %>%
  select(-line_number)

#eliminate the column
df5 <- df5 %>%
  select(-chapter_line)

# Print dataframe
print(df5)


##############


# Select the first book from the list
read_book6 <- books[[6]]
print(read_book1)

book_title <- "Through the Looking Glass"  

# Create a dataframe with text and line numbers
df6 <- tibble(
  text = read_book6,
  title = rep(book_title, length(read_book6))) %>%
  mutate(line_number = row_number())

# Identify lines that contain the 'CHAPTER' followed roman numerals to enter the chapter ID
df6 <- df6 %>%
  mutate(is_chapter_start = if_else(str_detect(text, regex("^CHAPTER\\s+[IVXLCDM0-9]+", ignore_case = TRUE)), TRUE, FALSE),
         chapter_id = cumsum(is_chapter_start))

# extract only the chapter lines and their IDs
chapter_lines <- df6 %>%
  filter(is_chapter_start) %>%
  select(chapter_line = line_number, chapter_id)

# Join this back to the original dataframe to fill in the chapter line number for all rows
df6 <- df6 %>%
  select(-is_chapter_start) %>%
  left_join(chapter_lines, by = "chapter_id") %>%
  arrange(line_number) %>%
  fill(chapter_line, .direction = "down")

#remove NA
df6 <- df6 %>%
  filter(!is.na(chapter_line))

# Exclude rows from row number 3937 till the end
df6 <- df6 %>%
  slice(1:3936)

#Eliminate the line number
df6 <- df6 %>%
  select(-line_number)

#eliminate the column
df6 <- df6 %>%
  select(-chapter_line)

# Print dataframe
print(df6)



#############################################################

# Merge all dataframes into a single dataframe and filter out any NA values
all_books <- bind_rows(df, df2, df3, df4, df5, df6)%>% 
  filter(!is.na(text))
print(all_books)

#create a new column "document" from the title and chapter
books <- all_books %>% 
  unite(document, title, chapter_id)

# tokenize the text into words
chapters_by_word <- books %>%
  unnest_tokens(word, text)
print(head(chapters_by_word))

#stopwords
data("stop_words")

# remove stopwords and count the words
word_counts <- chapters_by_word %>% 
  anti_join(stop_words, by = "word") %>% 
  count(document, word, sort = TRUE)
print(word_counts)

nrow(word_counts)
ncol(word_counts)


################


#Plot Top 12 most frequest words
total_word_counts <- word_counts %>%
  group_by(word) %>%
  summarize(total_frequency = sum(n)) %>%
  ungroup() %>%
  top_n(12, total_frequency) %>%
  arrange(desc(total_frequency))

ggplot(total_word_counts, aes(x = reorder(word, total_frequency), y = total_frequency, fill = word)) +
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Total Frequency", title = "Top 12 Most Frequent Words Across All Documents") +
  theme_minimal() +
  theme(legend.position = "none") 


####################


# Create a Document-Term Matrix
document_term_matrix <- word_counts %>% 
  cast_dtm(document, word, n)

view_doc_term_matrix <- as.matrix(document_term_matrix)
nrow(view_doc_term_matrix)
ncol(view_doc_term_matrix)


#####################

# Create LDA model
lda_model <- LDA(document_term_matrix, k = 3, control = list(seed = 1111))

#top terms
top_terms <- terms(lda_model, 10)
print(top_terms)

####################

#Beta - probability of the word associated with the topic
terms_beta <- tidy(lda_model, matrix = "beta")
print(terms_beta)

#group by topic
terms_beta %>% 
  group_by(topic) %>% 
  summarise(n = n(), sum = sum(beta))

# top 10 words for each topic
top_10_beta <- terms_beta %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  mutate(term = fct_reorder(term, beta),
         topic = paste("Topic", topic))  

print(top_10_beta, n = 15)

#Plot
ggplot(top_10_beta, aes(x = term, y = beta , fill = topic)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  facet_wrap(~topic, scales = "free") +
  theme_light()


##############

#gamma - the probability of a topic in a book
gamma_documents <- tidy(lda_model, matrix = "gamma")

# separate document into book and chapter
gamma_documents <- gamma_documents %>%
  separate(document, into = c("book", "chapter"), sep = "_", remove = FALSE) %>%
  mutate(topic = as.factor(topic))  

# line chart
ggplot(line_data, aes(x = book, y = mean_probability, color = topic, group = topic, linetype = topic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +  
  scale_color_manual(values = c("blue", "green", "red")) +  # change the color of each line
  scale_linetype_manual(values = c("solid", "twodash", "dotted")) +  #change the type of line
  labs(x = "Book", y = "gamma", title = "Distribution of gamma across all the Books") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################


# gamma for a single book "A Tale of Two Cities"
specific_book_data <- gamma_documents %>%
  filter(book == "A Tale of Two Cities")

# plot of the histogram
ggplot(specific_book_data, aes(x = gamma, fill = topic)) +
  geom_histogram(bins = 30, alpha = 0.6) + 
  facet_wrap(~topic) +  # Separate plots for each topic
  labs(x = "Gamma", y = "Count", title = "Gamma Distribution in the Topic in 'A Tale of Two Cities'") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "top") 
