# Text Mining and Topic Modeling with LDA

This project applies text mining and Latent Dirichlet Allocation (LDA) to analyze six classic books. Using natural language processing techniques, it extracts patterns, identifies frequently used words, and models topics in the texts to understand their thematic structure.

---

## Introduction
The aim of this project is to use LDA for topic modeling on the text of six books:
1. *A Tale of Two Cities*
2. *Adventures of Huckleberry Finn*
3. *Alice's Adventures in Wonderland*
4. *Great Expectations*
5. *The Adventures of Tom Sawyer*
6. *Through the Looking-Glass*

### Objectives:
- Preprocess the text data by removing stopwords, tokenizing, and organizing into a Document-Term Matrix (DTM).
- Apply LDA to extract themes from the books.
- Visualize the distribution of topics and their prevalence across different books and chapters.

---

## Dataset
The dataset consists of six books in plain text format, where each book is split into chapters and analyzed. Each text is processed into a structured format with the following columns:
- **Document**: Book name and chapter ID.
- **Word**: Words appearing in each chapter.
- **Frequency**: Number of times each word is repeated.

---

## Methodology
### 1. Data Preprocessing:
- Removed stopwords using pre-defined stopword lists.
- Tokenized text into individual words.
- Assigned chapter IDs to text lines based on "CHAPTER" headings.
- Constructed a Document-Term Matrix (DTM) for analysis.

### 2. Topic Modeling with LDA:
- Applied LDA with `k=3` topics to identify thematic clusters in the texts.
- Evaluated word distributions (`beta`) across topics.
- Analyzed topic proportions (`gamma`) across books and chapters.

### 3. Visualization:
- Bar charts for the most frequent words.
- Beta distribution for top words across topics.
- Gamma distribution across books and within individual chapters.

---

## Results
### 1. Top 12 Most Frequent Words:
The most common words across all six books include character names like *Joe*, *Alice*, and *Tom*.

### 2. LDA Topic Modeling:
- **Topic 1**: Captures interactions between characters with words like *time*, *looked*, *miss*, and *head*.
- **Topic 2**: Dominated by character references such as *Alice*, *Lorry*, *Defarge*, and *Queen*.
- **Topic 3**: Indicates conversational dialogues with informal terms like *ain't*, *didn't*, *couldn't*, and *warn't*.

### 3. Gamma Distribution:
- Topic 1 is most prevalent in *Great Expectations*.
- Topic 2 dominates in *Alice's Adventures in Wonderland* and *Through the Looking-Glass*.
- Topic 3 is most prominent in *The Adventures of Huckleberry Finn* and *The Adventures of Tom Sawyer*.

### 4. Chapter-Specific Gamma Distribution:
Analyzed topic prevalence within specific books (e.g., *A Tale of Two Cities*) to understand thematic shifts between chapters.

---

## Setup Instructions
### Prerequisites:
1. Install R and required libraries:
   ```R
   install.packages(c("tidytext", "readr", "dplyr", "tidyr", "tm", "tidyverse", "topicmodels", "ggplot2", "reshape2"))

Steps to Run:

Place the text files of the six books in the appropriate directory.
Run the R script to execute the analysis

## Key Findings
Word Frequency Analysis: Highlights the key terms across books, providing an overview of the themes.

LDA Topics: Effectively separates text into meaningful topics based on word usage patterns.

Topic Proportions: Provides insights into how themes vary across books and chapters.

## Future Work
Experiment with different values of k in LDA to identify more granular or broader topics.

Explore coherence scores to validate the optimal number of topics.

Apply similar techniques to larger datasets or collections of text (e.g., novels by a single author or genre-specific texts).
