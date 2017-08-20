
library(text2vec)
library(data.table)
library(tm)
library(SnowballC)
library(stringr)
library(base)
library(knitr)
library(ggplot2)
library(wordcloud)
library(plyr)
library(dplyr)


data("movie_review")
setDT(movie_review)
setkey(movie_review, id)
set.seed(2016L)


## ------------------------------------------------------------------------
all_ids = movie_review$id
train_ids = sample(all_ids, 4000)o
test_ids = setdiff(all_ids, train_ids)
colnames(movie_review)<-c("id","resposta","conteudo")
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]
train_review = train$conteudo
test_review = test$conteudo

movie_resposta=movie_review[,c("id","resposta")]


## ------------------------------------------------------------------------
# limpeza
limpeza_docs <- function (docs) {
  
  stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
  docs <- stripWhitespace(docs)
  docs <- tolower(docs)
  docs <- gsub("br", " ", docs)
  docs <- removeNumbers (docs)
  docs <- removePunctuation(docs)
  docs <- removeWords(docs,stopwords("english"))
  docs <- removeWords(docs,stop_words)
  docs <- stripWhitespace(docs)
  docs <- stemDocument(docs)
}



## ------------------------------------------------------------------------
train_clean <- limpeza_docs(train_review)
train_review[1]


## ------------------------------------------------------------------------
train_clean <- limpeza_docs(train_review)
train_clean[1]


## ------------------------------------------------------------------------
prep_fun = limpeza_docs
tok_fun = word_tokenizer

it_train = itoken(train$conteudo, 
                  preprocessor = prep_fun,
                  tokenizer = tok_fun, 
                  ids = train_ids, 
                  progressbar = FALSE)

vocab = create_vocabulary(it_train, ngram = c(1L,3L))
prune_vocab = prune_vocabulary(vocab, term_count_min = 20) 
vectorizer = vocab_vectorizer(prune_vocab)
dtm_train = create_dtm(it_train, vectorizer)

dtm_train_df=as.data.frame(as.matrix(dtm_train))
dtm_train_df$id = row.names(dtm_train_df)
dtm_train_df_target <- merge(dtm_train_df, movie_resposta, by.x="id", by.y="id", all.x=TRUE )



## ------------------------------------------------------------------------
 
freq <- sort(colSums(as.matrix(dtm_train)),decreasing=T) 
wf <- data.frame(word=names(freq), freq=freq)   
#kable(wf[1:10,])

p <- ggplot(subset(wf, freq>1000), aes(x = reorder(word, -freq), y = freq)) +
          geom_bar(stat = "identity") + 
          theme(axis.text.x=element_text(angle=45, hjust=1))
p 


## ------------------------------------------------------------------------

dtm_1 = subset(dtm_train_df_target, resposta == 1)
freq_1 <- sort(colSums(as.matrix(dtm_1[,-c(1, ncol(dtm_1))])),decreasing=T) 
wf_1 <- data.frame(word=names(freq_1), freq=freq_1)   
wordcloud(names(freq_1), freq_1, min.freq=25, scale=c(5, .2), colors=brewer.pal(6, "Dark2")) 

## ------------------------------------------------------------------------

dtm_0 = subset(dtm_train_df_target, resposta == 0)
freq_0 <- sort(colSums(as.matrix(dtm_0[,-c(1, ncol(dtm_0))])),decreasing=T) 
wf_0 <- data.frame(word=names(freq_0), freq=freq_0)   
wordcloud(names(freq_0), freq_0, min.freq=25, scale=c(5, .2), colors=brewer.pal(6, "Dark2")) 


## ------------------------------------------------------------------------

dtm_train_l1_norm = normalize(dtm_train, "l1")
dtm_train_l1_norm_df = as.data.frame(as.matrix(dtm_train_l1_norm))

dtm_train_l1_norm_df$id = row.names(dtm_train_l1_norm_df)
dtm_train_l1_norm_df_target <- merge(dtm_train_l1_norm_df, movie_resposta, by.x="id", by.y="id", all.x=TRUE)


## ------------------------------------------------------------------------
dtm_train_bin = sign(dtm_train)
dtm_train_bin_df = as.data.frame(as.matrix(dtm_train_bin))

dtm_train_bin_df$id = row.names(dtm_train_bin_df)
dtm_train_bin_df_target <- merge(dtm_train_bin_df, movie_resposta, by.x="id", by.y="id", all.x=TRUE)

dtm_tf_bin_1 = subset(dtm_train_bin_df_target, resposta == 1)
dtm_tf_bin_0 = subset(dtm_train_bin_df_target, resposta == 0)

freq_media_1 <- sort(
  colSums(as.matrix(dtm_tf_bin_1[,-c(1,ncol(dtm_tf_bin_1))]))/nrow(as.matrix(dtm_tf_bin_1[,-c(1, ncol(dtm_tf_bin_1))])), decreasing=T) 
wf_1 <- data.frame(word=names(freq_media_1), freq=freq_media_1)   

freq_media_0 <- sort(colSums(as.matrix(dtm_tf_bin_0[,-c(1,ncol(dtm_tf_bin_0))]))/
                     nrow(as.matrix(dtm_tf_bin_0[,-c(1, ncol(dtm_tf_bin_0))])),decreasing=T) 
wf_0 <- data.frame(word=names(freq_media_0), freq=freq_media_0)   

grafico_wf = cbind(wf_1[order(wf_1[,1]),], wf_0[order(wf_0[,1]),])
colnames(grafico_wf)=c("word","freq_1","word2","freq_0")

ggplot(grafico_wf, aes(x = freq_0, y = freq_1,label = word))+
  geom_abline(color = "gray40", lty = 1) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  coord_cartesian(ylim=c(0,0.6),xlim=c(0,0.6))+
  labs(x = "Freq. palavras nas reviews de sentimento negativo", 
       y = "Freq. palavras nas reviews de sentimento positivo")

ggplot(grafico_wf, aes(x = freq_0, y = freq_1,label = word))+
  geom_abline(color = "gray40", lty = 1) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  coord_cartesian(ylim=c(0,0.2),xlim=c(0,0.2))+
  labs(x = "Freq. palavras nas reviews de sentimento negativo", 
       y = "Freq. palavras nas reviews de sentimento positivo")

ggplot(grafico_wf, aes(x = freq_0, y = freq_1,label = word))+
  geom_abline(color = "gray40", lty = 1) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = FALSE, vjust = 1.5)+
  coord_cartesian(ylim=c(0.2,0.4),xlim=c(0.2,0.4))+
  labs(x = "Freq. palavras nas reviews de sentimento negativo", 
       y = "Freq. palavras nas reviews de sentimento positivo")



## ------------------------------------------------------------------------

prep_fun = limpeza_docs
tok_fun = word_tokenizer

it_train = itoken(train$conteudo, 
                  preprocessor = prep_fun,
                  tokenizer = tok_fun, 
                  ids = train_ids, 
                  progressbar = FALSE)

vocab = create_vocabulary(it_train, ngram = c(1L,3L))
prune_vocab = prune_vocabulary(vocab, 
                               doc_proportion_min = 0.01,
                               doc_proportion_max = 0.97,
                               term_count_min = 20) 
vectorizer = vocab_vectorizer(prune_vocab)
dtm_train = create_dtm(it_train, vectorizer)

dtm_train_df=as.data.frame(as.matrix(dtm_train))
dtm_train_df$ID = row.names(dtm_train_df)
dtm_train_df_target <- merge(dtm_train_df, movie_resposta, by.x="ID", by.y="id", all.x=TRUE )

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)
dtm_train_tfidf_df=as.data.frame(as.matrix(dtm_train_tfidf))
dtm_train_tfidf_df2<-dtm_train_tfidf_df
dtm_train_tfidf_df2$ID = row.names(dtm_train_tfidf_df)
dtm_train_tfidf_df_target <- merge(dtm_train_tfidf_df2, movie_resposta, by.x="ID", by.y="id")

 
max_tfidf <- apply(dtm_train_tfidf, 2, FUN=max)
plot_tfidf <- data.frame(word=colnames(dtm_train_tfidf_df), tfidf=max_tfidf)   

plot_tfidf <-   arrange(plot_tfidf, desc(tfidf)) 
plot_tfidf_15<- top_n(plot_tfidf,15,tfidf)

ggplot(plot_tfidf_15, aes(word, tfidf))+
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()



