library(gutenbergr)
library(tm)
library(tidyverse)
library(tidytext)
library(tm.plugin.mail)

# get the stop words data
data("stop_words")


# now try to do the same with the document
setwd("/Users/clee/Documents/home/cuny_professional_studies/data_607/git/607_document_classification")




# try to read the directory of files
filenames <- list.files("spam", pattern="*.*", full.names=TRUE)

# now read all of these files into a tibble with column names(filename and text?)
all_spam <- tibble()

spam_words <- tibble(
  word = "a",
  spam_word_count  = 1
)

stopper_limit <- 5000
stopper <- 0
start_time <- Sys.time()
#fn <- "spam/00001.317e78fa8ee2f54cd4890fdc09ba8176"
for (fn in filenames) {
  # injest the file
  spam <- read_file(fn)
  
  print(fn)
  
  email_message <- VCorpus(MBoxSource(fn), readerControl = list(reader = readMail))
  
    # skip if there are attachments or weird base64 assets because they mess up
    # unnest tokens
    if(str_detect(spam,"Content-Transfer-Encoding: base64")) {
      next
    }
  
    # skip if the length is zero
    if (length(email_message) == 0) {
      next
    }

    # skip if we can't get the content type?
    if (is.na(email_message[[1]]$meta$header)) {
      next
    }
  
    header <- email_message[[1]]$meta$header
    hits <- str_match(names(header), regex('content-type', ignore_case = T))
    content_type_index <- which(!is.na(hits))
    length(content_type_index)
  
    # skip if this email is unparseable
    if (length(content_type_index) == 0) {
      next
    } else {
      content_type <- header[content_type_index]
    }
    
    content <- email_message[[1]]$content
    # determine if it's a plain-text or html spam
  if (str_detect(content_type, regex('plain', ignore_case = T))) {

    print("plain")
    
    spam_lines <- unlist(str_split(spam, "(\n| )"))
    spam_tibble <- tibble(
      #word = spam_lines,
      #text = spam_lines,
      text = content,
      )
    
    spam_words_tmp <- spam_tibble %>% 
      unnest_tokens(word, text)
      
    spam_words_tmp <- spam_words_tmp %>% 
      anti_join(stop_words, by="word") %>%
      count(word, name="spam_word_count") %>%
      arrange(desc(spam_word_count))
    
    #spam_words_tmp <- spam_words_tmp %>% mutate (filename = fn)
    #spam_words <- bind_rows(spam_words,spam_words_tmp)
    
  } else {
    print ("html")
    
    spam_tibble <- tibble(
      #text = spam,
      text = content,
    )
    
    spam_words_tmp <- spam_tibble %>% 
      unnest_tokens(word, text,format = "html")

    spam_words_tmp <- spam_words_tmp %>% 
      anti_join(stop_words, by="word")  %>%
      count(word, name="spam_word_count") %>%
      arrange(desc(spam_word_count))
    #spam_words_tmp <- spam_words_tmp %>% mutate (filename = fn)
    
  }
    #spam_words <- bind_rows(spam_words,spam_words_tmp)
    # if (stopper == 2) {
    #   break
    # }
    
  spam_words <- merge(x = spam_words, y = spam_words_tmp, by.x = "word", by.y = "word", all.x=TRUE, all.y = TRUE)
  
  #spam_words
  spam_words[is.na(spam_words)] <- 0
  spam_words <- mutate(spam_words, spam_word_count = spam_word_count.x + spam_word_count.y)
  spam_words <- spam_words[-c(2,3)]
  #spam_words   

  
  stopper <- stopper + 1
  print (paste("Iteration", stopper))
  if (stopper >= stopper_limit) {
    break
  }
  # 
  # spam_lines <- unlist(str_split(spam, "(\n| )"))
  # spam_tibble <- tibble(
  #   text = spam_lines,
  # )
  # 
  # 
  # spam_words_tmp <- spam_tibble %>% 
  #   anti_join(stop_words, by=c("text" = "word")) %>%
  #   unnest_tokens(word, text) %>%
  #   count(word, name="spam_word_count") %>%
  #   arrange(desc(spam_word_count))
  # 
  # spam_words_tmp <- spam_words_tmp %>% mutate (filename = fn)
  # 
  # spam_words <- bind_rows(spam_words,spam_words_tmp)
  # 
}

# only keep words that are at least three letters long
spam_words <- spam_words %>% filter (str_length(word) >= 3)

end_time <- Sys.time()

end_time - start_time

# # try to use tm.plugin.mail
# mbox <- MBoxSource("./spam")
# convert_mbox_eml(mbox = "/Users/clee/Documents/home/cuny_professional_studies/data_607/git/607_document_classification/spam", dir = "./tmp2")
# 
# mail_messages <- VCorpus(MBoxSource("spam/00024.6b5437b14d403176c3f046c871b5b52f"), readerControl = list(reader = readMail))
# removeMultipart(mail_messages[[1]])
# mail_messages[[1]]$meta$header$`Content-Type`
# mail_messages[[1]]$content
# 
# 
# 
# ## start parsing
# all_spam <- tibble()
# spam_words <- tibble()
# filenames <- list.files("spam", pattern="*.*", full.names=TRUE)
# for (fn in filenames) {
#   email_message <- VCorpus(MBoxSource(fn), readerControl = list(reader = readMail))
#   print (fn)
#   
#   # skip if the length is zero
#   if (length(email_message) == 0) {
#     next 
#   }
#   
#   # skip if we can't get the content type?
#   if (is.na(email_message[[1]]$meta$header)) {
#     next
#   }
# 
#   # let's handle the text/plain header
#   header <- email_message[[1]]$meta$header
#   hits <- str_match(names(header), regex('content-type', ignore_case = T))
#   content_type_index <- which(!is.na(hits))
#   length(content_type_index)
#   
#   if (length(content_type_index) >0) {
#     content_type <- header[content_type_index]
#     #print (content_type)
#     if (str_detect(content_type, regex('plain', ignore_case = T))) {
#       print ("plain")
#       
#       spam_lines <- unlist(str_split(spam, "(\n| )"))
#       spam_tibble <- tibble(
#         text = spam_lines,
#       )
#       
#       
#       spam_words_tmp <- spam_tibble %>% 
#         anti_join(stop_words, by=c("text" = "word")) %>%
#         unnest_tokens(word, text) %>%
#         count(word, name="spam_word_count") %>%
#         arrange(desc(spam_word_count))
#       
#       spam_words_tmp <- spam_words_tmp %>% mutate (filename = fn)
#       
#       spam_words <- bind_rows(spam_words,spam_words_tmp)
#       
#       
#     } else if (str_detect(content_type, regex('html', ignore_case = T))) {
#       print ("html")
# 
#       
#       df <- email_message[[1]]$content %>% unnest_tokens(ngram, y, token = "ngrams", n = 1)
#       break
#       
#       
#     } else if (str_detect(content_type, regex('multipart', ignore_case = T))) {
#       print ("multipart")
#       
#       # now find the boundary token from the content type
#       matches <- str_match(content_type, regex("boundary=\"(.*)", ignore_case = T))
#       boundary <- print(matches[2])
#       print (boundary)
#       content_type
#       
#       content <- email_message[[1]]$content
#       boundary_count <- 0
#       
#       message_body <- ""
#       for (c in content) {
#         if (str_detect(c, boundary)) {
#           boundary_count <- boundary_count + 1
#         }
#         
#         # if we have an odd number of boundaries then we have a new message body section
#         if (boundary_count %% 1 == 1) {
#           message_body <- paste(message_body, c)
#         }
#         
#         
#       }
#       
#       
#       break
#     } else {
#       print ("could not find content type")
#       break
#     }
#     
#   }
#   
# 
#   print("")
#   
# }


# content_type <- 'Content-Type: multipart/mixed; boundary="----=_NextPart_000_00C2_37C70C2D.A8844B81"'
# matches <-str_match(content_type, regex("boundary=\"-+=(.*)", ignore_case = T))
# matches[2]
# ?str_match

spam_words %>% arrange(desc(spam_word_count)) %>% filter(spam_word_count > 50) %>% nrow()

#write.csv(spam_words, file = "spam_words.csv")




# now let's build up ham words

filenames <- list.files("ham", pattern="*.*", full.names=TRUE)

# now read all of these files into a tibble with column names(filename and text?)
all_ham <- tibble()

ham_words <- tibble(
  word = "a",
  ham_word_count  = 1
)

stopper_limit <- 5000
stopper <- 0
start_time <- Sys.time()
#fn <- "ham/00001.317e78fa8ee2f54cd4890fdc09ba8176"
for (fn in filenames) {
  # injest the file
  ham <- read_file(fn)
  
  print(fn)
  
  email_message <- VCorpus(MBoxSource(fn), readerControl = list(reader = readMail))
  
  # skip if there are attachments or weird base64 assets because they mess up
  # unnest tokens
  if(str_detect(ham,"Content-Transfer-Encoding: base64")) {
    next
  }
  
  # skip if the length is zero
  if (length(email_message) == 0) {
    next
  }
  
  # skip if we can't get the content type?
  if (is.na(email_message[[1]]$meta$header)) {
    next
  }
  
  header <- email_message[[1]]$meta$header
  hits <- str_match(names(header), regex('content-type', ignore_case = T))
  content_type_index <- which(!is.na(hits))
  length(content_type_index)
  
  # skip if this email is unparseable
  if (length(content_type_index) == 0) {
    next
  } else {
    content_type <- header[content_type_index]
  }
  
  content <- email_message[[1]]$content
  # determine if it's a plain-text or html ham
  if (str_detect(content_type, regex('plain', ignore_case = T))) {
    
    print("plain")
    
    ham_lines <- unlist(str_split(ham, "(\n| )"))
    ham_tibble <- tibble(
      #word = ham_lines,
      #text = ham_lines,
      text = content,
    )
    
    ham_words_tmp <- ham_tibble %>% 
      unnest_tokens(word, text)
    
    ham_words_tmp <- ham_words_tmp %>% 
      anti_join(stop_words, by="word") %>%
      count(word, name="ham_word_count") %>%
      arrange(desc(ham_word_count))
    
    #ham_words_tmp <- ham_words_tmp %>% mutate (filename = fn)
    #ham_words <- bind_rows(ham_words,ham_words_tmp)
    
  } else {
    print ("html")
    
    ham_tibble <- tibble(
      #text = ham,
      text = content,
    )
    
    ham_words_tmp <- ham_tibble %>% 
      unnest_tokens(word, text,format = "html")
    
    ham_words_tmp <- ham_words_tmp %>% 
      anti_join(stop_words, by="word")  %>%
      count(word, name="ham_word_count") %>%
      arrange(desc(ham_word_count))
    #ham_words_tmp <- ham_words_tmp %>% mutate (filename = fn)
    
  }
  #ham_words <- bind_rows(ham_words,ham_words_tmp)
  # if (stopper == 2) {
  #   break
  # }
  
  ham_words <- merge(x = ham_words, y = ham_words_tmp, by.x = "word", by.y = "word", all.x=TRUE, all.y = TRUE)
  
  #ham_words
  ham_words[is.na(ham_words)] <- 0
  ham_words <- mutate(ham_words, ham_word_count = ham_word_count.x + ham_word_count.y)
  ham_words <- ham_words[-c(2,3)]
  #ham_words   
  
  
  stopper <- stopper + 1
  print (paste("Iteration", stopper))
  if (stopper >= stopper_limit) {
    break
  }

}

# only keep words that are at least three letters long
ham_words <- ham_words %>% filter (str_length(word) >= 3)

write_csv(ham_words, "ham_words.csv")

end_time <- Sys.time()

end_time - start_time




# dissect the spam subject words
# try to read the directory of files
filenames <- list.files("spam", pattern="*.*", full.names=TRUE)

# now read all of these files into a tibble with column names(filename and text?)

spam_subject_words <- tibble(
  word = "a",
  spam_subject_word_count  = 1
)

stopper_limit <- 10000
stopper <- 0
start_time <- Sys.time()
#fn <- "spam_subject/00001.317e78fa8ee2f54cd4890fdc09ba8176"
for (fn in filenames) {
  # injest the file
  spam_subject <- read_file(fn)
  
  if (str_detect(spam_subject, "\n(S|s)ubject: .*?\n")) {
    subject <- str_to_lower( str_match(spam_subject, "\nSubject: (.*?)\n"))
    spam_subject_tibble <- tibble(
      text = subject[2]
    )
    print (paste(stopper, subject[2]))
  }

  spam_subject_tibble
    spam_subject_words_tmp <- spam_subject_tibble %>% 
      unnest_tokens(word, text)
    spam_subject_words_tmp
    spam_subject_words_tmp <- spam_subject_words_tmp %>% 
      anti_join(stop_words, by="word") %>%
      count(word, name="spam_subject_word_count") %>%
      arrange(desc(spam_subject_word_count))
    
    spam_subject_words <- merge(x = spam_subject_words, y = spam_subject_words_tmp, by.x = "word", by.y = "word", all.x=TRUE, all.y = TRUE)
    spam_subject_words[is.na(spam_subject_words)] <- 0
    spam_subject_words <- mutate(spam_subject_words, spam_subject_word_count = spam_subject_word_count.x + spam_subject_word_count.y)
    spam_subject_words <- spam_subject_words[-c(2,3)]
    spam_subject_words
    
    

  stopper <- stopper + 1
  print (paste("Iteration", stopper))
  if (stopper >= stopper_limit) {
    break
  }

}


# only keep words that are at least three letters long
spam_subject_words <- spam_subject_words %>% filter (str_length(word) >= 3)
spam_subject_words %>% arrange(desc(spam_subject_word_count)) %>% head()
end_time <- Sys.time()

end_time - start_time

write_csv(spam_subject_words, "spam_subject_words.csv")


# the below command cleaned up base64 content from email messages
# LC_ALL=C sed -i '' -e 's/^[0-9a-zA-Z\+=/]\{20,\}$//g' *

# get the spam and ham dictionaries
spam_subject_words_dictionary <- read_csv("spam_subject_words.csv")
spam_words_dictionary <- read_csv("spam_words.csv")
ham_words_dictionary <- read_csv("ham_words.csv")

a <- function(b) { 
  return (tibble( 
      a = 1,
      b = 2, 
      c = 3
    )
  )
  }

t <- a("moo")

get_spam_subject_metric <- function(filename) { 
  spam_message <- read_file(filename)
  
  subject <- str_to_lower( str_match(spam_message, "\nSubject: (.*?)\n"))
  spam_subject_tibble <- tibble(
    text = subject[2]
  )
  spam_subject_words_tmp <- spam_subject_tibble %>% 
    unnest_tokens(word, text)
  spam_subject_word_count <- nrow(spam_subject_words_tmp)
  
  spam_subject_words_tmp <- inner_join(spam_subject_words_tmp, spam_subject_words_dictionary)
  spam_subject_metric <- nrow(spam_subject_words_tmp) / spam_subject_word_count
  return (spam_subject_metric)
}


get_spam_body_metric <- function(filename) { 
  email_message <- VCorpus(MBoxSource(filename), readerControl = list(reader = readMail))
  
  if (length(email_message) == 0) {
    return (0)
  }
  
  message_body <- email_message[[1]]$content
  message_body_tibble <- tibble(
    text = message_body
  )
  message_body
  message_body_tibble <- message_body_tibble %>% 
    unnest_tokens(word, text, format="html")
  message_body_count <- nrow(message_body_tibble)
  spam_body_tibble <- inner_join(message_body_tibble, spam_words_dictionary)
  spam_body_metric <- nrow(spam_body_tibble) / message_body_count
  return (spam_body_metric)
}

get_ham_body_metric <- function(filename) {
  email_message <- VCorpus(MBoxSource(filename), readerControl = list(reader = readMail))
  
  if (length(email_message) == 0) {
    return (0)
  }
  
  
  message_body <- email_message[[1]]$content
  message_body_tibble <- tibble(
    text = message_body
  )
  message_body
  message_body_tibble <- message_body_tibble %>% 
    unnest_tokens(word, text, format="html")
  message_body_count <- nrow(message_body_tibble)

  ham_body_tibble <- inner_join(message_body_tibble, ham_words_dictionary)
  ham_body_metric <- nrow(ham_body_tibble) / message_body_count
  return (ham_body_metric)
}





# now read a spam and see if we can generate three metrics: number of spam words; num of ham words; num of spam subject
fn <- "spam/0481.77b1644dfd682bf753d5894ad04f8020"
spam_message <- read_file("spam/0481.77b1644dfd682bf753d5894ad04f8020")

subject <- str_to_lower( str_match(spam_message, "\nSubject: (.*?)\n"))
spam_subject_tibble <- tibble(
  text = subject[2]
)
spam_subject_words_tmp <- spam_subject_tibble %>% 
  unnest_tokens(word, text)
spam_subject_word_count <- nrow(spam_subject_words_tmp )

spam_subject_words_tmp <- inner_join(spam_subject_words_tmp, spam_subject_words_dictionary)
spam_subject_metric <- nrow(spam_subject_words_tmp) / spam_subject_word_count
spam_subject_metric

# now get the spam_body_metric
email_message <- VCorpus(MBoxSource(fn), readerControl = list(reader = readMail))
message_body <- email_message[[1]]$content
message_body_tibble <- tibble(
  text = message_body
)
message_body
message_body_tibble <- message_body_tibble %>% 
  unnest_tokens(word, text, format="html")
message_body_count <- nrow(message_body_tibble)
spam_body_tibble <- inner_join(message_body_tibble, spam_words_dictionary)
spam_body_metric <- nrow(spam_body_tibble) / message_body_count
spam_body_metric

# now calculate the ham body metric
ham_body_tibble <- inner_join(message_body_tibble, ham_words_dictionary)
ham_body_metric <- nrow(ham_body_tibble) / message_body_count
ham_body_metric




# now create our cases
filenames <- list.files("spam", pattern="^04", all.files = FALSE,full.names=TRUE)

# read 100 spam messages
stopper <- 0
email_messages <- tibble()
for (fn in filenames) {
  print (paste(fn, stopper))
    new_observation <- tibble(
        name = fn,
        type = 'spam',
        spam_subject_metric = get_spam_subject_metric(fn),
        spam_body_metric = get_spam_body_metric(fn),
        ham_body_metric = get_ham_body_metric(fn)
      )
    email_messages <- bind_rows(email_messages, new_observation)
    print (stopper)
    stopper <- stopper + 1
}

# now read 100 ham messages
# now create our cases
filenames <- list.files("ham", pattern="^07", full.names=TRUE)

stopper <- 0
for (fn in filenames) {
  print (paste(fn, stopper))
  new_observation <- tibble(
    name = fn,
    type = 'ham',
    spam_subject_metric = get_spam_subject_metric(fn),
    spam_body_metric = get_spam_body_metric(fn),
    ham_body_metric = get_ham_body_metric(fn)
  )
  email_messages <- bind_rows(email_messages, new_observation)
  print (stopper)
  stopper <- stopper + 1
}


library(DiceDesign)
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(mlbench)
library(rsample)
library(recipes)
library(parsnip)
library(yardstick)

set.seed(234589)
email_split <- initial_split(email_messages, 
                                prop = 3/4)
email_split

email_train <- training(email_split)
email_test <- testing(email_split)

email_cv <- vfold_cv(email_train)

# define the recipe
email_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(type ~ spam_subject_metric +spam_body_metric + ham_body_metric, 
         data = email_messages) %>%
  step_normalize(all_numeric()) %>%
  step_impute_knn(all_predictors())


email_recipe


email_train_preprocessed <- email_recipe %>%
  # apply the recipe to the training data
  prep(email_train) %>%
  # extract the pre-processed training dataset
  juice()
email_train_preprocessed


rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune()) %>%
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification") 


# set the workflow
rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(email_recipe) %>%
  # add the model
  add_model(rf_model)
rf_workflow

rf_grid <- expand.grid(mtry = c(2,3))
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = email_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

rf_tune_results %>%
  collect_metrics()

param_final <- rf_tune_results %>%
  select_best(metric = "accuracy")
param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)


rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(email_split)
rf_fit


test_performance <- rf_fit %>% collect_metrics()
test_performance

test_predictions <- rf_fit %>% collect_predictions()
test_predictions

final_model <- fit(rf_workflow, email_messages)
final_model


test_filename <- "spam/00951.f7044a1b178dc3dcff44932f840b8805"
test_filename <- "ham/2531.8f40eb6fd94f9e48d56f6374ddc3427a"
new_email_subject_metric <- get_spam_subject_metric(test_filename)
new_email_spam_body_metric <- get_spam_body_metric(test_filename)
new_email_ham_body_metric <- get_ham_body_metric(test_filename)

new_email <- tribble(~spam_subject_metric, ~spam_body_metric, ~ham_body_metric,
                     new_email_subject_metric, new_email_spam_body_metric, new_email_ham_body_metric
                     )
  
new_email

predict(final_model, new_data = new_email)
