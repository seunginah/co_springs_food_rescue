library(dplyr)
library(stringr)
library(writexl)

# the data file lives here
data_dir <- paste(getwd(), 'data', sep='/')

# open data
fname <- list.files(data_dir)[[1]]
fpath <- paste(data_dir, fname, sep='/')
raw <- read.csv(fpath, stringsAsFactors = F)

# determine new name for file
new_fname <- "robot_data_2019_cleaned"

# these are the columns that have multiple colon-separated values
sep_cols <- c('item.types', 'item.weights', 'item.descriptions')
non_sep_cols <- setdiff(names(raw_row),sep_cols)

get_new_rows <- function(raw_row, n_splits, item_types, item_weights, item_descriptions){
  # given the number of new rows that need to be made,
  # lists of item types, weights, descriptions,
  # return a data frame of rows
  new_rows <- NULL
  for (i in 1:n_splits){
    item_type <- item_types[i]
    item_weight <- item_weights[i]
    item_description <- item_descriptions[i]
    new_row <- raw_row %>% 
      mutate(item_type = item_type,
             item_weight = item_weight,
             item_description = item_description)
    
    if (is.null(new_rows)){
      new_rows <- new_row
    } else {
      new_rows <- rbind(new_rows, new_row)
    }
  }
  return(new_rows)
}


# the new data will be created here
data <- NULL

# if we have disagreements (where splitting by colons leads to different numbers of splits...) save ids here for inspection
inspect_ids <- c()

# for each row in raw data,
for (row in 1:nrow(raw)){
  # grab the row
  raw_row <- raw[row, ]
  
  # for each column that needs to be separated into multiple rows, split by colon
  item_types <- unlist(str_split(raw_row$item.types[[1]], ':'))
  item_weights <- unlist(str_split(raw_row$item.weights, ':'))
  item_descriptions <- unlist(str_split(raw_row$item.descriptions, ':'))
  n_splits <- length(item_types)
  
  # item types should always come out to same length as item weights - raise error if not
  if (length(item_types) != length(item_weights)){
    cat(stderr(), "\nitem weights != item types, \tID:", raw_row$id)
  }
  
  # noticed that sometimes the item_descritions is all blanks, and has different number
  # of colons that item types and weights. fix that
  if (length(item_descriptions) != n_splits){
    if (unique(item_descriptions) == c('') || length(item_descriptions) == 0){
      cat(stderr(), "\nitem descriptions is either empty or all empty")
      item_descriptions <- rep("", n_splits)
    } else {
      cat(stderr(), "\nitem descriptions != item types, and non empty strings\t ID:", raw_row$id)
      inspect_ids <- c(inspect_ids, raw_row$id)
    }
  }
  
  # given the raw row, the number of new rows needed, and the split out lists,
  # get the new rows as a data frame
  new_rows <- get_new_rows(raw_row, n_splits, item_types, item_weights, item_descriptions)
  
  # add the new rows to the data
  if (is.null(data)){
    data <- new_rows
  } else {
    data <- rbind(data, new_rows)
  }
}

# if inspect ids was not empty
if (length(inspect_ids > 0)){
  raw %>% filter(id %in% inspect_ids) %>% View()
}

# select columns in this order
new_cols <- names(raw)
new_cols <- append(new_cols, c("item_type"), after=which(new_cols=='item.types')-1)
new_cols <- append(new_cols, c("item_weight"), after=which(new_cols=='item.weights')-1)
new_cols <- append(new_cols, c("item_description"), after=which(new_cols=='item.descriptions')-1)

data <- data %>% select(new_cols)

# output to csv and xlsx
fpath <- paste(data_dir, new_fname, sep='/')
write.csv(data, paste0(fpath, ".csv"))
write_xlsx(data, paste0(fpath, ".xlsx"))
