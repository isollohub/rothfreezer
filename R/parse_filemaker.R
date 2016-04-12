library(readr)
library(tidyr)
library(dplyr)

parse_filemaker_export <- function( file , write = F ) {
  

  strains <-
    read_csv(
      file,
      col_names = c('set', 'number', 'genotype', 'notes', 'parent', 'made_by', 
                    'dna_made', 'shelf', 'rack', 'pullout', 'insert', 'row', 'column',
                    'date', 'origin', 'original_number', 'plasmid', 'extension',
                    'location', 'strain_name', 'dash', 'reference', 'strain_id',
                    'parent1', 'parent2', 'parents'),
      col_types = cols(.default = col_character()),
      na = c('', 'NA', ' ', '"', '""', '"""', '""""')
    )

  strains$entry <- 1:nrow(strains)
  
  cleanish <-
    strains %>%
    mutate_each(funs(gsub('\v.*', '', .)), set, number, extension, shelf) %>% # Remove everything after a \v (appears to behave like a newline)
    mutate_each(funs(gsub('[^0-9]','', .)), shelf, rack, pullout, insert)  %>% # Yes, some entries do have weird characters next to the shelf/rack/pullout number
    mutate_each(funs(substr(., 1, 1)), shelf, rack, pullout, insert, row, column) %>%      # After checking, only first number is relevant
    mutate_each(funs(toupper(.)), row, column)  %>%
    mutate_each(funs(suppressWarnings(ifelse(is.na(as.integer(.)), match(.,LETTERS), .))) , row, column) %>%
    mutate(
      extension = ifelse(is.na(extension), '', extension),
      set = toupper(set),
      set = gsub('^P', 'p', set),
      strain_id = paste0(set, number, '-', extension),
      strain_id = gsub('-{1,}$', '', strain_id),
      location = paste0(shelf, rack, pullout, insert, row, column)
    ) %>%
    mutate_each(funs(gsub('\t|\v', ' ', .))) %>%         # replace tabls with space
    mutate_each(funs(gsub('^\\s{1,}|\\s{1,}$', '', .))) %>%  # Remove \v (?!) and leading/trailing spaces
    filter(
      !(set %in% c('', '?', NA)), 
      !(is.na(as.integer(shelf))), 
      !(is.na(as.integer(rack))),
      !(is.na(as.integer(pullout))), 
      !(is.na(as.integer(insert))), 
      !(is.na(as.integer(row))), 
      !(is.na(as.integer(column)))
      )  # All valid strains must have a set value and a location number

  notsoclean <- 
    strains %>%
    filter(!(entry %in% cleanish$entry))
  
  duplicated <- 
    cleanish %>%
    filter(duplicated(location) | duplicated(location, fromLast = T))
  
  cleanish <- 
    cleanish %>%
    filter(!(entry %in% duplicated$entry))
  
  notsoclean <- select(notsoclean , -entry)
  cleanish <- select(cleanish , -entry)
  duplicated <- select(duplicated , -entry)

  addresses <- select(cleanish, strain_id, shelf, rack, pullout, insert, row, column)
  cleanish <- cleanish %>%
    select(-c(shelf,rack,pullout,insert,row,column)) %>%
    select(strain_id, everything())
    
    
  
  dir <- dirname(file)  
    
  write.csv(x = addresses, 
            file = paste0(dir, "/" , "addresses.csv"), 
            quote = F, 
            row.names = F)
  
  if (write){
  
    write.csv(x = notsoclean, 
              file = paste0(dir, "/" , gsub('[.].*', '', file), "-dirty.csv"), 
              quote = F, 
              row.names = F)
    
    write.csv(x = cleanish, 
      file = paste0(dir, "/" , gsub('[.].*', '', file), "-cleaned.csv"), 
      quote = F, 
      row.names = F)
    
    write.csv(
      x = duplicated,
      file = paste0(dir, "/" , gsub('[.].*', '', file), "-duplicates.csv"),
      quote = F,
      row.names = F)
  
    message("4 csv files have been created")
  } else { message("1 csv file has been created") }
  
  
  
  stock <- list(cleanish, notsoclean, duplicated)

  return(stock)
  
}