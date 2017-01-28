library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)

#CONSTANTS
BASE_URL <- 'https://nips.cc/Conferences/2016/Schedule'
CSS_SELECTOR_CONTAINERS <- '.narrower'
CSS_SELECTOR_NAME_TALK <- '.maincardBody'
CSS_SELECTOR_AUTHORS <- '.maincardFooter'
CSS_SELECTOR_TYPE <- '.pull-right.maincardHeader.maincardType'
CSS_SELECTOR_MEDIA <- '.btn-xs' 

#####################################
## MAIN - SCRAPING TALK CONTAINERS ##
#####################################

response <- GET(BASE_URL)

if(response$status_code != 200){stop('broken link')}
  
data_raw <- content(response, as = 'text')
containers <- read_html(data_raw) %>% html_nodes(CSS_SELECTOR_CONTAINERS)

url_talk_description <- paste0(BASE_URL, '?showEvent=', containers %>% html_attr('id') %>% str_extract('[0-9]+$'))
name_talk <- containers %>% html_nodes(CSS_SELECTOR_NAME_TALK) %>% html_text(trim=TRUE)
authors <- containers %>% sapply(., function(node_author){node_author %>% html_node(CSS_SELECTOR_AUTHORS) %>% html_text(trim=TRUE)})
type_talk <- sapply(containers, function(node_container){html_node(node_container, CSS_SELECTOR_TYPE) %>% html_text(trim=TRUE)})
media <- bind_rows(mapply(as.character(1:length(containers)), containers, FUN = function(id_talk, node_container){
  
      node_media <- data.frame(id_talk, rbind(node_container %>% 
                                 html_nodes(CSS_SELECTOR_MEDIA) %>% 
                                 html_attr('href') %>% 
                                 (function(media_urls){if(length(media_urls) == 0){return(list())} else return(media_urls)})), stringsAsFactors = FALSE)
      
      colnames(node_media) <- c('id_talk', node_container %>% html_nodes(CSS_SELECTOR_MEDIA) %>% html_attr('title') %>% str_trim())
      return(node_media)}, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  
nips_data <- data.frame(id_talk = as.character(1:length(containers)), type_talk = type_talk, name_talk = name_talk, authors = authors, url_talk_description = url_talk_description, stringsAsFactors = FALSE)
nips_data <- left_join(nips_data, media, by = 'id_talk')
nips_data <- nips_data %>% select(id_talk:Video, Video_1=`Video 1`, Video_2=`Video 2`, Slides, PDF, Notes, Spotlight_Video=`Spotlight Video`)

###################
## CLEANING DATA ##
###################

#Getting rid of breaks, tapas ... there is no author on those fields
nips_data <- filter(nips_data, authors != '') %>% select(-id_talk)

#######################
## DIRECT LINKS PDFS ##
#######################

nips_data$PDF <- sapply(nips_data$PDF, function(url_pdf){
  ifelse(!is.na(url_pdf), paste0(GET(url_pdf)$url, '.pdf'), NA)
})

###################################################
## LINKS TO DOWNLOAD VIDEOS IN CHANNEL 9 WEBSITE ##
###################################################

get_link_video <- function(video_link){
  
  if(is.na(video_link)){return('')}
  
  #NOTE -> returns the high quality video in the Channel 9 video quality selector
  video_url <- read_html(video_link) %>%
    html_node('#format') %>%
    html_nodes('option') %>%
    html_attr('value') %>%
    str_extract('.*_high.mp4') %>%
    .[!is.na(.)]
  
  #Returns empty string if the link doesn't exist
  if(length(video_url) == 0){return('')}
  
  #Return value
  return(video_url)
  
}

nips_data$Video <- sapply(nips_data$Video, get_link_video)
nips_data$Video_1 <- sapply(nips_data$Video_1, get_link_video) 
nips_data$Video_2 <- sapply(nips_data$Video_2, get_link_video)   

######
  
#NA -> empty string
nips_data[is.na(nips_data)] <- ''

#Order columns
nips_data <- select(nips_data, type_talk:url_talk_description, PDF, Video:Slides, Spotlight_Video, Notes)

#Saving scraping
write.table(nips_data, file = 'nips_data_2016.csv', sep = ';', row.names = FALSE, quote = FALSE)


