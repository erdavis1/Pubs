
library(tidyverse)
library(httr)
library(jsonlite)
library(ggmap)
library(osmdata)
library(sqldf)
library(googleway)


#---------------function to get UK health data----------------
#-----------from here: https://www.trafforddatalab.io/open_data_companion/----------
get_health <- function(BusinessTypeId, Name, pageNumber) {
  path <- "http://api.ratings.food.gov.uk/Establishments"
  if (Name=="") {
    request <- GET(url = path,
                   query = list(
                     BusinessTypeId = BusinessTypeId,
                     pageNumber = pageNumber,
                     pageSize = 5000),
                   add_headers("x-api-version" = "2"))
  } else {
    request <- GET(url = path,
                   query = list(
                     BusinessTypeId = BusinessTypeId,
                     Name = Name,
                     pageNumber = pageNumber,
                     pageSize = 5000),
                   add_headers("x-api-version" = "2"))
  }
  
  
  response <- content(request, as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE) %>% 
    pluck("establishments") %>% 
    as_tibble()
  
  # tidy the data
  df <- response %>% 
    mutate_all(funs(replace(., . == '', NA))) %>% 
    select(name = BusinessName,
           type = BusinessType,
           rating = RatingValue,
           address1 = AddressLine1,
           address2 = AddressLine2,
           address3 = AddressLine3,
           address4 = AddressLine4,
           postcode = PostCode,
           long = geocode.longitude,
           lat = geocode.latitude) %>% 
    unite(address, address1, address2, address3, address4, remove = TRUE, sep = ", ") %>% 
    mutate(address = str_replace_all(address, "NA,", ""),
           address = str_replace_all(address, ", NA", ""),
           long = as.numeric(long),
           lat = as.numeric(lat))
  
  return(df)
  
}


#----------------uk---------------
#first, get all pub/bar/nightclub
uk <- NULL
for (i in 1:11) {
  df <- get_health(7843, "", i)
  uk <- rbind(uk, df)
}

#then, get restaurants with bar or pub in the name. Some pubs are coded as restaurants, unfortunately.
df_pub <- get_health(1, "pub", 1)
df_bar <- get_health(1, "bar", 1)

#exclude things that don't seem like bars. 
exclude <- c('burger', 'coffee', 'bagel', 'espresso', 'chocolat', 'chocolate', 'noodle', 'snack', 'meat', 'tea', 'fish',
              'milk', 'sandwich', 'brunch', 'burrito', 'chicken', 'pasta', 'juice', 'cafe', 'deli', 'energy', 'salad',
             'salsa', 'shake', 'snax', 'Hummus', 'oyster', 'breakfast', 'Bar B Q', 'tapas', 'Caffe', 'Baguette',
             'Cappuccino', 'cake', 'brownie', 'dessert', 'cupcake', 'Expresso', 'pizza', 'Food Bar', 'cream', 'Mozzarella',
             'meze', 'sushi', 'Sundae', 'garden bar')
df_bar <- df_bar[!grepl(paste(exclude, collapse="|"), df_bar$name, ignore.case=TRUE),]
uk <- rbind(uk, df_pub)
uk <- rbind(uk, df_bar)

#get regions by post code
scotland <- c("AB", "DD", "KW", "DG", "KY", "EH", "ML", "FK", "PA", "G", "PH", "TD", "IV", "ZE", "KA", "HS")
northireland <- c("BT")
wales <- c("CF", "NP", "LD", "SY", "LL", "SA")

uk$postcode <- gsub("\\d", "", substr(uk$postcode, 1, 2))    
uk$region <- "england"
uk$region[uk$postcode %in% scotland] <- "scotland"
uk$region[uk$postcode %in% northireland] <- "northireland"
uk$region[uk$postcode %in% wales] <- "wales"


#--------------ireland--------------
pubsir <- read.csv("https://www.revenue.ie/en/corporate/documents/statistics/excise/liquor-licences.csv")
pubsir <- pubsir[grep("^Publican's Licence \\([6-7]", pubsir$description), ] 
pubsir <- subset(pubsir, !is.na(trading_name))
pubsir$geocodename <- gsub(" ", "+",paste0(pubsir$trading_name, ",+", pubsir$address_1, ",+", pubsir$address_2, "+,Ireland"))

colnames(pubsir)[1] <- "ID"

#geocode the pubs
ire <- NULL
key <- "my google api key here"
register_google(key = key)


for (i in 1:nrow(pubsir)){
  geo <- geocode(pubsir$geocodename[i])
  temp <- data.frame(pubsir$ID[i], pubsir$trading_name[i] , geo$lat, geo$lon)
  ire <- rbind(temp, ire)
  
  if (i%%20==0) {
    Sys.sleep(1)
    print(i) #sleep a bit so Google doesn't cut you off
  }
}

colnames(ire) <- c("ID", "name", "lat", "long")
ire$region <- "ireland"

#get rid of cruddy geocodes
ire <- subset(ire, long > -9.978)
ire <- subset(ire, long < -6.033)
ire <- subset(ire, lat < 55.132)
ire <- subset(ire, lat > 51.670)

#merge uk and ireland
t1 <-data.frame(uk$name,uk$long,uk$lat, uk$region)
t2 <- data.frame(ire$name,ire$long,ire$lat, ire$region)

colnames(t1) <- c("name", "long", "lat", "region")
colnames(t2) <- c("name", "long", "lat", "region")
allpubs <- rbind(t1, t2)

#remove missing geocodes
allpubs <- subset(allpubs, !(is.na(lat)))

#add an id
allpubs$ID <- seq.int(nrow(allpubs))


#---------------selective google maps queries-------------
#---these spots seemed a bit blank on the map so I double checked them on Google
key <- "my google distinaces api key here"
check <- data.frame(x = c(-6.26,-7.353, -4.7,-6.146, -6.92, -4.78, -4.625, -4.375, -7,-6.84, -4.21, -3.07,-6.87, -4.63, -4.53)
            , y = c(57.335, 57.264, 58.297,55.823,56.493,56.496, 56.625, 58.1875, 58, 57.97, 58.234, 56.818, 57.867, 56.206, 54.22))


results <- NULL
for (i in 1:nrow(check)) {
  x <- check$x[i]
  y <- check$y[i]
  
  temp <- google_places(location = c(y, x), radius = 30000, place_type = 'bar', key = key)
  results <- data.frame(temp$results$name, temp$results$geometry$location$lat, temp$results$geometry$location$lng) %>% rbind(results)
  Sys.sleep(1)
  
  while ( !(is.null(temp$next_page_token))) {
    Sys.sleep(1)
    nextpage <- temp$next_page_token
    temp <- google_places(location = c(y, x), radius = 30000, place_type = 'bar', key = key, page_token = nextpage)
    results <- data.frame(temp$results$name, temp$results$geometry$location$lat, temp$results$geometry$location$lng) %>% rbind(results)
  }
  
  Sys.sleep(5)
}

##!!!!####
#I merged the other results with these new Google results by hand. Not all the results were actually pub, and some were already in the dataset.



#----final----
write.csv(allpubs, "ALLPUBS.csv", row.names =FALSE)







