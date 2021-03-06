####SET UP####
#If you ever use this script on Fabian (desktop), make sure that default.stringsAsFactors() is FALSE

#Working directory
setwd("C:/Users/Bas1994/Documents/RA Sequeira/Media")

#Packages
library(rvest)
library(dplyr)
library(stringr)
library(writexl)

#Functions
#The function scraplinks builds a dataframe with in one column all the links that can be found on a webpage,
#and in a second column all the corresponding urls. 
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data.frame(link = link_, url = url_))
}

#Parameters
number_of_paragraphs_express <- 4
number_of_paragraphs_daily_mail <- 2 #larger paragraphs
number_of_paragraphs_evening_standard <- 4 #introduction is the first paragraph
number_of_paragraphs_times <- 2 #large variability in size of paragraphs
number_of_paragraphs_guardian <- 2
number_of_paragraphs_independent <- 3

#keywords
key_words <- c("brexit", "european union", "\\beu\\b", "climate change", "paris agreement", "environment", 
               "global warming", "unfccc", "british identity", "british passport", "british culture", 
               "british heritage", "british goods", "british products", "british manufacturing",
               "made in britain", "independence", "migration", "migrants", "foreigners","unemployment",
               "vassalage")

#anti keywords, used to quickly drop articles: if an article has one of these words in the title, 
#it is definitely not relevant. 
anti_key_words <- c("football", "arsenal", "chelsea", "everton", "liverpool", "leicester city", 
                    "manchester united", "manchester city", "tottenham", "mourinho", "man utd", 
                    "premier league", "fa cup")

###EXPRESS####
#create empty dataframe
df_express <- data.frame(headline=character(),
                 newspapers=character(), 
                 date=character(), 
                 author=character(),
                 page_number=character(),
                 text=character()) 

for (year in 2015:2017){
  if (year ==2015){
    months <- 3:12
  } else if (year == 2016){
    months <- 1:12
  } else {
    months <- 1:3 
  }
  for (month in months){
    if (month ==2 & year == 2016){
      days <- 29
    } else if (month == 2){
      days <- 28
    } else if (month %in% c(4, 6, 9, 11)){
      days <- 30
    } else {
      days <- 31
    }
    
    for (day in 1:days){
      #build the correct url:
      page <- paste0("https://www.express.co.uk/sitearchive/",year,"/",month,"/",day)
      
      #Navigate there:
      page_html <- read_html(page)
      headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="maincontainer"]/div[1]/div/section/ul/li[1]/a'))
      
      #Build a dataframe with links and urls:
      links <- scraplinks(page)
      #Remove irrelevant links and urls:
      links <- links[which(links$link==headline1):(which(links$link=="Find us on Facebook")[2]-1),]
      links <- links[substr(links$url,1,6)=="/news/",]
      
      for (i in 1:length(links$url)){
        #Navigate to an article, there is a tryCatch function around it, because sometimes the url is "broken",
        #which means that the url of the article is not the same as the url on the main page.  
        art_html <- tryCatch(read_html(paste0("https://www.express.co.uk/",links$url[i])), error = function(e){NA})
        
        if (!is.na(art_html)){
          #author
          author <- art_html %>%    
            rvest::html_nodes('body') %>% 
            xml2::xml_find_all("//div[contains(@class, 'author')]") %>% 
            rvest::html_text()
          author <- str_replace_all(author, " By " , "")
          if (length(author)==0){
            author <- NA
          }
          
          #introduction
          intro <- html_text(art_html %>% html_nodes("h3"))[1]
          
          #paragraphs
          #There are two options for the xpath (depends on whether there is a video on the page or not)
          xp1 <- '//*[@id="singleArticle"]/article/div[2]/div/div[2]/p[1]'
          xp2 <- '//*[@id="singleArticle"]/article/div[2]/div/div[3]/p[1]'
          
          if (identical(nchar(html_text(art_html %>% html_nodes(xpath=xp1))),integer(0))){
            xp_value <- 3
          } else if (identical(nchar(html_text(art_html %>% html_nodes(xpath=xp2))),integer(0))){ 
            xp_value <- 2
          } else if (nchar(html_text(art_html %>% html_nodes(xpath=xp1)))>nchar(html_text(art_html %>% html_nodes(xpath=xp2)))){
            xp_value <- 2
          } else {
            xp_value <- 3
          }
          
          parag <- character()
          for (j in 1:number_of_paragraphs_express){
            xp <- paste0('//*[@id="singleArticle"]/article/div[2]/div/div[',xp_value ,']/p[',j,']')
            parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
          }
          
          if (!identical(parag,character(0))){
            #Check whether the headline, the introduction or the paragraphs contain any of the keywords
            if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],intro,parag)))){
              #If that is the case, add the article to the dataset:
              row <- c(links$link[i],"Express", paste(day,month,year,sep="-"), author, NA, paste(intro, parag))
              df_express <- rbind(df_express,row)
              #print(links$link[i])
            }
          }
        }
      }
      #Some output to track progress
      print(paste(paste(day,month,year,sep="-"), "is done!"))
      print(paste("Number of articles is now",dim(df_express)[1]))
    }
  }
}
names(df_express) <- c("headline", "newspaper", "date", "author", "page", "text")

####DAILY MAIL####
#Similar to express, only the page_url is constructed differently.
df_dm <- data.frame(headline=character(),
                    newspapers=character(), 
                    date=character(), 
                    author=character(),
                    page_number=character(),
                    text=character()) 

for (year in 2015:2017){
  if (year ==2015){
    months <- 3:12
  } else if (year == 2016){
    months <- 1:12
  } else {
    months <- 1:3 
  }
  for (month in months){
    if (month ==2 & year == 2016){
      days <- 29
    } else if (month == 2){
      days <- 28
    } else if (month %in% c(4, 6, 9, 11)){
      days <- 30
    } else {
      days <- 31
    }
    
    for (day in 1:days){
      if (day < 10){
        day_str <- paste0(0,day)
      } else {
        day_str <- day
      }
      
      if (month < 10){
        month_str <- paste0(0,month)
      } else {
        month_str <- month
      }
      
      page <- paste0("https://www.dailymail.co.uk/home/sitemaparchive/day_",year,month_str,day_str,".html")
      
      page_html <- read_html(page)
      headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="content"]/div[1]/div[1]/ul[2]/li[1]/a'))
      
      links <- scraplinks(page)
      links <- links[which(links$link==headline1):(which(links$link=="\n      Like\n      DailyMail\n    ")-2),]
      links <- links[substr(links$url,1,6)=="/news/",]
      
      for (i in 1:length(links$url)){
        art_html <- tryCatch(read_html(paste0("https://www.dailymail.co.uk",links$url[i])), error = function(e){NA})
        
        if (!is.na(art_html)){
          author <- art_html %>%    
            rvest::html_nodes('body') %>% 
            xml2::xml_find_all("//p[contains(@class, 'author-section byline-plain')]") %>% 
            rvest::html_text()
          author <- str_replace_all(author, "By " , "")
          if (length(author)==0){
            author <- NA
          }
          
          intro <- html_text(art_html %>% html_nodes(xpath='//*[@id="js-article-text"]/ul'))
          
          parag <- character()
          for (j in 1:number_of_paragraphs_daily_mail){
            xp <- paste0('//*[@id="js-article-text"]/div[2]/p[',j,']')
            parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
          }
          
          if (!identical(parag,character(0))){
            if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],intro,parag)))){
              row <- c(links$link[i],"Daily Mail", paste(day,month,year,sep="-"), author, NA, paste(intro, parag))
              df_dm <- rbind(df_dm,row)
              #print(links$link[i])
            }
          }
        }
      }
      print(paste(paste(day,month,year,sep="-"), "is done!"))
      print(paste("Number of articles is now",dim(df_dm)[1]))
    }
  }
}
names(df_dm) <- c("headline", "newspaper", "date", "author", "page", "text")

####EVENING STANDARD####
#This code has to be run in several rounds, because after some time you can't access the site any more.
#You should then wait for a bit and then continue running the script with the following piece of 
#code implemented (for example):
#
#day_start <- 1
#if (month == 3 & year == 2016){
#  day_start <- 10
#}
#
#for (day in day_start:days){

df_ev_stan <- data.frame(headline=character(),
                         newspapers=character(), 
                         date=character(), 
                         author=character(),
                         page_number=character(),
                         text=character()) 

for (year in 2015:2017){
  if (year ==2015){
    months <- 3:12
  } else if (year == 2016){
    months <- 1:12
  } else {
    months <- 1:3 
  }
  for (month in months){
    if (month ==2 & year == 2016){
      days <- 29
    } else if (month == 2){
      days <- 28
    } else if (month %in% c(4, 6, 9, 11)){
      days <- 30
    } else {
      days <- 31
    }
    
    for (day in 1:days){
      page <- paste0("https://www.standard.co.uk/archive/",year,"-",month,"-",day)
      
      page_html <- read_html(page)
      headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="body"]/section/ol/li[1]/h2/a'))
      
      links <- scraplinks(page)
      links <- links[which(links$link==headline1):((which(links$link==" 1")[1])-1),]
      links <- links[substr(links$url,1,6)=="/news/",]
      
      for (i in 1:length(links$url)){
        art_html <- tryCatch(read_html(paste0("https://www.standard.co.uk",links$url[i])), error = function(e){NA})
        
        if (!is.na(art_html)){
          author <- art_html %>%    
            rvest::html_nodes('body') %>% 
            xml2::xml_find_all("//li[contains(@class, 'author author-last')]") %>% 
            rvest::html_text()
          author <- str_replace_all(author, "\n" , "")
          if (length(author)==0){
            author <- NA
          }
          
          #There is no introduction
          
          parag <- character()
          xp_value<-3
          for (j in 1:number_of_paragraphs_evening_standard){
            xp <- paste0('//*[@id="main"]/div/div/p[',j,']')
            parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
          }
          
          if (!identical(parag,character(0))){
            if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],parag)))){
              row <- c(links$link[i],"Evening Standard", paste(day,month,year,sep="-"), author, NA, parag)
              df_ev_stan <- rbind(df_ev_stan,row)
              #print(links$link[i])
            }
          }
        }
      }
      print(paste(paste(day,month,year,sep="-"), "is done!"))
      print(paste("Number of articles is now",dim(df_ev_stan)[1]))
    }
  }
}
names(df_ev_stan) <- c("headline", "newspaper", "date", "author", "page", "text")

####THE TIMES####
df_times <- data.frame(headline=character(),
                    newspapers=character(), 
                    date=character(), 
                    author=character(),
                    page_number=character(),
                    text=character()) 

for (year in 2015:2017){
  if (year ==2015){
    months <- 3:12
  } else if (year == 2016){
    months <- 1:12
  } else {
    months <- 1:3 
  }
  for (month in months){
    for (week in 1:4){
      if (month <= 10){
        month_str <- paste0(0,month)
      } else {
        month_str <- month
      }
      page <- paste0("https://www.thetimes.co.uk/html-sitemap/",year,"-",month_str,"-",week)
      
      page_html <- read_html(page)
      headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="top"]/section/section/div[2]/ul/li[1]'))
      
      links <- scraplinks(page)
      links <- links[which(links$link==headline1):(which(links$link=="Back to top")-1),]
      
      #filter out headlines that have "anti-headlines":
      links <- links[!(grepl(paste(anti_key_words, collapse = "|"), tolower(links$link))),]
      
      for (i in 1:length(links$url)){
        art_html <- tryCatch(read_html(paste0("https://www.thetimes.co.uk",links$url[i])), error = function(e){NA})
        
        if (!is.na(art_html)){
          author <- html_text(art_html %>% html_nodes(xpath='//*[@id="article-main"]/main/div/div[1]/div[2]/div[1]/div/div/span/span'))
            if (length(author)==0){
              author <- NA
            } else {
              author <- author[1]
            }
          
          intro <- html_text(art_html %>% html_nodes("h2"))[3]
          if (is.na(intro)){
            intro<-""
          }
          
          #Sometimes the first two paragraphs are empty on this website. The following code block accounts for this:
          first <- 1
          
          if(!identical(nchar(html_text(art_html %>% html_nodes(xpath='//*[@id="article-main"]/main/article/p[1]'))),integer(0)) & 
             !identical(nchar(html_text(art_html %>% html_nodes(xpath='//*[@id="article-main"]/main/article/p[2]'))),integer(0))){
            if (first == 1){
              if (nchar(html_text(art_html %>% html_nodes(xpath='//*[@id="article-main"]/main/article/p[1]')))<2){
                first <- 2
                if (nchar(html_text(art_html %>% html_nodes(xpath='//*[@id="article-main"]/main/article/p[2]')))<2){
                  first <- 3
                }
              } 
            }
            
            parag <- character()
            for (j in first:(first+number_of_paragraphs_times)){
              xp <- paste0('//*[@id="article-main"]/main/article/p[',j,']')
              parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
            }
            
            date<- html_text(art_html %>% html_nodes(xpath='//*[@id="article-main"]/main/div/div[1]/div[2]/div[2]/div/div/span/time'))
            day <- tryCatch(as.integer(str_split(date," ")[[1]][3]),error = function(e){NA})
            
            if ((!identical(parag,character(0))) & is.integer(day)){
              if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],intro,parag)))){
                row <- c(links$link[i],"The Times", paste(day,month,year,sep="-"), author, NA, paste(intro, parag))
                df_times <- rbind(df_times,row)
                #print(links$link[i])
              }
            }
          }
        }
      }
      print(paste(paste(week,month,year,"(in weeks)",sep="-"), "is done!"))
      print(paste("Number of articles is now",dim(df_times)[1]))
    }
  }
}
names(df_times) <- c("headline", "newspaper", "date", "author", "page", "text")

####THE GUARDIAN####
#The guardian newspaper is different on Sundays, and the articles hence also look different on Sundays.
df_guardian <- data.frame(headline=character(),
                          newspapers=character(), 
                          date=character(), 
                          author=character(),
                          page_number=character(),
                          text=character()) 

months_abbr <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

for (year in 2015:2017){
  if (year ==2015){
    months <- 3:12
  } else if (year == 2016){
    months <- 1:12
  } else {
    months <- 1:3 
  }
  for (month in months){
    if (month ==2 & year == 2016){
      days <- 29
    } else if (month == 2){
      days <- 28
    } else if (month %in% c(4, 6, 9, 11)){
      days <- 30
    } else {
      days <- 31
    }
    
    month_str <- months_abbr[month]
    
    for (day in 1:days){
      if (day < 10){
        day_str <- paste0(0,day)
      } else {
        day_str <- day
      }
      
      #Check whether this day is a Sunday. "zondag" is the Dutch word for Sunday, you should probably change that
      #to "sunday" on an English PC. 
      if (weekdays(as.Date(paste(year,month,day,sep="-")))=="zondag"){
        page <- paste0("https://www.theguardian.com/theobserver/",year,"/",month_str,"/",day_str)
        page_html <- read_html(page)
        headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="front-page"]/div/div/div/ul/li[1]/div/div/a'))
        
        links <- scraplinks(page)
        
        #Sometimes there are no front page articles on the website, then another way to shrink the links 
        #dataframe has to be used:
        if (identical(headline1,character(0))){
          lb <- which(links$link == "\nNews\n")[1]+1
        }else{
          lb <- which(links$link==headline1)
        }
        
        links <- links[lb:(which(links$link=="\nNews\n")[2]-1),]
        #Trim the white space in the link column:
        links$link <- trimws(links$link)
        #Remove duplicates:
        links<-links[!duplicated(links$link),]
        #Remove "short" links to get rid of the header links
        links<-links[sapply(strsplit(links$link, " "), length)>3,]
        for (i in 1:length(links$url)){
          art_html <- tryCatch(read_html(links$url[i]), error = function(e){NA})
          
          if (!is.na(art_html)){
            author<-html_text(art_html %>% html_nodes(xpath='/html/body/section[1]/div/div/div[7]/div/div/div/div[1]/div/address/div'))
            if (length(author)==0){
              author <- NA
            }
            author <- author[1]
            
            intro<-html_text(art_html %>% html_nodes(xpath='/html/body/section[1]/div/div/div[4]/div/div/text()'))
            
            parag <- character()
            for (j in 1:number_of_paragraphs_guardian){
              xp <- paste0('/html/body/section[1]/div/div/div[8]/main/main/div[1]/div/p[',j,']')
              parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
            }
            
            if (!identical(parag,character(0))){
              if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],intro,parag)))){
                #This is the only newspaper that has some information about whether an article was on the front 
                #page or not. In the links dataframe this can be seen by checking whether it was in the top 3.
                if(i %in% 1:3){
                  row <- c(links$link[i],"The Guardian", paste(day,month,year,sep="-"), author, 1, paste(intro, parag))
                  df_guardian <- rbind(df_guardian,row)
                  print(links$link[i])
                }else{
                  row <- c(links$link[i],"The Guardian", paste(day,month,year,sep="-"), author, NA, paste(intro, parag))
                  df_guardian <- rbind(df_guardian,row)
                  print(links$link[i])
                }
              }
            }
          }
        }
        print(paste(paste(day,month,year,sep="-"), "is done!"))
      } else {
        page <- paste0("https://www.theguardian.com/theguardian/",year,"/",month_str,"/",day_str)
        page_html <- read_html(page)
        headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="front-page"]/div/div/div/ul/li[1]/div/div/a'))
        headline2 <- html_text(page_html %>% html_nodes(xpath='//*[@id="front-page"]/div/div/div/ul/li[2]/div/div/a'))
        
        #Sometimes there are no front page articles on the website, then another way to shrink the links 
        #dataframe has to be used (find a lowerbound (lb) and upperbound (ub)):
        links <- scraplinks(page)
        if (identical(headline1,character(0)) & identical(headline2,character(0))){
          lb <- which(links$link == " Top stories ")+1
        }else if(!identical(headline1,character(0)) & identical(headline2,character(0))){
          lb <- which(links$link==headline1)
        }else if(identical(headline1,character(0)) & !identical(headline2,character(0))){
          lb <- which(links$link==headline2)
        }else{
          lb <- min(which(links$link==headline1), which(links$link==headline2))
        }
        
        if (identical(which(links$link==" Editorials & reply "),integer(0))){
          ub <- which(links$link=="\nToday's paper\n")[3]-1
        }else{
          ub <- which(links$link==" Editorials & reply ")-1
        }
        
        links <- links[lb:ub,]
        
        links$link <- trimws(links$link)
        links<-links[!duplicated(links$link),]
        links<-links[sapply(strsplit(links$link, " "), length)>3,]
        for (i in 1:length(links$url)){
          art_html <- tryCatch(read_html(links$url[i]), error = function(e){NA})
          
          if (!is.na(art_html)){
            author<-html_text(art_html %>% html_nodes(xpath='/html/body/section[1]/div/div/div[7]/div/div/div/div[1]/div/address/div'))
            if (length(author)==0){
              author <- NA
            }
            author <- author[1]
            
            intro<-html_text(art_html %>% html_nodes(xpath='/html/body/section[1]/div/div/div[4]/div/div/text()'))
            
            parag <- character()
            for (j in 1:number_of_paragraphs_guardian){
              xp <- paste0('/html/body/section[1]/div/div/div[8]/main/main/div[1]/div/p[',j,']')
              parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
            }
            
            if (!identical(parag,character(0))){
              if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],intro,parag)))){
                if(i %in% 1:3){
                  row <- c(links$link[i],"The Guardian", paste(day,month,year,sep="-"), author, 1, paste(intro, parag))
                  df_guardian <- rbind(df_guardian,row)
                  print(links$link[i])
                }else{
                  row <- c(links$link[i],"The Guardian", paste(day,month,year,sep="-"), author, NA, paste(intro, parag))
                  df_guardian <- rbind(df_guardian,row)
                  print(links$link[i])
                }
              }
            }
          }
        }
        print(paste(paste(day,month,year,sep="-"), "is done!"))
      }
    }
  }
}
names(df_guardian) <- c("headline", "newspaper", "date", "author", "page", "text")

####THE INDEPENDENT####
df_indep <- data.frame(headline=character(),
                       newspapers=character(), 
                       date=character(), 
                       author=character(),
                       page_number=character(),
                       text=character()) 

for (year in 2015:2017){
  if (year ==2015){
    months <- 3:12
  } else if (year == 2016){
    months <- 1:12
  } else {
    months <- 1:3 
  }
  for (month in months){
    if (month ==2 & year == 2016){
      days <- 29
    } else if (month == 2){
      days <- 28
    } else if (month %in% c(4, 6, 9, 11)){
      days <- 30
    } else {
      days <- 31
    }
    
    for (day in 1:days){
      page <- paste0("https://www.independent.co.uk/archive/",year,"-",month,"-",day)
      
      page_html <- read_html(page)
      headline1 <- html_text(page_html %>% html_nodes(xpath='//*[@id="body"]/div[1]/section/ol/li[1]/h2/a'))
      
      links <- scraplinks(page)
      links <- links[which(links$link==headline1):length(links$link),]
      links <- links[substr(links$url,1,6)=="/news/",]
      
      for (i in 1:length(links$url)){
        
        art_html <- tryCatch(read_html(paste0("https://www.independent.co.uk",links$url[i])), error = function(e){NA})
        
        if (!is.na(art_html)){
          author <- html_text(art_html %>% html_nodes(xpath='//*[@id="top-container-wrapper"]/div/div/div[1]/ul/li[1]/a'))
          if (length(author)==0){
            author <- NA
          }
          
          #intro <- html_text(art_html %>% html_nodes("h3"))[1]
          
          parag <- character()
          xp_value<-3
          for (j in 1:number_of_paragraphs_independent){
            xp <- paste0('//*[@id="main"]/div/div[1]/p[',j,']')
            parag <- paste(parag, html_text(art_html %>% html_nodes(xpath=xp)))
          }
          
          if (!identical(parag,character(0))){
            if (grepl(paste(key_words, collapse = "|"), tolower(paste(links$link[i],parag)))){
              row <- c(links$link[i],"The Independent", paste(day,month,year,sep="-"), author, NA, parag)
              df_indep <- rbind(df_indep,row)
              print(links$link[i])
            }
          }
        }
      }
      print(paste(paste(day,month,year,sep="-"), "is done!"))
    }
  }
}
names(df_indep) <- c("headline", "newspaper", "date", "author", "page", "text")


####EXPORTING & GRAPHS####
write_xlsx(df_express, 'express_after_filters.xlsx')
write_xlsx(df_dm, 'daily_mail_after_filters.xlsx')
write_xlsx(df_ev_stan, 'evening_standard_after_filters.xlsx')
write_xlsx(df_times, 'times_after_filters.xlsx')
write_xlsx(df_guardian, 'guardian_after_filters.xlsx')
write_xlsx(df_indep, 'independent_after_filters.xlsx')

#Some code that produces the number-of-articles-per-day-graph
a <- as.data.frame(table(df_ev_stan$date))
a$Var1 <- as.Date(a$Var1,"%d-%m-%Y")
a<-a[order(a$Var1),]
plot(a,type="l")
