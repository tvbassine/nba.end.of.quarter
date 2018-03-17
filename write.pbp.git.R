########################################################
# 3/17/18
# Goal: Create a scraper of bball reference play-by-play
# data which I can be confident in.
# Next, I want to identify the beginning and end of possessions,
# what types of shots were taken, and how many points were scored
# on the possession.

#########################################################
# PART 1
# Want to build a function which takes in year and 
# reads all play-by-play data for that year (then 
# writes it as a csv to a folder)

write.pbp <- function(year){
  
  games <- list()
  count <- 1
  
  for(month in c("","-november","-december",
                 "-january", "-february",
                 "-march")){ #put all the months here
    
    file <- paste("https://www.basketball-reference.com/leagues/NBA_", year ,"_games",
                  month,".html", sep = "")
    jj <- scan(file, what="", sep="\n")
    
    if(month %in% c("","-november","-december")){yr.id <- year - 1}
    else{yr.id <- year}
    
    marker <- paste("/boxscores/", yr.id, sep = "")
    
    jj <- jj[grep(marker,jj)] 
    
    get.url <- function(line){
      temp <- unlist(strsplit(line, split = "data-stat=\"box_score_text"))[2]
      temp <- unlist(strsplit(temp, split = "\">Box Score"))[1]
      temp <- unlist(strsplit(temp, split = "href=\""))[2]
      temp <- unlist(strsplit(temp, split = "boxscores"))[2]
      out <- paste("https://www.basketball-reference.com/boxscores/pbp", temp , sep = "")
      return(out)
    }
    
    test <- lapply(jj,get.url)
    
    
    get.pbp <- function(file){
      jj <- scan(file, what="", sep="\n")
      
      get.tms <- jj[intersect(grep("vs.",jj),grep("Play-By-Play",jj))[1]]
      get.tms <- unlist(strsplit(get.tms,"Play-By-Play -"))[2]
      get.tms <- unlist(strsplit(get.tms,"-"))[1]
      get.tms <- unlist(strsplit(get.tms,"vs."))
      get.tms <- unlist(strsplit(get.tms,"\\("))[c(1,3)]
      get.tms <- trimws(get.tms)
      
      ind.1 <- grep("Start of 1st quarter",jj)
      ind.2 <- grep("</table",jj)
      ind.2 <- min(ind.2[ind.2 > ind.1])
      jj <- jj[ind.1:ind.2]
      
      ind.plays <- grep("&nbsp",jj)
      ind.time <- ind.plays - 1
      
      
      # want to store actions and times
      game.pbp <- data.frame(play = jj[ind.plays], time = jj[ind.time],
                             away = get.tms[1], home = get.tms[2],
                             stringsAsFactors = F)
      game.pbp$total.plays <- nrow(game.pbp)
      return(game.pbp)
      
    }
    
    
    
    for(file in test){
      games[[count]] <- get.pbp(file)
      games[[count]]$id <- count
      count <- count + 1
      print(count)
      print(year)
    }
    
  }
  
  games.dat <- do.call("rbind",games)
  head(games.dat)
  tail(games.dat)
  
  setwd("/Users/thomasbassine/Desktop/My sports projects/Play by Play 2017-2018")
  
  file.nm <- paste("games.", year, ".csv", sep = "")
  
  write.csv(games.dat, file.nm, row.names = F)
  
}
