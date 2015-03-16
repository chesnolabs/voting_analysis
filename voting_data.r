loadMP <- function (fl) 
{
	x <- read.csv(fl, nrows = 5)
	classes <- sapply(x,class)
	read.csv(fl, colClasses = classes)
}

loadFactions <- function(fl)
{
  classes = rep("numeric",7)
	read.csv(fl, colClasses = classes, comment.char = "\"", quote = "")
}

loadVotings <- function(fl)
{
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y"))   
  classes <- c ("numeric","myDate","character","character")
	read.table(fl, colClasses = classes, sep = "\t", header = TRUE, quote = "")
}

exclude_blank_votings <- function()
{
  exclude <- votings$voting_ID[votings$title == '""']
  votings <<- votings[!(votings$voting_ID %in% exclude), ]
  fdf <<- fdf[!(fdf$voting_ID %in% exclude), ]
  mpdf <<- mpdf[!(mpdf$voting_ID %in% exclude), ]
  vottab <<- vottab[!(vottab$voting_ID %in% exclude), ]
}

findIDs <- function (MPids = "", factionIDS = "")
{
	x <- read.csv(MPids, nrows = 5, header = FALSE)
	classes <- sapply(x,class)
	mpids <<- read.csv(MPids, colClasses = classes, header = FALSE)
  names(mpids) <<- c("Deputy_name","MP_ID")
	x <- read.csv(factionIDS, nrows = 5)
	classes <- sapply(x,class)
	factids <<- read.csv(factionIDS, colClasses = classes)
}

votan.init <- function (filenames = 'filenames.csv')
{
	x <- read.table(filenames, nrows = 6, colClasses = c("character"))
	findIDs (x[[1,1]], x[[2,1]])
	mpdf <<- loadMP (x[[3,1]])
	fdf <<- loadFactions (x[[4,1]])
	votings <<- loadVotings (x[[5,1]])
#	addFactionHistory (x[[6,1]])
  print("Setting voting types...")
	votings <<- set_voting_types(votings)
  print("Setting faction support...")
	fdf <<- set_faction_support(votings, fdf)
  print("Creating main table vottab...")
  vottab <<- create_voting_tables(votings, fdf, mpdf)
  print("Calculating results of the votings...")
  votings$result <<- addResults(vottab)
  vottab$result <<- votings$result
  print("Excluding blank votings...")
  exclude_blank_votings()
  print("Saving...")
  dir.create("votan/")
  save(mpdf, file = "votan/mpdf.Rda")
  save(votings, file = "votan/votings.Rda")
  save(fdf, file = "votan/fdf.Rda")
  save(factids, file = "votan/factids.Rda")
  save(mpids, file = "votan/mpids.Rda")
  save(vottab, file = "votan/vottab.Rda")
}

append <- function(filenames = 'filenames.csv')
{
  x <- read.table(filenames, nrows = 6, colClasses = c("character"))
  findIDs (x[[1,1]], x[[2,1]])
  new_mpdf <- loadMP (x[[3,1]])
  new_fdf <- loadFactions (x[[4,1]])
  new_votings <- loadVotings (x[[5,1]])
  new_votings <- set_voting_types(new_votings)
  new_fdf <- set_faction_support(new_votings,new_fdf)
  new_vottab <- create_voting_tables(new_votings, new_fdf, new_mpdf)
  new_votings$result <- addResults(new_vottab)
  new_vottab$result <- new_votings$result
  loadData()
  votings <<- rbind(votings, new_votings)
  mpdf <<- rbind(mpdf,new_mpdf)
  fdf <<- rbind(fdf, new_fdf)
  vottab <<- rbind(vottab, new_vottab)
  exclude_blank_votings()
  save(mpdf, file = "votan/mpdf.Rda")
  save(votings, file = "votan/votings.Rda")
  save(fdf, file = "votan/fdf.Rda")
  save(factids, file = "votan/factids.Rda")
  save(mpids, file = "votan/mpids.Rda")
  save(vottab, file = "votan/vottab.Rda")
}

set_voting_types <- function (votings, first = NULL, last = NULL)
{
	if (is.null(first)) first <- min(votings$voting_ID)
	if (is.null(last)) last <- max(votings$voting_ID)
	if (is.null(votings$type))
	{
		type <- rep (0, length.out = length(votings$voting_ID))
		votings$type <- type
	}
  votings <- fn$sqldf(c("update votings set type = 1 where like('%проект Закону%',title) and like('%за основу%',title)", "select * from main.votings"))
	votings <- fn$sqldf(c("update votings set type = 2 where like('%проект Закону%',title) and like('%в цілому%',title)",  "select * from main.votings"))
  votings <- fn$sqldf(c("update votings set type = 3 where like('%поправ%',title) and like ('%проекту Закону%',title)",  "select * from main.votings"))
	votings <- fn$sqldf(c("update votings set type = 4 where like ('%порядку денного%',title) and like ('%проект%',title) and like('%закон%',title) and (like('%розгляд%',title) = 0) ",  "select * from  main.votings"))
  votings
}

set_faction_support  <- function(votings, fdf, first = NULL, last = NULL, light = FALSE)
{ 
  if (is.null(first)) first <- min(votings$voting_ID)
  if (is.null(last)) last <- max(votings$voting_ID)
  if (is.null(fdf$support))
  {
    support <- rep (0, length.out = length(fdf$voting_ID))
    fdf$support <- support
  }  
  fdf <- fn$sqldf(c("update fdf set support = 0 where (voting_ID >= $first) and (voting_ID <= $last)","select * from main.fdf"))
  if (light) 
    fn$sqldf(c("update fdf set support = 1 where ([for.] > abstain + against + did_not_vote) and (voting_ID >= $first) and (voting_ID <= $last) ","select * from main.fdf"))
  else fn$sqldf(c("update fdf set support = 1 where ([for.] > abstain + against + did_not_vote + absent) and (voting_ID >= $first) and (voting_ID <= $last)","select * from main.fdf")) 
  
}

addFactionHistory <- function(fl)
{
  transformDates <- function(from)
  {
    date.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = "Date")
    ret <- numeric()
    class(ret) = "Date"
    for (i in 1:length(from)) ret <- c(ret,date.ifelse(from[i] == "теперішній час",as.Date(NA),as.Date(from[i], format="%Y-%m-%d")))
    ret
  }
  setClass('myDate')
  setAs("character","myDate", transformDates) 
  classes <- c("numeric","character","character","character","myDate","myDate")
  fhistory <- read.csv(fl, colClasses = classes)
  names(fhistory) <- c("MP_ID","deputy","current_faction","faction","in_date","out_date")
  fhistory <- fn$sqldf("select MP_ID, ID,  in_date,out_date from fhistory, factids where faction_title = faction")
  if (is.null(mpdf$faction))
  {
    faction <- rep (0, length.out = length(mpdf$voting_ID))
    mpdf$faction <<- faction
  }
  mpdf2 <- fn$sqldf("select mpdf.*, votings.date 
                    from mpdf, votings 
                    where votings.voting_ID = mpdf.voting_ID")
  for (i in 1:length(fhistory$MP_ID))
  {
    d1 <- fhistory[i,3]
    d2 <- fhistory[i,4]
    dep <- fhistory[i,1]
    f <- fhistory[i,2]
    if (!is.na(d2)) mpdf2 <- fn$sqldf(c("update mpdf2 set faction = $f where (MP_ID = $dep) and (date < $d2) and (date >= $d1)","select * from mpdf2"))
      else mpdf2 <- fn$sqldf(c("update mpdf2 set faction = $f where (MP_ID = $dep)  and (date >= $d1)","select * from mpdf2"))
  } 
#mpdf2 <- fn$sqldf("update mpdf2 inner join fhistory 
#           using(MP_ID)  
#           set mpdf2.faction = fhistory.ID 
#           where ((mpdf2.date < out_date) and (mpdf2.date >= in_date))
#           or ((mpdf2.date >= in_date) and (out_date = null))",
#            "select * from main.mpdf2")
  mpdf <<- fn$sqldf("select voting_ID, MP_ID, voting, faction from mpdf2")
}

create_voting_tables <- function (votings, fdf, mpdf, startDate = NULL, endDate = NULL)
# Create a table in format suitable for comparing deputies
{
  preparevv <- function (startDate = NULL, endDate = NULL,
                         law_like_votings = FALSE)
  # Helper function. Takes a votings massive in a range of given dates.  
  {
    if (is.null(startDate)) startDate <- min(votings$date)
    if (is.null(endDate)) endDate <- max(votings$date)
    if (law_like_votings != TRUE) 
    {
      v <- fn$sqldf("select * 
                   from votings 
                   where (date <= $endDate)
                   and (date >= $startDate)")
      
    } else {
      v <- fn$sqldf("select * 
                     from votings 
                     where (date <= $endDate)
                     and (date >= $startDate)
                     and (type != 0)")
      
    }
    v
  }  
  
  preparefv <- function (startDate = NULL, endDate = NULL,
                         law_like_votings = FALSE)
  # Helper function. Takes factions votings.    
  {
    v <- preparevv (startDate = startDate, endDate = endDate,
                    law_like_votings = law_like_votings)
    fv <- fn$sqldf("select fdf.* 
                   from fdf, v 
                   where (v.voting_ID = fdf.voting_ID)
                   ")
    fv
  }  
  
  preparedv <- function (startDate = NULL, endDate = NULL,
                         law_like_votings = FALSE, absence_as_against = TRUE)
  # Helper function. Takes MP's votings.  
  {
    v <- preparevv (startDate = startDate, endDate = endDate,
                    law_like_votings = law_like_votings)
    mpdf2 <- fn$sqldf("select mpdf.* 
                   from mpdf, v 
                   where (v.voting_ID = mpdf.voting_ID)
                   ")
    if (!absence_as_against)
    {
      mpdf2 <- fn$sqldf("select * 
                        from mpdf2 
                        where not like('В%', voting)
                        ")
    }
    mpdf2
  }
  
  mpdf2 <- preparedv(startDate = startDate, endDate = endDate)
  v <- preparevv(startDate = startDate, endDate = endDate)
  fv <- preparefv(startDate = startDate, endDate = endDate)
  l <- list()
  f <- list()
  for (i in 1:length(v$voting_ID))
  {
    id <- v$voting_ID[i]
    t <- fn$sqldf("select MP_ID, voting, faction_id
                  from mpdf2
                  where voting_ID = $id
                  order by MP_ID")
    l[[i]] <- t
    t <- fn$sqldf("select faction_ID,
                  [for.] as for, against, abstain, 
                  did_not_vote, absent, support
                  from fv
                  where voting_ID = $id
                  order by faction_ID")
    f[[i]] <- t
  }
  v$pv <- l
  v$fv <- f
  v
}

addResults <- function(vottab, startDate = NULL, endDate = NULL)
{
  if (is.null(startDate)) startDate <- min(vottab$date)
  if (is.null(endDate)) endDate <- max(vottab$date)
  j <- match(startDate,vottab$date)  
  r <- numeric()
  while ((!(is.na(j)) && (j <= length(vottab$date))) && (vottab$date[j]<= endDate))
  {
    t <- vottab$pv[[j]]
    r[[j]] <- sum(vottab$fv[[j]]$'for')
    
    j <- j+1
  }
  r
}

loadData <- function ()
{
  load(file = "votan/mpdf.Rda", envir = globalenv())
  load(file = "votan/votings.Rda", envir = globalenv())
  load(file = "votan/fdf.Rda", envir = globalenv())
  load(file = "votan/factids.Rda", envir = globalenv())
  load(file = "votan/mpids.Rda", envir = globalenv())
  load(file = "votan/vottab.Rda", envir = globalenv())
}