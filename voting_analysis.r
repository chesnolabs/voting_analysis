downloadMP <- function (fl) 
{
	x <- read.csv(fl, nrows = 5)
	classes <- sapply(x,class)
	mpdf <<- read.csv(fl, colClasses = classes)
}

downloadFactions <- function(fl)
{
  classes = rep("numeric",7)
	fdf <<- read.csv(fl, colClasses = classes, comment.char = "\"", quote = "")
}

downloadVotings <- function(fl)
{
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y"))   
  classes <- c ("numeric","myDate","character","character")
	votings <<- read.table(fl, colClasses = classes, sep = "\t", header = TRUE, quote = "")
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
	downloadMP (x[[3,1]])
	downloadFactions (x[[4,1]])
	downloadVotings (x[[5,1]])
#	addFactionHistory (x[[6,1]])
	set_voting_types()
	set_faction_support()
  vottab <<- create_voting_tables()
  votings$result <<- addResults()
  vottab$result <<- votings$result
  save(mpdf, file = "votan/mpdf.Rda")
  save(votings, file = "votan/votings.Rda")
  save(fdf, file = "votan/fdf.Rda")
  save(factids, file = "votan/factids.Rda")
  save(mpids, file = "votan/mpids.Rda")
  save(vottab, file = "votan/vottab.Rda")
}

set_voting_types <- function (first = NULL, last = NULL)
{
	if (is.null(first)) first <- min(votings$voting_ID)
	if (is.null(last)) last <- max(votings$voting_ID)
	if (is.null(votings$type))
	{
		type <- rep (0, length.out = length(votings$voting_ID))
		votings$type <<- type
	}
  votings <<- fn$sqldf(c("update votings set type = 1 where like('%проект Закону%',title) and like('%за основу%',title)", "select * from main.votings"))
	votings <<- fn$sqldf(c("update votings set type = 2 where like('%проект Закону%',title) and like('%в цілому%',title)",  "select * from main.votings"))
  votings <<- fn$sqldf(c("update votings set type = 3 where like('%поправ%',title) and like ('%проекту Закону%',title)",  "select * from main.votings"))
	votings <<- fn$sqldf(c("update votings set type = 4 where like ('%порядку денного%',title) and like ('%проект%',title) and like('%закон%',title) and (like('%розгляд%',title) = 0) ",  "select * from  main.votings"))
}

set_faction_support  <- function(first = NULL, last = NULL, light = FALSE)
{ 
  if (is.null(first)) first <- min(votings$voting_ID)
  if (is.null(last)) last <- max(votings$voting_ID)
  if (is.null(fdf$support))
  {
    support <- rep (0, length.out = length(fdf$voting_ID))
    fdf$support <<- support
  }  
  fdf <<- fn$sqldf(c("update fdf set support = 0 where (voting_ID >= $first) and (voting_ID <= $last)","select * from main.fdf"))
  if (light) 
    fdf <<- fn$sqldf(c("update fdf set support = 1 where ([for.] > abstain + against + did_not_vote) and (voting_ID >= $first) and (voting_ID <= $last) ","select * from main.fdf"))
  else fdf <<- fn$sqldf(c("update fdf set support = 1 where ([for.] > abstain + against + did_not_vote + absent) and (voting_ID >= $first) and (voting_ID <= $last)","select * from main.fdf")) 
  
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

loadData <- function ()
{
  load(file = "votan/mpdf.Rda", envir = globalenv())
  load(file = "votan/votings.Rda", envir = globalenv())
  load(file = "votan/fdf.Rda", envir = globalenv())
  load(file = "votan/factids.Rda", envir = globalenv())
  load(file = "votan/mpids.Rda", envir = globalenv())
  load(file = "votan/vottab.Rda", envir = globalenv())
}

compare_with_faction <- function (deps = NULL, fid = NULL, 
                      startDate = NULL, endDate = NULL, 
                      law_like_votings = FALSE, absence_as_against = TRUE)
{
  if ((class(deps) != "numeric") & (class(deps) != "integer")) stop("Invalid deputies shortlist")
  if (!(fid %in% factids$ID)) stop("Invalid faction_ID")
  if (is.null(startDate)) 
  {
    startDate <- min(votings$date)
  } else {
    startDate <- as.Date(startDate, format="%d.%m.%Y")
  }
  if (is.null(endDate)) 
  {
    endDate <- max(votings$date)
  } else
  {
    startDate <- as.Date(endDate, format="%d.%m.%Y")
  }
  ret <- matrix(ncol = 5,nrow = length(deps))
  ret <- data.frame(ret)
  names(ret) <- c("deputy","for_with_faction","for_all",
                  "not_for_with_faction","not_for_all")
  for (i in 1:length(deps))
  {
    d <- deps[i]
    fwf <- 0
    fa <-0
    nfwf <- 0
    nfa <- 0
    j <- closestDate(searchDate = startDate, dateList = vottab$date)
    if (is.na(j)) stop("Немає голосувань з такими датами, або ж дата не в тому форматі")
    while ((j <= length(vottab$date)) && (vottab$date[j]<= endDate))    
    {
      if ((vottab$type[j] != 0) || (law_like_votings != TRUE))
      {
        dp <- match(d,vottab$pv[[j]]$MP_ID)
        fp <- match(fid,vottab$fv[[j]]$faction_ID)
        if ((!is.na(dp)) && (!is.na(fp)))
        {
          if (grepl("За",vottab$pv[[j]]$voting[dp]) == 1)
          {
            fa <- fa + 1
            if (vottab$fv[[j]]$support[fp] == 1) fwf <- fwf+1
          }
          else  {
            if ((grepl("Відс",vottab$pv[[j]]$voting[dp]) != TRUE) || (absence_as_against))
            {
              if (!is.null(vottab$pv[[j]]$voting[dp])) nfa <- nfa + 1
              if (vottab$fv[[j]]$support[fp] == 0) nfwf <- nfwf+1
            }
          }  
        }
      }
      j <- j+1
    }
    ret$for_with_faction[i] <- fwf
    ret$for_all[i] <- fa
    ret$not_for_with_faction[i] <- nfwf
    ret$not_for_all[i] <- nfa
    ret$deputy[i] <- as.character(fn$sqldf("select Deputy_name from mpids
                                           where MP_ID = $d")[[1]])
  }
  ret
}

compare_deputies <- function (d1 = NULL, d2 = NULL, 
                           startDate = NULL, endDate = NULL,
                           law_like_votings = FALSE, absence_as_against = TRUE)
{
  if (!(d1 %in% mpids$MP_ID)) stop("Некоректний ID першого депутата")
  if (!(d2 %in% mpids$MP_ID)) stop("Некоректний ID другого депутата")
  if (is.null(startDate)) 
  {
    startDate <- min(votings$date)
  } else {
    startDate <- as.Date(startDate, format="%d.%m.%Y")
  }
  if (is.null(endDate)) 
  {
    endDate <- max(votings$date)
  } else
  {
    startDate <- as.Date(endDate, format="%d.%m.%Y")
  }
  fwd <- 0
  fa <-0
  nfwd <- 0
  nfa <- 0
  j <- closestDate(searchDate = startDate, dateList = vottab$date)
  if (is.na(j)) stop("Немає голосувань з такими датами, або ж дата не в тому форматі")
  while ((j <= length(vottab$date)) && (vottab$date[j]<= endDate))
  {
    if ((vottab$type[j] != 0) || (law_like_votings != TRUE)) 
    {
      dp1 <- match(d1,vottab$pv[[j]]$MP_ID)
      dp2 <- match(d2,vottab$pv[[j]]$MP_ID)
      if ((!is.na(dp1)) && (!is.na(dp2)))
      {
        if (grepl("За", vottab$pv[[j]]$voting[dp1]))
        {
          fa <- fa + 1
          if (grepl("За", vottab$pv[[j]]$voting[dp2])) fwd <- fwd + 1
        } else {
          if ((grepl("Відс",vottab$pv[[j]]$voting[dp1]) != 1) || (absence_as_against))
          {
            nfa <- nfa + 1
            if ((grepl("За", vottab$pv[[j]]$voting[dp2]) != 1) &&  
                  ((grepl("Відс",vottab$pv[[j]]$voting[dp2]) != 1) 
                   || (absence_as_against)))
              nfwd <- nfwd + 1
          }       
        }
      }
    }
    j <- j +1 
  }
  ret <- c(fwd,fa,nfwd,nfa)
  names(ret) <- c("for_with","for_all","not_for_with","not_for_all")
  ret
}

compare_factions <- function (f1 = NULL, f2 = NULL, startDate = NULL, endDate = NULL,
                              law_like_votings = FALSE)
{
  if (!(f1 %in% factids$ID)) stop("Некоректний ID першої фракції")
  if (!(f2 %in% factids$ID)) stop("Некоректний ID другої фракції")
  if (is.null(startDate)) 
  {
    startDate <- min(votings$date)
  } else {
    startDate <- as.Date(startDate, format="%d.%m.%Y")
  }
  if (is.null(endDate)) 
  {
    endDate <- max(votings$date)
  } else
  {
    startDate <- as.Date(endDate, format="%d.%m.%Y")
  }
  fwf <- 0
  fa <-0
  nfwf <- 0
  nfa <- 0
  j <- closestDate(searchDate = startDate, dateList = vottab$date)
  if (is.na(j)) stop("Немає голосувань з такими датами, або ж дата не в тому форматі")
  while ((j <= length(vottab$date)) && (vottab$date[j]<= endDate))
  {
    if ((vottab$type[j] != 0) || (law_like_votings != TRUE)) 
    {
      fp1 <- match(f1,vottab$fv[[j]]$faction_ID)
      fp2 <- match(f2,vottab$fv[[j]]$faction_ID)
      if ((!is.na(fp1)) && (!is.na(fp2)))
      {
        if (vottab$fv[[j]]$support[fp1] == 1)
        {
          fa <- fa + 1
          if (vottab$fv[[j]]$support[fp1] == vottab$fv[[j]]$support[fp2])
            fwf <- fwf + 1
        } else
        {
          nfa <- nfa + 1
          if (vottab$fv[[j]]$support[fp1] == vottab$fv[[j]]$support[fp2])
            nfwf <- nfwf + 1
        }
      }
    }
    j <- j + 1
  }
  ret <- c(fwf,fa,nfwf,nfa)
  names(ret) <- c("for_with","for_all","not_for_with","not_for_all")
  ret
}

preparedv <- function (startDate = NULL, endDate = NULL,
                       law_like_votings = FALSE, absence_as_against = TRUE)
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
preparefv <- function (startDate = NULL, endDate = NULL,
                       law_like_votings = FALSE)
{
  v <- preparevv (startDate = startDate, endDate = endDate,
                  law_like_votings = law_like_votings)
  fv <- fn$sqldf("select fdf.* 
                   from fdf, v 
                   where (v.voting_ID = fdf.voting_ID)
                   ")
  fv
}  



compare_shortlist <- function (shortlist = NULL, startDate = NULL, endDate = NULL,
                       law_like_votings = FALSE, absence_as_against = TRUE)
{
  if (sum(shortlist %in% mpids$MP_ID)<length(shortlist))
    stop("Не всі ID депутатів були введені коректно")
  col <- rep(list(numeric(0)), length(shortlist))
  ret <- data.frame()
  n <- character()
  for (i in 1:length(shortlist))
  {
    t <- shortlist[i]
    n <- c(n, as.character(fn$sqldf("select Deputy_name 
                                    from mpids
                                    where MP_ID = $t")[[1,1]]))
    ret[,i] <- col    
  }
  names(ret) <- n
  
  for (i in 1:length(shortlist))
    for (j in 1:length(shortlist))
    {
      if (i==j) 
      {
        ret[i,j] <- NA
        next
      }           
      ret[[i,j]] <- list(compare_deputies(shortlist[i],shortlist[j],startDate = startDate, 
                                 endDate = endDate,
                                 law_like_votings = law_like_votings, 
                                 absence_as_against = absence_as_against))
      
    }
  row.names(ret) <- n
  ret
}

preparevv <- function (startDate = NULL, endDate = NULL,
                       law_like_votings = FALSE)
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

create_voting_tables <- function (startDate = NULL, endDate = NULL)
{
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

addResults <- function(startDate = NULL, endDate = NULL)
{
  if (is.null(startDate)) startDate <- min(votings$date)
  if (is.null(endDate)) endDate <- max(votings$date)
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

closestDate <- function(searchDate, dateList) 
{
  dist <- dateList - searchDate
  which(min(dist[dist>=0]) == dist)[1]
}

compare_faction_with_deps <- function (deps = NULL, fid = NULL, 
                                  startDate = NULL, endDate = NULL, 
                                  law_like_votings = FALSE, absence_as_against = TRUE)
{
  if ((class(deps) != "numeric") & (class(deps) != "integer")) stop("Invalid deputies shortlist")
  if (!(fid %in% factids$ID)) stop("Invalid faction_ID")
  if (is.null(startDate)) 
  {
    startDate <- min(votings$date)
  } else {
    startDate <- as.Date(startDate, format="%d.%m.%Y")
  }
  if (is.null(endDate)) 
  {
    endDate <- max(votings$date)
  } else
  {
    startDate <- as.Date(endDate, format="%d.%m.%Y")
  }
  ret <- matrix(ncol = 5,nrow = length(deps))
  ret <- data.frame(ret)
  names(ret) <- c("deputy","for_with_faction","for_all",
                  "not_for_with_faction","not_for_all")
  for (i in 1:length(deps))
  {
    d <- deps[i]
    fwf <- 0
    fa <-0
    nfwf <- 0
    nfa <- 0
    j <- closestDate(searchDate = startDate, dateList = vottab$date)
    if (is.na(j)) stop("Немає голосувань з такими датами, або ж дата не в тому форматі")
    while ((j <= length(vottab$date)) && (vottab$date[j]<= endDate))    
    {
      if ((vottab$type[j] != 0) || (law_like_votings != TRUE))
      {
        dp <- match(d,vottab$pv[[j]]$MP_ID)
        fp <- match(fid,vottab$fv[[j]]$faction_ID)
        if ((!is.na(dp)) && (!is.na(fp)))
        {
          if ((vottab$fv[[j]]$support[fp] == 1) && ((grepl("Відс",vottab$pv[[j]]$voting[dp]) != TRUE) || (absence_as_against)))
          {
            fa <- fa + 1
            if (grepl("За",vottab$pv[[j]]$voting[dp]) == 1) fwf <- fwf+1
            
          }
          else  {
            if ((grepl("Відс",vottab$pv[[j]]$voting[dp]) != TRUE) || (absence_as_against))
            {
              if (!is.null(vottab$fv[[j]]$support[fp])) nfa <- nfa + 1
              if (grepl("За",vottab$pv[[j]]$voting[dp]) != 1) nfwf <- nfwf+1
            }
          }  
        }
      }
      j <- j+1
    }
    ret$for_with_faction[i] <- fwf
    ret$for_all[i] <- fa
    ret$not_for_with_faction[i] <- nfwf
    ret$not_for_all[i] <- nfa
    ret$deputy[i] <- as.character(fn$sqldf("select Deputy_name from mpids
                                           where MP_ID = $d")[[1]])
  }
  ret
}

library(sqldf)

