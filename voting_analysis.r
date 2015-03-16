start <- function()
{
  source("voting_data.r", local = TRUE)
  cat("It is a cript for MPs' votings analysis 
        Please choose what do yo want to do: 
        1) Load votings dataset in R format
        2) Append some votings in .csv format to R dataset
        3) Create whole new dataset from .csv format raw data")
  choice <- readline()
  if (choice == 1)
  {
    loadData()
  } else {
    if (choice == 2)
    {
      append()
    } else {
      if (choice == 3) 
      {
        votan.init()
      } else {
        cat("Incorrect choice. Please run start() function one more time.")
      }
    }
  }
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
start()


