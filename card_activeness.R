card_active <- function(votes)
{
  if (sum(grepl("Відс",votes)) < length(votes))
    1 else
    {
      0
    }
  
}

fuck_grouping <- function()
{
  un <- unique(A[,c("date","session_status"), with = FALSE])
  pres <- NULL
#   for (i in 1:length(un$date))
#   {
#     for (j in 1:length)
#   }
}

percent_activity <- function(votes)
{
  wh <- length(votes)
  part <- wh- sum(grepl("Відс", votes))
  part / wh
}

A <- data.table(merge(votings, mpdf, by = "voting_ID"))
A[, session_status := ifelse(time < "16:00:00", "Ранкове", "Вечірнє")]
A[date == "2014-12-29", "session_status"] <- "Ранкове"
A[(date == "2014-12-28") & (session_status == "Вечірнє"), ] <- "Ранкове"
A[date == "2014-12-04", "session_status"] <- "Ранкове"
A[date == "2014-12-23", "session_status"] <- "Ранкове"
A[date == "2014-12-25", "session_status"] <- "Ранкове"
A[date == "2015-02-10", "session_status"] <- "Ранкове"
A[date == "2014-12-29", "date"] <- as.Date("2014-12-28")
deps <- read.csv("don_deps.csv")
A <- A[MP_ID %in% deps$MP_ID, ]
A[, present := card_active(voting), by = list(MP_ID, date, session_status)]
A[, activity_percent := percent_activity(voting), by = list(MP_ID, date, session_status)]
