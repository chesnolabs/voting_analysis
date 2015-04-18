integrated_match <- function(x)
{
  (x['for_with'] + x['not_for_with'])/(x['for_all'] + x['not_for_all'])
}

table_mint <- function(facts)
{
  ret <- matrix(length(facts$ID),length(facts$ID))
  ret <- data.frame(ret)
#  ret[,1] <- facts$faction_title
  for (i in 1:length(facts$ID))
  {
#    ret[as.character(facts$faction_title[i])] <- rep(1, length(facts$ID))
    for (j in 1:length(facts$ID))
    {
      if (i != j) 
      {
        ret[j,i] <- integrated_match(compare_factions(facts$ID[j],facts$ID[i]))
      } else {
        ret[j,i] <- 1
      }
    }
  }
  names(ret) <- facts$faction_title
  ret$faction_title <- facts$faction_title
  ret
}