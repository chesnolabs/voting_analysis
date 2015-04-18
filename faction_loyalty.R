L <- merge(fdf,mpdf, by.x = c("voting_ID", "faction_ID"), by.y = c("voting_ID", "faction_id"))
#L <- merge(L,votings, by = "voting_ID")
#L <- L[L$type != 0,]
ret <- NULL
ret2 <- NULL
for (i in 1:length(factids$ID))
{
  posmatch <- sum( L$support[(grepl("За",L$voting) == 1) & (L$faction_ID == factids$ID[i])]) / 
    sum(L$support[L$faction_ID == factids$ID[i]])
  for_cons <- sum( L$support[(grepl("За",L$voting) == 1) & (L$faction_ID == factids$ID[i])])
  for_all <- sum(L$support[L$faction_ID == factids$ID[i]])
  negmatch <- sum( (L$support == 0) & (grepl("За",L$voting) != 1) & (L$faction_ID == factids$ID[i])) /
    sum(L$support[L$faction_ID == factids$ID[i]] == 0)
  not_for_cons <- sum( (L$support == 0) & (grepl("За",L$voting) != 1) & (L$faction_ID == factids$ID[i]))
  not_for_all <- sum(L$support[L$faction_ID == factids$ID[i]] == 0)
  intmatch <- (sum( L$support[(grepl("За",L$voting) == 1) & (L$faction_ID == factids$ID[i])]) +
    sum( (L$support == 0) & (grepl("За",L$voting) != 1) & (L$faction_ID == factids$ID[i]))) /
    sum(L$faction_ID == factids$ID[i])
  ret <- rbind(ret, cbind(factids$ID[i],posmatch,negmatch, intmatch))
  ret2 <- rbind (ret2, cbind(factids$ID[i],for_cons, for_all, not_for_cons, not_for_all))
}
ret <- data.table(ret)
ret2 <- data.table(ret2)
names(ret) <- c("ID", "posmatch", "negmatch", "intmatch")
names(ret2) <- c("ID", "for_cons", "for_all", "not_for_cons", "not_for_all")
ret <- merge(ret, factids, by = "ID")
ret2 <- merge(ret2, factids, by = "ID")
write.csv(ret, "faction_discipline2.csv")
write.csv(ret2, "faction_discipline_absolute.csv")