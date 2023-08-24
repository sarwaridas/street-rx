library(data.table)
library(ggplot2)
library(magrittr)

voter_stats = fread("Data/part2/voter_stats_20201103.txt")
history_stats = fread("Data/part2/history_stats_20201103.txt")

counties <- unique(history_stats$county_desc)
set.seed(212)
random25 <- sample(counties, 25)

voter_stats = voter_stats[county_desc %in% random25]
history_stats = history_stats[county_desc %in% random25]
voter_stats$update_date = NULL
voter_stats$stats_type = NULL
voter_stats$election_date = NULL
history_stats$election_date = NULL
history_stats$stats_type = NULL
history_stats$update_date = NULL
history_stats$party_cd = history_stats$voted_party_cd
history_stats$voted_party_cd = NULL
history_stats$hist_n_voters = history_stats$total_voters 
voter_stats$vote_n_voters = voter_stats$total_voters 
history_stats$total_voters = NULL
voter_stats$total_voters = NULL

history_stats = history_stats[,.(hist_n_voters = sum(hist_n_voters)),
                              by=.(county_desc,precinct_abbrv,vtd_abbrv,age,party_cd,race_code,ethnic_code,sex_code)]

merge_vars = names(history_stats)[names(history_stats) != c("hist_n_voters")]
df = merge(voter_stats, history_stats, by=merge_vars)
df[is.na(hist_n_voters), hist_n_voters:=0]

df[race_code == "", race_code := "U"]
df2 = df[,.(hist_n_voters = sum(hist_n_voters),
            vote_n_voters = sum(vote_n_voters)),
                              by=.(county_desc,age,party_cd,race_code,ethnic_code,sex_code)]

#fwrite(df, "Data/part2/cleaned_sk_20211013.csv")
fwrite(df2, "Data/part2/extra_cleaned_sk_20211019.csv")









