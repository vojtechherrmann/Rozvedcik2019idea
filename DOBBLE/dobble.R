library(dplyr)
library(reshape2)


### DECLARE PARAMS ##############
nr_teams = 5
nr_players = 6
nr_redundant_symbols = 4
### DECLARE PARAMS ##############


# DELETES pics folder content
do.call(file.remove, list(list.files("DOBBLE/pics", full.names = TRUE)))

set.seed(95)

# set of guests from GS
guests_secret <- read.csv("DOBBLE/guests.csv", encoding = "UTF-8", stringsAsFactors = FALSE) 

# set of categories - CategoryId, CategoryInternalName, CategoryExternalName
CategoryInternalNames <- guests_secret %>% pull(CategoryInternalName) %>% unique()
categories <- data.frame(
  CategoryId =
    seq(length(CategoryInternalNames)),
  CategoryInternalName =
    CategoryInternalNames, 
  CategoryExternalName = 
    c("Jahoda", "Avokado", "Mango", "Citron", "Merunka", "Pomeranc", "Mandarinka", "Boruvka", "Dyne",
      "Jablko", "Banan", "Meloun", "Malina", "Broskev")[1:length(CategoryInternalNames)],
  stringsAsFactors = FALSE
)

# set of teams - TeamId, CategoryId1, , CategoryId2 ... , CategoryId5/6
teams <- read.csv("DOBBLE/teams.csv", encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  melt(id.vars = "TeamId", value.name = "CategoryInternalName") %>%
  left_join(categories, by = "CategoryInternalName") %>%
  select(TeamId, CategoryExternalName) %>%
  arrange(TeamId)

# set of guests - Name, CategoryId
guests_public <- guests_secret %>%
  left_join(categories, by = "CategoryInternalName") %>%
  select(Name, CategoryExternalName)

# set of colors
colors = c("black", "dodgerblue3", "forestgreen", "red", "gold2", "darkorchid2")[1:nr_teams]
# set of symbols
symbols = c(
  "2600", "2601", "2602", "2605", "260E", "2622",
  "262F", "263B", "2638", "2660", "2663", "2665",
  "2666", "266B", "26BD", "2708", "273F", "265B", 
  "26D6", "2702", "270E", "265C", "265E", "265A",
  "265D", "265F", "26AB", "0024", "00A3", "25A0"
)

makePlot <- function(dfToPlot, main, team_order, member_order, nr_to_display) {
  
  dfToPlot_inner = dfToPlot[sample(nrow(dfToPlot)),]
  
  n = nrow(dfToPlot_inner)
  
  wide = floor(sqrt(n))
  long = ceiling(n/wide)
  
  dfToPlot_inner$x = rep(seq(wide), long)[1:n]
  dfToPlot_inner$y = rep(seq(long), each = wide)[1:n]
  
  #png(paste0("DOBBLE/pics/", team_order, "_", member_order, "_", main, ".png"))
  png(paste0("DOBBLE/pics/", nr_to_display, "_", main, ".png"))
  plot(
    dfToPlot_inner$x,dfToPlot_inner$y,
    main = main,
    xlim = c(0.5, wide+0.5),
    ylim = c(0.5, long+0.5),
    cex = 10, 
    col = dfToPlot_inner$color,
    pch = stri_unescape_unicode(gsub("\\U","\\u", paste0("\\U", dfToPlot_inner$symbol), fixed=TRUE)),
    xaxt='n', yaxt='n', frame.plot=FALSE, xlab = "", ylab = ""
  )
  dev.off()
  
}


db <- data.frame(
  symbol = rep(symbols, length(colors)),
  color = rep(colors, each = length(symbols)),
  stringsAsFactors = FALSE
)
  
db <- db[sample(nrow(db)),]

used_symbols = 0
team_order = 1
tournamentmember_order = 1
tournamentmember_randomnumbers = sample(nr_teams*nr_players)

TeamIds <- teams %>% pull(TeamId) %>% unique()
for (team_id in TeamIds) {
  
  member_order = 1
  
  TeamContents <- teams %>% filter(TeamId == team_id) %>% pull(CategoryExternalName) %>% sample()
  
  team_points <- data.frame(
    CategoryExternalName = rep(TeamContents, 2+nr_redundant_symbols),
    symbol = NA,
    color = NA
  ) 
  
  common_symbols <- db[(used_symbols+1):(used_symbols+nr_players),]
  
  team_points[1:nr_players,"symbol"] = common_symbols$symbol
  team_points[1:nr_players,"color"] = common_symbols$color
  team_points[(nr_players+2):(2*nr_players),"symbol"] = common_symbols$symbol[1:(nr_players-1)]
  team_points[(nr_players+2):(2*nr_players),"color"] = common_symbols$color[1:(nr_players-1)]
  team_points[(nr_players+1),"symbol"] = common_symbols$symbol[nr_players]
  team_points[(nr_players+1),"color"] = common_symbols$color[nr_players]
  
  eigen_symbols <- db[(used_symbols+nr_players+1):(used_symbols+(nr_players*(1+nr_redundant_symbols))),]
  
  team_points[((2*nr_players)+1):((2+nr_redundant_symbols)*(nr_players)),"symbol"] = eigen_symbols$symbol
  team_points[((2*nr_players)+1):((2+nr_redundant_symbols)*(nr_players)),"color"] = eigen_symbols$color
  
  for (TeamMember in TeamContents) {
    makePlot(
      team_points %>% filter(CategoryExternalName == TeamMember), 
      TeamMember,
      team_order,
      member_order,
      tournamentmember_randomnumbers[tournamentmember_order]
    )
    
    member_order = member_order + 1
    tournamentmember_order = tournamentmember_order + 1
  }
  
  used_symbols = used_symbols + (nr_players*(1+nr_redundant_symbols))
  team_order = team_order + 1
 
}





