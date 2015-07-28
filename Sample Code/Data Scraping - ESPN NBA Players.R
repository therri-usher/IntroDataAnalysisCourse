# ESPN Data #

# Start with parsing the website for team rosters
website <- "http://espn.go.com/nba/players"
html.score <- htmlParse(website, useInternalNodes=T)

# Obtain the links present in the parsed code
other.links <- xpathSApply(html.score, "//a", "href", fun=xmlGetAttr)

# Create an index to save the proper links
index <- 20 + 2*(1:30)

# Paste the missing part of the URLs to the proper links
other.links <- paste("http://espn.go.com", other.links[index], sep="")
other.links

# Create variables to hold player names and links to their ESPN profiles
all.links <- c()
all.names <- c()

# Loop through the URLs to obtain player rosters for each team
for(k in 1:length(other.links))
{
  # Parse each team's roster website
  parse.site <- htmlParse(other.links[k], useInternalNodes=T)
  
  # Get the links to the player profiles
  links <- xpathSApply(parse.site, "//td[@class='sortcell']/a", "href", fun=xmlGetAttr)
  
  # Get the player names
  names <- xpathSApply(parse.site, "//td[@class='sortcell']/a", fun=xmlValue)
  for(l in 1:length(links))
  {
    # Loop through each player to add his name and profile link to the defined list of player names and profile links
    all.links <- c(all.links, links[l])
    all.names <- c(all.names, names[l])
  }
}

# Another way to obtain player names
player <- c()

for(k in 1:length(all.links))
{
  html.player <- htmlParse(all.links[k], useInternalNodes=T) 
  player[k] <- xpathSApply(html.player, "//h1", fun=xmlValue)[2]
}

# TIP: If you are interested in the entire roster, along with player info,
# try using the readHTMLTable command.

dat <- c()

for(k in 1:length(other.links))
{
  parse.site <- htmlParse(other.links[k], useInternalNodes=T)
  team.list <- readHTMLTable(parse.site)
  dat <- c(dat, team.list[1])
}