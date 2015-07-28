### CDC Vaccine List ###

# I will be scraping URLs and names for tables of vaccine prices at different time points.

# Save the URL as a string.
website <- "http://www.cdc.gov/vaccines/programs/vfc/awardees/vaccine-management/price-list/archive.html"

# Parse the website to create an HTML or XML document.
# Sometimes htmlParse() does not work. Add the argument useInternalNodes=TRUE
# after listing the website in the command. If this does not work, try the other
# parsing commands in the XML package.
website.html <- htmlParse(website)

# Obtain the desired attributes of the HTML code.
# Give the parsed website, then provide information for R to look at the proper nodes.
# We want attributes so defined where the attribute can be found (href) and the function
# to get the attributes.
websites.list <- xpathApply(website.html, "//li/a", "href", fun=xmlGetAttr)

# Create a vector to hold the wanted websites scraped by the previous command
wanted.websites <- c()

# Use a for loop to move through websites.list (list of scraped websites).
# Add the wanted links to the vector wanted.websites after pasting the domain
# URL on to the link.
for(i in 1:87)
{
  wanted.websites <- c(wanted.websites, paste("http://www.cdc.gov", websites.list[[i+11]], sep=""))
}

# Getting the names of the lists is much easier. Luckily, the names are formatted as a list.

# Provide the website to be scraped
website <- "http://www.cdc.gov/vaccines/programs/vfc/awardees/vaccine-management/price-list/archive.html"

# Parse the website
website.html <- htmlParse(website)

# Read the parsed source code as an HTML list.
# The which argument allows us to save only the parts of the list we are interested in.
lists <- readHTMLList(website.html, which=4:10)
lists

# To read a vaccine price table into R, use the same process but use the readHTMLTable command,
# not the readHTMLList command.
website <- "http://www.cdc.gov/vaccines/programs/vfc/awardees/vaccine-management/price-list/index.html"
website.html <- htmlParse(website)
tables <- readHTMLTable(website.html)
tables