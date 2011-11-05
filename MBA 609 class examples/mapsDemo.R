# Two choropleth maps showing number of assaults (left) and the ratio
# of assaults to murders (right).

library(ggplot2)
library(maps)
states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

# Merge map data with crime data

choro <- merge(states, arrests, by = "region")

# Reorder the rows because order matters when drawing polygons
# and merge destroys the original ordering

choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, 
  fill = assault, geom = "polygon")
qplot(long, lat, data = choro, group = group, 
  fill = assault / murder, geom = "polygon")
  
# Map of Iowa

ia <- map_data("county", "iowa")
mid_range <- function(x) mean(range(x, na.rm = TRUE))
centres <- ddply(ia, .(subregion), 
  colwise(mid_range, .(lat, long)))
ggplot(ia, aes(long, lat)) + 
  geom_polygon(aes(group = group), 
    fill = NA, colour = "grey60") +
  geom_text(aes(label = subregion), data = centres, 
    size = 2, angle = 45)