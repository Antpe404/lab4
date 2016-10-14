#'Visualize aiport delays
#'
#' Visualize all airport delays in the nycflights13 package with a mapogram.
#' The package takes no arguments. 
#'
#'
#'@export


visualize_airport_delays<-function() {

require(ggplot2)  
require(dplyr) 
require(nycflights13) 


meantbl<-flights %>%
  group_by(dest) %>% 
  summarise(avg = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(airports,by=c("dest"="faa"))  %>% 
  filter(!is.na(avg)) %>% 
  filter(!is.na(lon)) %>%
  filter(!is.na(lat))
#maps::map_data()
#worldmap<-map_data('world')
USAUSA<-map_data('world') %>% 
  filter(region=="USA" | region == "Canada"|region =="Mexico") 
  
p<-ggplot(USAUSA,aes(x=long, y=lat,group = group)) +
  geom_polygon(fill="white",col="grey") +
  geom_point(data=meantbl,aes(x=lon,y=lat,group = avg,color=avg)) + 
  scale_colour_gradient(high = "red",low="green") +xlim(c(-179,-35)) +
  labs(title= "USA map of average delay \n at arival destination",
  y = "Latitude", x ="Longitude") 

plot(p)
}

#plot(1,1)


#arlines 

#glimpse(planes)
#glimpse(weather)
#glimpse(airlines)
#glimpse(airports)
#glimpse(flights)
#det genomsnittliga f?resningen f?r flyg gruperat  flygplatser sorterat p? long/lat
# i flights har vi flygplan och i airports har vi flygplatser, long och lat


#meantbl<-flights %>%
 # group_by(dest) %>% 
  #summarise(avg = mean(dep_delay, na.rm = TRUE))

#meantbl<-left_join(meantbl,airports,by=c("dest"="faa"))  

#semi_join(meantbl,airports,by=c("dest"="faa"))

#meantbl<-filter(meantbl,!is.na(avg)) %>% 
 # filter(!is.na(lon)) %>%
#  filter(!is.na(lat))


