####opening up packages and making sure everything is installed####
#library(devtools)
#library(stringi)
#devtools::install_github("dkahle/ggmap")
library(ggmap)#load ggmap
library(rvest)#load rvest - we will use this to grab data
library(geosphere)#we will use this package to make a distance matrix
my_key<-"your key here" #add in the name of your API key here
register_google(key=my_key)#register your API key here 

####grabbing and cleaning some data####
  list_of_LA_hospitals<-"http://www.laalmanac.com/health/he02.php" #list of hospitals in LA
  hospital_webpage <- read_html(list_of_LA_hospitals) #reading webpage html
  grab_me<-"h5+ table .text-left:nth-child(2) , h5+ table .text-left:nth-child(1)" #designating content to grab from webpage 
  place_list <- html_nodes(hospital_webpage, grab_me) #grabbing the content we need 
  place_list_df <- as.data.frame(html_text(place_list))#storing as a dataframe
  place_list_df$place_list<-as.character(place_list_df[,1]) #making character column
  place_list_df<-place_list_df[place_list_df$place_list!="Facility Name" ,] #throwing out rows we don't want
  place_list_df<-place_list_df[place_list_df$place_list!="Address" ,] #throwing out rows we don't want
  place_list_df$row_num <- seq.int(nrow(place_list_df))#adding row numbers
  place_list_df$address_row<-0 #marking rows w/o addresses
  place_list_df$address_row[(place_list_df$row_num %% 2) == 0]<-1 #marking rows w addresses (i.e. even number rows)
  place_list_df2<-as.data.frame(cbind(place_list_df$place_list[place_list_df$address_row==0],
                                      place_list_df$place_list[place_list_df$address_row==1]))#collapsing into new dataframe
  names(place_list_df2)<-c("place","address")#adding column names
  place_list_df2$place<-as.character(place_list_df2$place)#storing as character
  place_list_df2$address<-as.character(place_list_df2$address)#storing as character

####looking at the geocode function####
  #geocode function can take address information and spit out long and lat
  #here are some examples 
  geocode("1600 Pennsylvania Ave NW, Washington, DC 20500")#enter in like this
  geocode("White House")#or, if you don't know the address this can work *some times*
                                         #when you need to disambiguate between several potential places with the same name,
                                         #this is not a good bet...
  #can apply to an entire list like this:
  place_list_ex<-place_list_df2[1:5,]#subsetting example dataframe
  geo_coordinates_ex<-geocode(place_list_ex$address) #running over all address w/in the dataframe 
  
  #we can also call for additional info and Google meta data on places 
  geocode("Ronald Reagan UCLA Medical Center", output="all") #set output equal to "all" here
 
  #writing a function to grab specific items we want
  grab_geo_info<-function(x="place"){
    results<- geocode(x, output="all")#storing results as objec
    neighborhood<-results$results[[1]]$address_components[[3]]$long_name #grabbing neighborhood name
    lat<-results$results[[1]]$geometry$location$lat #getting latitude
    lng<-results$results[[1]]$geometry$location$lng #getting longitude
    #type1<-results$results[[1]]$types[[1]] #getting place type 1
    #type2<-results$results[[1]]$types[[2]] #getting place type 2
    geo_stuff<-as.data.frame(cbind(lat, lng, neighborhood))#storing info in data frame
    return(geo_stuff)   }#return the frame 
  
  #let's give this function a test run 
  ex_results<-grab_geo_info(x="757 Westwood Plaza Los Angeles 90095")
  ex_results2<-ex_results #making a copy
  #then we can loop through these others if we want to add them on
  for (address in place_list_ex$address){
    print(address) #listing address
    ex_results3<-grab_geo_info(x=address) #applying function
    ex_results2<-as.data.frame(rbind(ex_results2,ex_results3))} #storing info together
  
  #removing example stuff from the environment 
  remove(ex_results)
  remove(ex_results2)
  remove(ex_results3)
  remove(example_list)
  remove(geo_coordinates_ex)
  remove(place_list_ex)

####going to geocode all of the hospitals now####
  result_list_1<-grab_geo_info(x=place_list_df2$address[1])#applying to first row of address 
                                                          #NOTE: indexing begins w 0 in R
  result_list_1<-as.data.frame(cbind (place_list_df2$place[1],place_list_df2$address[1], result_list_1)) #pulling everything together
  names(result_list_1)<-c("place","address","lat","lng","neighborhood") #adding column names 
  result_list_2<-result_list_1 #making copy 
  
  #now going to loop over remaining rows in the dataframe 
  for (i in 2:nrow(place_list_df2)){
    print(place_list_df2$place[i])
    result_list_x<-grab_geo_info(x=place_list_df2$address[i])#applying to 'i'th row of address 
    result_list_x<-as.data.frame(cbind (place_list_df2$place[i],place_list_df2$address[i], result_list_x))#pulling everything together
    names(result_list_x)<-names(result_list_1) #adding names again
    result_list_2<-rbind(result_list_2,result_list_x) #appending rows
    remove(result_list_x) } #throwing out unused dataframe 
  
  #it would probably be wise to export a copy of the geo-coordinates at this point, just in case something crashes later on
    getwd()#looking for current directory 
    write.csv(result_list_2, "comp_soc_ws_ex_hospital_lat_long.csv", row.names = F, na="")#writing out coordinates as a csv file 
  
####getting the distance between stuff####
  mapdist("Royce Hall UCLA","The Getty Villa",mode = 'driving') #enter the start point, then the destination, followed transportation mode
  mapdist("Royce Hall UCLA","Diddy Riese",mode = 'walking')#you can try swapping this out for other modes like walking, biking, in transit
  
  #now let's try finding the nearest neighbor for each of the hospitals
      #-this is kind of cheating since it relies on the straight-line rathe than diriving distance, but saves a lot of time!
  result_list_2$lat<-as.numeric(as.character(result_list_2$lat))#converting to numeric 
  result_list_2$lng<-as.numeric(as.character(result_list_2$lng))#converting to numeric 
  distance_mat <- distm(result_list_2[,c('lng','lat')], result_list_2[,c('lng','lat')], fun=distVincentyEllipsoid) #making a distance matrix
  diag(distance_mat)<-10^10 #setting diagnoal to a high value so that we don't misregonize a hospital as its own nearest neighbor  
  result_list_2$nearest_hospital <- result_list_2$place[max.col(-distance_mat)] #grabbing the name of the hospital w/min distance
  result_list_2$nearest_lat <- result_list_2$lat[max.col(-distance_mat)] #grabbing the lat of nearest hospital 
  result_list_2$nearest_lng<- result_list_2$lng[max.col(-distance_mat)] #grabbing the long of nearest hospital 
  
  #now we request all of the routes
  walking_distance1<-mapdist(paste(result_list_2$lat[1], ",",result_list_2$lng[1]), 
                             paste(result_list_2$nearest_lat[1], 
                                   ",",result_list_2$nearest_lng[1]), mode = 'walking') #applying function to 1st row
  walking_distance1$place<-result_list_2$place[1] #adding place column 
  walking_distance1$nearest_hospital<-result_list_2$nearest_hospital[1] #adding nearest place column 
  walking_distance2<-walking_distance1 #making copy 
  
  #Throwing out the Catalina hospital because we only want WALKING distance 
  result_list_2<-result_list_2[result_list_2$place!="Catalina Island Medical Center",]#remove this hospital
  
  #looping over remaing pairs of origin and destination points 
  for (i in 2:nrow(result_list_2)){
    print(i)
    print(result_list_2$place[i])
    walking_distance_ex<-mapdist(paste(result_list_2$lat[i], ",",result_list_2$lng[i]), 
                                 paste(result_list_2$nearest_lat[i], 
                                       ",",result_list_2$nearest_lng[i]), mode = 'walking') #applying function to `i`th row
    walking_distance_ex$place<-result_list_2$place[i] #adding place column 
    walking_distance_ex$nearest_hospital<-result_list_2$nearest_hospital[i] #adding nearest place column 
    names(walking_distance_ex)<-names(walking_distance1)#making sure names are standardized
    walking_distance2<-as.data.frame(rbind(walking_distance2, walking_distance_ex))#appending data
    remove(walking_distance_ex)}#removing un-used data frame  
  
  #saving these results too
  write.csv(walking_distance2, "comp_soc_ws_walking_distance.csv", row.names = F, na="")#writing out distances as a csv file 
  
#### Some basic visualizations####  
  LA_county1 <- get_map("Los Angeles County",zoom=10)#calling for plot of LA county, where zoom parameter =10
  ggmap(LA_county1)#let's try plotting it 
  LA_county2<-get_map("Los Angeles County",zoom=10,maptype = "satellite")#grabbing a satellite image from Google
  ggmap(LA_county2)#Looks pretty cool 
  LA_county3 <- get_map("Los Angeles County",zoom=10, color="bw")#going with a more basic layout for graphing 
  ggmap(LA_county3)#plotting
  
  #going to add a layer of geometric points for the hospitals 
      #for those already familiar w ggplot2, this syntax should basically be identical 
  ggmap(LA_county3) + 
       geom_point(data=result_list_2,
       aes(x=lng,y=lat),
        size=4,alpha=.7)
  #now going to add a layer of capture the density/clustering of the points
  ggmap(LA_county3) + 
    geom_point(data=result_list_2,
               aes(x=lng,y=lat),
               size=4,alpha=.7) +
                stat_density2d(data = result_list_2,
                   aes(x = lng, y = lat,fill = ..level.., alpha = ..level..), 
                   geom = "polygon") + 
                    scale_fill_gradient(low = "green", high = "red") + 
                      scale_alpha(range = c(0, 0.75), guide = FALSE)
  #now looking at route info 
  begin_point<-paste(result_list_2$lat[99], ",", result_list_2$lng[99]) #saving begin point info for 1st row
  end_point<-paste(result_list_2$nearest_lat[99], ",", result_list_2$nearest_lng[99]) #saving end point info for 1st row
  example_route_df <- route(begin_point, end_point, structure = "route") #creating route object
  bel_air<- get_map("Bel Air",zoom=12, color="bw")#making map object 
  ggmap(eagle_rock)#plotting
  
  #now adding route layer!
  ggmap(bel_air) +
      geom_path(
        aes(x = lon, y = lat),  
        colour = "blue", size = 1.5,
        data = example_route_df, lineend = "round") #fun fact, this hospital in Van Nuys is where both my mom and mother in law were born
