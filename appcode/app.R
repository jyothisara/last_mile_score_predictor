#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(odbc)
library(DBI)
library(gsubfn)
library(tidyverse)

last_mile_score <- function(c,s){
    
    ## DB Connection
    con <- dbConnect(odbc(),
                     driver = "PostgreSQL ANSI(x64)",
                     database = "osm",
                     UID    = "postgres",
                     PWD = "London@25",
                     host = "localhost",
                     port = 5432)
    
    ##Getting the Destination Record 
    dest<- fn$dbGetQuery(con,"SELECT *
      from  planet_osm_point
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= .1) 
     and ((boundary <> 'administrative') or (boundary is null));")
    if (nrow(dest)==0) {dest<- fn$dbGetQuery(con,"SELECT *
      from  planet_osm_polygon
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= .1) 
     and ((boundary <> 'administrative') or (boundary is null));")}
    
    
    #Total no of points in last mile
    num_points<- fn$dbGetQuery(con,"SELECT count(*)
      from  planet_osm_point
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600);")
    
    #Total no of polygons in last mile
    num_polygons <- fn$dbGetQuery(con,"SELECT count(*)
      from  planet_osm_polygon
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
       and ((boundary <> 'administrative') or (boundary is null));")
    
    num_buildings <- as.numeric((num_points + num_polygons)[[1]])
    
    
    if (num_buildings < 500) {near_points <- 10
    }else near_points <- (-(num_buildings/950)+(200/19))
    if (near_points < 0 ){near_points <-0}
    
    
    #Access
    if (dest['access'] %in% c('yes','customers','permissive','delivery','barrier-free')) {access <- 10
    }else if (dest['access'] %in% c('private','limited','emergency')) {access <- 6
    }else if (dest['access'] %in% c('no','restricted','blocked')) {access <- 1
    }else access <- 5
    
    #Amenity        
    if (dest['amenity'] %in% c('pub','bank','restaurant','bar','cafe','fuel','pharmacy','library')) {amenity <- 9
    }else if (dest['amenity'] %in% c('university','college','school')) {amenity <- 6
    }else if (is.na(dest['amenity'])) {amenity <- 5
    }else amenity <- 2  
    
    #Building        
    if (dest['building'] %in% c('retail',	'church',	'commercial',	'public',	'hotel')) {building <- 9
    }else if (dest['building'] %in% c('university','college','school','office','industrial')) {building <- 6
    }else if (dest['building'] %in% c('semidetached_house','dormitory','apartments','residential','house')) {building <- 4
    }else if (is.na(dest['building'])) {building <- 5
    }else building <- 2
    
    #Landuse
    if (dest['landuse'] %in% c('retail','commercial')) {landuse <- 8
    }else if (dest['landuse'] %in% c('residential')) {landuse <- 4
    }else if (is.na(dest['landuse'])) {landuse <- 5
    }else landuse <- 2  
    
    #Bicycle
    if (dest['bicycle'] %in% c('yes','designated')) {bicycle <- 10
    }else if (dest['bicycle'] %in% c('no')) {bicycle <- 0
    }else if (is.na(dest['bicycle'])) {bicycle <- 5
    }else bicycle <- 2 
    
    
    #Parking
    parking <- fn$dbGetQuery(con,"select access,name,way, 
  ROUND(ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s)))::numeric,3) as dist from planet_osm_point where amenity ='parking'
	order by dist limit 1;")
    
    #Nearest PArking
    
    
    if (parking['dist'] < 100) {p_dist <- 10
    }else p_dist <-  ((-(parking['dist'])/100)+11)
    if (p_dist < 0 ){p_dist <-0}
    
    p_dist <- as.numeric(p_dist[[1]])
    
    #Parking access
    if (parking['access'] %in% c('yes','customers','public','permissive','delivery','barrier-free')) {p_access <- 10
    }else if (parking['access'] %in% c('private','limited','emergency')) {p_access <- 6
    }else if (parking['access'] %in% c('no','restricted','blocked')) {p_access <- 1
    }else p_access <- 5
    
    
    #Nearest Parking aisle
    park_aisle <- fn$dbGetQuery(con,"select highway,name,way, 
  ROUND(ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s)))::numeric,3) as dist from planet_osm_line where service ='parking_aisle'
	order by dist limit 1;")
    
    if (park_aisle['dist'] < 100) {pa_dist <- 10
    }else pa_dist <-  ((-(park_aisle['dist'])/100)+11)
    if (pa_dist < 0 ){pa_dist <-0}
    
    
    pa_dist <- as.numeric(pa_dist[[1]])
    
    #Roads
    #Total no of roads in last mile
    num_roads<- fn$dbGetQuery(con,"SELECT count(*)
      from  planet_osm_line
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
      and highway in ('official','designated','yes', 'unclassified','path','secondary','tertiary','residential','track','motorway_link','service','motorway');")
    
    if (num_roads > 1000) {near_roads <- 10
    }else near_roads <-  (num_roads/100)
    
    near_roads <- as.numeric(near_roads[[1]])
    
    barrier <- fn$dbGetQuery(con,"SELECT count(*)
      from  planet_osm_line
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
      and barrier is not null;")
    
    if (barrier < 150){barrier_score <- 10
    } else barrier_score<-(-0.006*barrier)+10.3
    if (barrier_score < 0 ){barrier_score <-0}
    
    barrier_score <- as.numeric(barrier_score[[1]])
    
    oneway <- fn$dbGetQuery(con,"SELECT count(*)
      from  planet_osm_line
      where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
      and oneway = 'yes';")
    
    if (oneway < 150){oneway_score <- 10
    } else oneway_score<-(-0.01*oneway)+11.5
    if (oneway_score < 0 ){oneway_score <-0} 
    
    oneway_score <- as.numeric(oneway_score[[1]])
    
    
    #Nearest Highway
    highway_line <- fn$dbGetQuery(con,"select access,barrier,bicycle,bridge,highway,oneway,ref,route,service,toll,tourism,tracktype,way,
  ROUND(ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s)))::numeric,3) as dist from planet_osm_line where highway in ('official','designated','yes','path','secondary','track','motorway_link','motorway')
	order by dist limit 1;")
    
    if (highway_line['dist'] < 500) {highway <- 10
    }else highway <-  ((-(highway_line['dist'])/500)+11)
    if (highway < 0 ){highway <-0}
    highway <- as.numeric(highway[[1]])
    
    #Parking Score
    if (pa_dist >p_dist){ parking_score <- pa_dist
    parking <- park_aisle 
    }else {parking_score <- (0.5*p_dist) + (0.5*p_access)}
    
    #Roads Score
    road_score <- as.numeric((0.5*highway) + (0.3*near_roads) + (.05*barrier_score) + (0.15*oneway_score))
    
    #LML Score
    lml_score <- round((0.2*parking_score)+(0.2*road_score)+(0.1*access)+(0.1*amenity)+
                           (0.1*building)+(0.1*near_points)+(0.075*landuse)+(0.025*bicycle),2)
    names(lml_score) <- "last mile score"
    return(lml_score)
}


#Define UI for app  ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Last Mile Score Predictor"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the inputs ----
            textInput("coord","Input coordinates"),
            textInput("srid","Input SRID")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            paste("The last mile score is"),
            textOutput("txtOutput")
        )
    )
)

server <- shinyServer(function(input,output) {
    
    ({})
    output$txtOutput <- renderText({
        paste(last_mile_score(input$coord,input$srid) )
    })
})

shinyApp(ui=ui,server=server)