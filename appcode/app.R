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
library(DT)


#Defining the Last Mile Score as a function
last_mile_score <- function(c,s){
    
    ## DB Connection
    con <- dbConnect(odbc(),
                     driver = "PostgreSQL ANSI(x64)",
                     database = "osm_maynooth",
                     UID    = "postgres",
                     PWD = "London@25",
                     host = "localhost",
                     port = 5432)
    
    
    ##Getting the Destination Object
    
    #checking at planet_osm_points
    dest<- fn$dbGetQuery(con,"SELECT *
        from  planet_osm_point
        where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= .1);")
    
    #checking at planet_osm_polygons if not found in points
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
    
    #Total number of objects
    num_objects <- as.numeric((num_points + num_polygons)[[1]])
    
    
    if (num_objects < 500) {near_points <- 10
    }else near_points <- (-(0.001*num_objects)+10.5)
    if (near_points < 0 ){near_points <-0}
    
    #Access Score
    if (dest['access'] %in% c('yes','customers','permissive','delivery','barrier-free')) {access <- 10
    }else if (dest['access'] %in% c('private','limited','emergency')) {access <- 7
    }else if (dest['access'] %in% c('no','restricted','blocked')) {access <- 1
    }else access <- 5
    
    #Amenity Score       
    if (dest['amenity'] %in% c('pub','bank','restaurant','bar','cafe','fuel',
                               'pharmacy','library')) {amenity <- 10
    }else if (dest['amenity'] %in% c('university','college','school')) {amenity <- 8
    }else if (is.na(dest['amenity'])) {amenity <- 5
    }else amenity <- 6  
    
    
    #Building Score      
    if (dest['building'] %in% c('retail',	'church',	'commercial',	'public',
                                'hotel')) { building <- 9
    }else if (dest['building'] %in% c('university','college','school','office',
                                      'industrial')) { building <- 7
    }else if (dest['building'] %in% c('semidetached_house','dormitory','apartments',
                                      'residential','house')) { building <- 4
    }else if (is.na(dest['building'])) { building <- 5
    }else building <- 6
    
    
    #Landuse Score
    if (dest['landuse'] %in% c('retail','commercial')) {landuse <- 10
    }else if (dest['landuse'] %in% c('residential')) {landuse <- 3
    }else landuse <- 5 
    
    
    ##Parking
    #Nearest parking from points
    parking_point <- fn$dbGetQuery(con,"select access,name,way, 
    ROUND(ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s)))::numeric,3) as dist 
    from planet_osm_point where amenity ='parking' order by dist limit 1;")
    
    #Nearest parking from polygon
    parking_polygon <- fn$dbGetQuery(con,"select access,name,way, 
    ROUND(ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s)))::numeric,3) as dist 
    from planet_osm_polygon where amenity ='parking' order by dist limit 1;")
    
    #Nearest parking point or polygon  
    if (parking_point['dist'] < parking_polygon['dist'] ){
        parking <- parking_point
    }else parking <- parking_polygon
    
    #Parking distance score
    if (parking['dist'] < 100) {p_dist <- 10
    }else p_dist <-  ((-(parking['dist'])*0.01)+11)
    if (p_dist < 0 ){p_dist <-0}
    
    p_dist <- as.numeric(p_dist[[1]])
    
    #Parking access
    if (parking['access'] %in% c('yes','customers','public','permissive','delivery','barrier-free')){
        p_access <- 10
    }else if (parking['access'] %in% c('private','limited','emergency')) {p_access <- 6
    }else if (parking['access'] %in% c('no','restricted','blocked')) {p_access <- 1
    }else p_access <- 5
    
    
    #Roads
    #Total no of roads in last mile
    num_roads<- fn$dbGetQuery(con,"SELECT count(*)
        from  planet_osm_line
        where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
        and highway in ('official','designated','yes','path','secondary','tertiary',
                        'residential','track','motorway_link','service','motorway');")
    
    #Score for no of roads  
    if (num_roads > 1000) {near_roads <- 10
    }else near_roads <-  (num_roads/100)
    
    near_roads <- as.numeric(near_roads[[1]])
    
    #No. of barriers   
    barrier <- fn$dbGetQuery(con,"SELECT count(*) from  planet_osm_line
        where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
        and barrier is not null;")
    
    if (barrier < 150){barrier_score <- 10
    } else barrier_score<-(-0.006*barrier)+10.3
    if (barrier_score < 0 ){barrier_score <-0}
    
    barrier_score <- as.numeric(barrier_score[[1]])
    
    #No. of oneway roads   
    oneway <- fn$dbGetQuery(con,"SELECT count(*) from  planet_osm_line
        where (ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s))) <= 1600)
        and oneway = 'yes';")
    
    #Oneway Score
    if (oneway < 150){oneway_score <- 10
    } else oneway_score<-(-0.01*oneway)+11.5
    if (oneway_score < 0 ){oneway_score <-0} 
    
    oneway_score <- as.numeric(oneway_score[[1]])
    
    
    #Nearest Highway
    highway_line <- fn$dbGetQuery(con,"SELECT * ,
    ROUND(ST_Distance(way,(ST_SetSRID(ST_MakePoint($c),$s)))::numeric,3) as dist 
    from planet_osm_line 
    where highway in ('official','designated','yes','path','secondary','track',
                      'motorway_link','motorway')
  	order by dist limit 1;")
    
    #highway Score  
    if (highway_line['dist'] < 500) {highway <- 10
    }else highway <-  ((-(highway_line['dist'])*0.002)+11)
    if (highway < 0 ){highway <-0}
    
    
    highway <- as.numeric(highway[[1]])
    
    #Parking Score
    parking_score <- (0.5*p_dist) + (0.5*p_access)
    
    #Roads Score
    road_score <- as.numeric((0.5*highway) + (0.3*near_roads) + 
                                 (.05*barrier_score) + (0.15*oneway_score))
    
    #LML Score
    lml_score <- round((0.2*parking_score)+(0.2*road_score)+(0.15*access)+ 
                           (0.15*building)+(0.1*amenity)+(0.1*near_points)+(0.1*landuse),2)
    
    dbDisconnect(con)
    names(lml_score) <- "last mile score"
    details <- dest[!unlist(lapply(dest, is.na))]
    details <- details[ , !(names(details) %in% c("osm_id","way"))]
    
    target <- list(lml_score,details, parking['dist'], highway_line['dist'])
    return(target)
}


#Define UI for app  ----
ui <- fluidPage(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: visible; content: 'An error occurred. Please contact the admin.';}"
    ),
    # App title ----
    titlePanel("Last Mile Complexity Scorer"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the inputs ----
            textInput("lat","Enter latitude"),
            textInput("long","Enter longitude"),
            textInput("srid","Enter SRID")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            h3("The last mile score is"),
            h3(textOutput('score', inline = TRUE)),
            br(),
            paste("Nearest parking available in "),
            textOutput('park', inline = TRUE),
            br(),
            br(),
            paste("Distance from highway to the destination is "),
            textOutput('highway',inline = TRUE),
            br(),
            br(),
            paste( "About the destination"),
            tableOutput('details')
            
        )
    )
)  

#Defing the server function for the app
server <- shinyServer(function(input,output) {
    output$score <- renderText({
        paste(last_mile_score(paste(input$long,input$lat,sep =","),input$srid)[[1]] )
    })
    output$park <- renderText({
        paste(last_mile_score(paste(input$long,input$lat,sep =","),input$srid)[[3]], "metres")
    })
    output$highway <- renderText({
        paste(last_mile_score(paste(input$long,input$lat,sep =","),input$srid)[[4]], "metres" )
    })
    output$details <- renderTable({last_mile_score(paste(input$long,input$lat,sep =","),input$srid)[[2]]
    })
})

shinyApp(ui=ui,server=server)