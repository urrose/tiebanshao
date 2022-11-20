library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(readr)
library(RColorBrewer)
library(stringr)

map0<-st_read('City_of_Boston_Boundary.geojson')


info<-read_csv('tmp7_f32p54.csv')

info<-info %>% 
      select(DISTRICT,OFFENSE_CODE,OFFENSE_DESCRIPTION,SHOOTING,MONTH, DAY_OF_WEEK, HOUR,Long,Lat,STREET) %>%
      filter(Long!=0,Lat!=0) %>%
	  mutate(OFFENSE_DESCRIPTION=str_to_title(OFFENSE_DESCRIPTION))

basemap<-ggplot()+
        geom_sf(data=map0,colour='#0C090A',fill=NA)+
        theme_bw()
		
infoTYPE<-info %>% 
           group_by(OFFENSE_DESCRIPTION)	%>%
           summarise(count=n()) %>%
           arrange(desc(count)) %>%
           slice_head(n=10)
	

library(shinythemes)

ui <- 
  navbarPage("Boston Crimes", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             tabPanel("Map",
			 selectInput('whatoffense','Select top Offense',choices=infoTYPE$OFFENSE_DESCRIPTION),
			 fluidRow(plotOutput('offsenmapout',width=700,height=670))
			 ),
             tabPanel("Table",
			 radioButtons('bywhatT','Aggregate By:',choices=names(info)[0-c(2,8,9)],selected='MONTH'),
			 tableOutput('tab2Table')
			 ),
             tabPanel("Region",
			 tags$h3("Most dangerous street in a Boston's District!"),
			 fluidRow(
			 column(4,selectInput('whatDistrict','Select one District',choices=na.omit(unique(info$DISTRICT)),selected='B3')),
			 column(4,numericInput('topnstreet','top N:',value=20,min=5,max=30))
			 ),
			 radioButtons('shooter','Shoot?',choices=c('No&Yes','No','Yes'),inline=T),
			 tableOutput('streetTable')
			 ), 
			 
             tabPanel("About",
			 helpText('Data source:',tags$a('https://data.boston.gov/',href='https://data.boston.gov/',target='_blank')),
			 helpText('About dataset used in this illustration:')
			 )
  )

server <- function(input, output) {

################
output$offsenmapout<-renderPlot({
info2<-info %>% 
filter(OFFENSE_DESCRIPTION==input$whatoffense) %>%
group_by(Long,Lat) %>%
summarise(n=n())


totalN<-infoTYPE[match(input$whatoffense,infoTYPE$OFFENSE_DESCRIPTION),'count']

basemap+
geom_point(mapping=aes(x=Long,y=Lat,colour=n,size=n),data=info2,alpha=0.5,pch=20)+
scale_colour_gradientn(colours=c(rev(brewer.pal(5,'YlGn')),brewer.pal(5,'YlOrRd')))+
labs(x='',y='',title=paste0('Distribution of ',totalN,' "',input$whatoffense,'s" in Boston in 2021'))+
scale_size_continuous(range=c(0.6,6))

})


################
output$tab2Table<-renderTable({


outT<-info %>% 
     group_by_at(input$bywhatT) %>%
     summarise(count=n()) %>%
     arrange(desc(count))
	 
if('SHOOTING' %in% names(outT)) outT$SHOOTING<-as.character(outT$SHOOTING)	 
if('MONTH' %in% names(outT)) outT$MONTH<-as.character(outT$MONTH)	 
if('HOUR' %in% names(outT)) outT$HOUR<-as.character(outT$HOUR)	 

outT
})


####

output$streetTable<-renderTable({

shooterV<-c(0,1)
if(input$shooter=='No') shooterV<-0
if(input$shooter=='Yes') shooterV<-1

info3<-info %>% 
       filter(DISTRICT==input$whatDistrict,STREET!='NULL',SHOOTING %in% shooterV) %>%
	   group_by(STREET) %>%
	   summarise(count=n()) %>%
	   arrange(desc(count)) %>%
	   slice_head(n=input$topnstreet)
	   
	   

})

}

shinyApp(ui = ui, server = server)









