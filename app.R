library(shiny)
library(magick)
library(googledrive)
library(tidyverse)

## Define UI 
ui <- fluidPage(
	fluidRow(
		column(width = 4, class="well",

					 # fileInput('file1', 'Choose file to upload'#,
					 # 					#accept = c(
					 # 					#	'text/plain'
					 # 					#)
					 #),
					 uiOutput("file"),
					 plotOutput("plot1", height =300,
					 					 dblclick = "plot1_dblclick",
					 					 brush = brushOpts(
					 					 	id = "plot1_brush",
					 					 	resetOnNew = FALSE
					 					 )
					 ),
					 br(),
					 p("Click and drag on image to zoom; double click to reset"),
					 hr(),
					 fluidRow(
					 	column(width=2,offset=1,
					  	actionButton("reset","Reset")
					 	),
					 	column(width=2,offset=2,
					 				 actionButton("undo","Undo")
					 				 )
					  	)
					  	),
		
		column(width = 8,class="well",
					 plotOutput("plot2", height = 500,
					 					 click="plot_click"
					 )
					 
		)
		
	),
	
	fluidRow(
		column(width=12,offset = 1,
					 tableOutput("table")
		)
	)
)

options(shiny.maxRequestSize = 9*1024^2)
x<-read.csv("files.csv")
server <- function(input, output) {
	
	
	
	rv<-reactiveValues()
	
	ranges <- reactiveValues(x = NULL,
													 y = NULL) 
	coords <- reactiveValues(x = NULL, 
													 y = NULL)
	coords$df <- data.frame(x=numeric(0),y=numeric(0))
	
	
	
	# specimen <- reactive({
	# 	inFile <- input$file1
	# 	if (is.null(inFile)) {
	# 		# User has not uploaded a file yet
	# 		return(NULL)
	# 	}
	# 	image_read(inFile$datapath) %>%
	# 		image_resize("700")
	# })
	
	output$file<-renderUI({
		selectInput(inputId = "file",label="Select Image to Measure",
						    choices=x$name)
	})

	specimen <- reactive({
			inFile <- input$file
			 if (is.null(inFile)) {
			 	# User has not uploaded a file yet
			 	return(NULL)
			 }
			x[x$name==input$file,2]%>%
				as.character()%>%
				image_read()%>%
				image_resize("700")
		})
	
	output$plot1 <- renderPlot({
		par(mar=c(0,0,0,0))
		if(is.null(specimen())) return(NULL)
		plot(specimen())
		points(coords$df[,1], coords$df[,2],pch=19,col="red",cex=0.9)
	})
	
	output$plot2 <- renderPlot({
		par(mar=c(0,0,0,0))
		if(is.null(specimen())) return(NULL)
		if (is.null(ranges$x) | is.null(ranges$y)){
			plot(specimen())
		}
		else {plot(specimen(),
							 xlim=ranges$x,
							 ylim=ranges$y)
		}
		info<-image_info(specimen())
		points(coords$df[,1], coords$df[,2],pch=19,col="red",cex=1.5)
		abline(v = seq(0,info$width,40),col="grey")
		abline(h = seq(0,info$height,40), col="grey")
		})
	
	# When a brush happens, zoom to the brush bounds
	observeEvent(input$plot1_brush, {
		brush <- input$plot1_brush
		if (!is.null(brush)) {
			ranges$x <- c(brush$xmin, brush$xmax)
			ranges$y <- c(brush$ymin, brush$ymax)
		}
	})

		## If double click, reset the zoom.
	observeEvent(input$plot1_dblclick,{
		ranges$x <- NULL
		ranges$y <- NULL
	})
	
	## if click plot 2, gather coords of click
	observeEvent(input$plot_click, {
		click<-input$plot_click
		coords$df[nrow(coords$df)+1,]<-c(click$x,click$y)
	})
	
	observeEvent(input$reset,{
		coords$df<-data.frame(x=numeric(0),y=numeric(0))
	})
	
	observeEvent(input$undo,{
		n<-nrow(coords$df)
		coords$df<-coords$df[1:n-1,]
	})
	
	observeEvent(input$file,{
		if (!is.null(specimen()))
			coords$df<-data.frame(x=numeric(0),y=numeric(0))
	})
	output$table<-renderTable({coords$df %>%
															summarise(total=n())})
}
# Run the application 
shinyApp(ui = ui, server = server)
