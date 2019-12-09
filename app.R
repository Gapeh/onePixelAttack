#Visual representation of the one pixel attack
#Created by Jordan Jones jordanjones2078@floridapoly.edu
#date 12/9/2019
#For: Scientific programming in R - final project


#librarys to inport
library(keras)
library(png)
library("colorspace")
library(imager)
library(plotly)
library(magrittr)
library(shinyBS)
#inport the cifar10 dataset and the neural network
cifar<-dataset_cifar10()

new_model <- load_model_hdf5("resnet.h5")

func <- function(fileName) {
    im<-readPNG(fileName) 
}
#shiny app
shinyApp(
  #UI code
  ui = fluidPage(
    tags$a(href="javascript:history.go(0)", 
           popify(tags$i(class="fa fa-refresh fa-5x"),
                  title = "Reload", 
                  content = "Click here to restart the Shiny session",
                  placement = "right")),
    #sidebar for the picture and pixel selection and colors
        sidebarLayout(
          #here are the selections for pictures and pixels
            sidebarPanel((position="left"),
              
              selectInput("picture", "Choose a picture:",
                          list(`PictureNumber` = (as.list(seq(from = 1, to = 10000, by = 1)))
                               
                          )
              ),
              selectInput("row", "Choose a pixel(Row):",
                          list(`Row` = (as.list(seq(from = 1, to = 32, by = 1)))
                          )
              ),
              selectInput("column", "Choose a pixel(Column):",
                          list(`Column` = (as.list(seq(from = 1, to = 32, by = 1)))
                          )
              )
            ),
          #slider for the main panel
            mainPanel(
                sliderInput("Red", "Red:",
                            min = 0, max = 1,
                            value = 0, step = 0.001),
                sliderInput("Green", "Green:",
                            min = 0, max = 1,
                            value = 0, step = 0.001),
                sliderInput("Blue", "Blue:",
                            min = 0, max = 1,
                            value = 0.5, step = 0.001),
                tableOutput("values"),
            ),
        ),
      #print the results of the neural network
      textOutput("result"),
      #a fluid row to display the pictures side by side
        fluidRow(
          column(5,imageOutput("imgOld")), 
          column(5,imageOutput("imgNew")),
        ), 
    #main panel left blank
    mainPanel(
    ),
    ),
  #server code for the shiny app
  server = function(input, output) {
    

        #here is the function to retrieve the slider values 
        slidervalues <- reactive({
            
            data.frame(
                Name = c("Red",
                         "Green",
                         "Blue"
                         ),
                Value = as.character(c(input$Red,
                                       input$Green,
                                       input$Blue))
                )
            
        })
        

            output$values <- renderTable({
            slidervalues()
        })

  
  #here is the function to recieve the input
  output$result <- renderText({
      
      #here will pull the test set of data
      test_x<-cifar$test$x/255
      #grab the row and column value
      x1<-input$row
      x2<-input$column
      #grab the picture value
      pictureNumb<- input$picture
      #convert both to numeric
      x1<-as.numeric(x1)
      x2<-as.numeric(x2)
      pictureNumb<-as.numeric(pictureNumb)
      
      #grab the slider values.    
      z<-slidervalues()
      #drop the levels
      redValue<-levels(droplevels(z[1,2]))
      blueValue<-levels(droplevels(z[2,2]))
      greenValue<-levels(droplevels(z[3,2]))
      
      #load the images
      im1 = test_x[pictureNumb,,,]
      im2 = test_x[pictureNumb,,,]
      
      #convert the color values to numeric values
      im2[x1,x2,1] = as.numeric(redValue)
      im2[x1,x2,2] = as.numeric(blueValue)
      im2[x1,x2,3] = as.numeric(greenValue)
      
      #write the images to the file
      writePNG(im1, target="imgOld.png")
      writePNG(im2, target="imgNew.png")
      
      #write the images to a value so the UI can display it
      output$imgOld <- renderImage({ 
        list(src = "imgOld.png", height = 240, width = 300)
        })
      
      output$imgNew <- renderImage({ 
        list(src = "imgNew.png", height = 240, width = 300)
        })
      #grab the pictures again for the neural network(need a vector so grave them again)
      im1Eval = test_x[c(pictureNumb,pictureNumb),,,]
      
      im2Eval = test_x[c(pictureNumb,pictureNumb),,,]
      
      #change the values of the pixels selected to the color values selected
      im2Eval[1,x1,x2,1] = as.numeric(redValue)
      im2Eval[1,x1,x2,2] = as.numeric(blueValue)
      im2Eval[1,x1,x2,3] = as.numeric(greenValue)
      im2Eval[2,x1,x2,1] = as.numeric(redValue)
      im2Eval[2,x1,x2,2] = as.numeric(blueValue)
      im2Eval[2,x1,x2,3] = as.numeric(greenValue)
      
      
      #evaluate the pictures in the neural network 
      out1<- new_model %>% evaluate(im1Eval, test_y[c(pictureNumb,pictureNumb),])
      out2<- new_model %>% evaluate(im2Eval, test_y[c(pictureNumb,pictureNumb),])
      
      #variable for output readability
      correct1 <-"False"
      correct2 <- "False"
      #set the variable(1 for true 0 for false) 
      if(out1[2] == "1"){
        correct1<-"True"
      }
      if(out2[2] == "1"){
        correct2<-"True"
      }
      #format and paste the neural network otuput so the UI can diplay it
      out1<-c("IMAGE 1: Correct classification?", correct1, "Loss =", out1[1],"                        ")
      out2<-c("IMAGE 2: Correct classification?", correct2, "Loss =", out2[1] )
      paste(c(out1,out2))

    })
  }
)



