
# FixedIncome App

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  headerPanel("Fixed Income Overview"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view. The helpText function is also used to 
  # include clarifying text. Most notably, the inclusion of a 
  # submitButton defers the rendering of output until the user 
  # explicitly clicks the button (rather than doing it immediately
  # when inputs change). This is useful if the computations required
  # to render output are inordinately time-consuming.
  
  
  sidebarPanel(
    
    #Displaying the CFRM and  GARP logo
    p(a(img(src="cfrm-logo.png", height = 40, width = 304), 
        target="_blank", href="http://depts.washington.edu/compfin/")),
    p(a(img(src="garp_logo_sm.gif", height = 40, width = 304), 
        target="_blank", href="http://www.garp.org")),
    
    tags$hr(),
    
    
    #Conditionalpanel function to change sidebarpanel according to tabs
    
    #UI for getting the Discount Factor
    conditionalPanel(condition="input.conditionedPanels==1",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("discount.factor_button", "Run!"),
                     tags$hr(),
                     
                     h5("Bond Prices"),
                     textInput("bondprice", "Enter the bond prices separated by comma (in $):" , 
                               value = "100.550,104.513,105.856"),
                     tags$hr(),
                     
                     h5("Coupon Rates"),
                     textInput("cr", "Enter the Coupon Rates for the bonds separated by comma (in %):" , 
                               value = "1.25,4.875,4.5"),
                     
                     tags$hr(),
                     h5("Time To Maturity"),
                     textInput("ttm", "Enter the time to maturities for the bonds separated by comma (in years):" , 
                               value = "0.5,1,1.5"),
                     tags$hr(),
                     h5("Significant Digits")
                     
    ),
    
    #UI for getting the Bond Price    
    conditionalPanel(condition="input.conditionedPanels==2",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("bond.price_button", "Run!"),
                     tags$hr(),
                     
                     h5("Current Date"),
                     dateInput("tn", "Enter Current Date:" , 
                               value = "2013-10-04"),
                     tags$hr(),
                     
                     h5("Previous Coupon Date"),
                     dateInput("t0", "Enter Previous Coupon Date:" , 
                               value = "2013-08-15"),
                     
                     tags$hr(),
                     h5("Next Coupon Date"),
                     dateInput("t1", "Enter Next Coupon Date:" , 
                               value = "2014-02-15"),
                     tags$hr(),
                     h5("Maturity Date"),
                     dateInput("tm", "Enter Maturity Date:" , 
                               value = "2017-08-15"),
                     tags$hr(),
                     h5("Coupon Rate"),
                     numericInput("bcr", "Enter Coupon Rate (In %):",4.75,step=0.1),                     
                     
                     tags$hr(),
                     h5("Yield"),
                     numericInput("yield", "Enter Yield:" , 
                                  value = 0.961,step=0.1),
                     tags$hr(),
                     h5("Significant Digits")                                          
    ),
    
    #UI for getting the Bond Parameters
    conditionalPanel(condition="input.conditionedPanels==3",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("present.value_button", "Run!"),
                     tags$hr(),
                     
                     h5("Coupon Rate"),
                     numericInput("pcr", "Enter Coupon Rate (In %):" , 
                                  value = 4.75),                     
                     tags$hr(),
                     h5("Discount Curve"),
                     textInput("df", "Enter Discount Factors separated by comma:" , 
                               value = "0.9680000, 0.9407242, 0.9031545, 0.8739803"),
                     tags$hr(),
                     
                     h5("Years to Maturity"),
                     numericInput("t", "Enter Years to Maturity:" , 
                               value = 2),
                     tags$hr(),
                     h5("Significant Digits")                                            
    ),
     #UI for getting the spot rates and discount curve 
      conditionalPanel(condition="input.conditionedPanels==4",
                     
                     h5("Enter inputs below and click 'Run!'"),
                     actionButton("spot.rate_button", "Run!"),
                     tags$hr(),
                     
                     selectInput("userip", "Select an Option:", 
                                 choices = c("Use Inbuilt Dataset"
                                             ,"Upload a Dataset"
                                             )),   
                     tags$hr(),
                     conditionalPanel(
                       condition = "input.userip == 'Use Inbuilt Dataset'",
                                              
                       selectInput("dataset", "Choose a dataset:", 
                                   choices = c("T-Notes",
                                               "T-Notes & Bonds"))
                                              
                       ),
                     
                     conditionalPanel(
                       condition = "input.userip == 'Upload a Dataset'",
                                         
                       selectInput("filetype", "Select File Type:", 
                                   choices = c("Excel", "Text", "CSV", "RData")
                                   ),   
                       tags$hr(),
                        conditionalPanel(
                         condition = "input.filetype== 'Excel' ",
                         fileInput('file.excel', 'Choose a file to upload'),                           
                         tags$hr(),
                         # checkbox for header
                         checkboxInput('header', 'Header', TRUE),
                         tags$hr(),
                         textInput("shname", "Enter SheetName if uploading a Excel File:" , 
                                 value = "sheet1")                           
                        ),
                      
                       conditionalPanel(
                         condition = "input.filetype== 'Text'",
                         fileInput('file.txt', 'Choose a file to upload'),                         
                         tags$hr(),
                         # checkbox for header
                         checkboxInput('header', 'Header', TRUE),
                         tags$hr(),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t',
                                        Space=' '),
                                      ',')
                       ),
                       
                      conditionalPanel(
                        condition = "input.filetype== 'RData' ",
                        fileInput('file.rdata', 'Choose a file to upload')
                       ),
                    
                      conditionalPanel(
                        condition = "input.filetype== 'CSV' ",
                        fileInput('file.csv', 'Choose a file to upload'),
                        
                        tags$hr(),
                        # checkbox for header
                        checkboxInput('header', 'Header', TRUE)
                      ),
                    
                      tags$hr(),
                      
                       p('If you want a sample files to upload,',
                         'you can first download the sample',
                         a(href = 'https://www.dropbox.com/sh/k95v1uesaeqd50q/AAARBVN9ZrXThz6eY0NWJOZSa?dl=0', 
                           'data'),'files, and then try uploading the file. '
                          ,'Please note that GARPFRM functions requires IssueDate, '
                          ,'MaturityDate, Coupon, Bid and Ask columns present in the dataset. Also, please note that the'
                         , 'coupon rate is entered in percentage.'
                       ) 
                     ),
                        tags$hr()
                ),#Conditional Panel 4
 
  conditionalPanel(condition="input.conditionedPanels!=5",
                 numericInput("digits", "Enter Number of significant digits required:" , 
             value = 3)
)

    
  ),#sidebarPanel
  
  #UI for displaying outputs
  mainPanel(
    tabsetPanel(
      tabPanel("Discount Factor", 
               verbatimTextOutput("discount.factor"),
               value=1), 
      tabPanel("Bond Pricing", 
               verbatimTextOutput("bond.price"),
               plotOutput("bond.price_plot"),               
               value=2),
      tabPanel("Bond Parameters", 
               verbatimTextOutput("bond.parameters"),
               value=3),      
      tabPanel("Data Tab - Spot & Forward Rates and Discount Curve", 
               verbatimTextOutput("spot.rate"),
               plotOutput("spot.rate_plot"),
               value=4),     
      tabPanel("About", 
               p(HTML("The application demonstrates functionality available in the GARP-FRM 
                   R package. The purpose of the this package is to implement the concepts 
                   and methods presented in the Global Association of Risk Professionals 
                      (GARP) Financial Risk Manager (FRM &reg) Part 1 series of books. 
                      Development of the GARP-FRM package is a collaborative project 
                      between the University of Washington Applied Mathematics Department 
                      MS-Degree Program in Computational Finance & Risk Management (MS-CFRM) 
                      and the Global Association of Risk Professionals to provide R 
                      computing applications that facilitate the learning of risk 
                      management concepts.")),
                value=5),     
       id = "conditionedPanels"
    )
  )
))