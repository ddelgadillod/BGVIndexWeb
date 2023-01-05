#App derivada de BGVdash v1

library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(dplyr)
library(ggraph)
library(circlepackeR)
library(plotly)
library(ggplot2)
library(scales)
library(stringr)
library(circlepackeR)
library(data.tree)
library(treemap)


options(warn=-1)
suppressMessages(library(ggplot2)) 
options(browser="chrome")

######################################################################

seleccionCriterios <- read.csv("seleccionCriterios.csv", encoding="UTF-8", sep=",", stringsAsFactors=FALSE)

seleccionCriterios$categoria3[seleccionCriterios$categoria == "BGV List"] <- "BGVCOL"
seleccionCriterios$categoria3[seleccionCriterios$categoria != "BGV List"] <- "NCB"

seleccionCriterios$categoria <- seleccionCriterios$categoria3


seleccionCriterios$categoria2 <- NULL
seleccionCriterios$categoria3 <- NULL


dfp <- select(seleccionCriterios,id,grupo,nombre_cientifico,
              un_origen,un_region_biogeografica,
              origen_x_regiones,
              categoria,
              origen_et,
              vulnerabilidad_et,
              rendimiento,
              tipo_origen,clasificacion_FAO,
              int_banco,
              int_banco_et,
              valor_produccion_h, porc_municipios,promedio_p,
              prom_ac, prom_vp)

spOptions <- levels(as.factor(dfp$nombre_cientifico))


especie <- "NONE"

especies <- dplyr::select(dfp,categoria,nombre_cientifico)

#bgvEsp <- filter(especies, categoria == "BGV List")

#extEsp <- filter(especies, categoria == "External species")


#chcsEsp <- bgvEsp

dfreg <- read.csv2('Countries&Regions.csv', fileEncoding="latin1", stringsAsFactors = FALSE)






l <- list(color = toRGB("black"), width = 0.6)


g <- list(
    showframe = FALSE,
    showcoastlines = TRUE,
    projection = list(type = 'Mercator')
)

colScale <- c("#ffb3b3","#ffe7b3","#f1ffb3","#d4ffb3","#dab3ff","#ffb3d3","#7bff00")


######################################################################



dbHeader <- dashboardHeaderPlus(titleWidth = 220,enable_rightsidebar = FALSE)
dbHeader$children[[2]]$children <-  tags$a(href='https://www.agrosavia.co/',
                                           tags$img(src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/logoCorporativoColor2.svg',height='60',width='200'))



ui <- dashboardPagePlus(skin = "blue",
                        title='Agrosavia - BGV priorization Index',
                        dbHeader,
                        dashboardSidebar(
                            width = 220,
                            
                            sidebarMenu(
                                menuItem("Filter Options", icon = icon("filter")),
                                selectInput("selectCat",
                                            "   Select category: ",
                                            choices = c("BGVCOL" = 1,"NCB group" = 2),
                                            multiple = FALSE,
                                            selected = 1),
                                selectInput("selectEsp",
                                            "   Select species:",
                                            "placeholder1"),
                                
                                
                                menuItem("About this site", icon = icon("monument"),
                                         #menuSubItem("About BGV Priorization Index", tabName = "intro", icon = icon("journal-whills")),
                                         #menuSubItem("Intro", tabName = "background", icon = icon("journal-whills")),
                                         menuSubItem("Priorization Index", tabName = "how", icon = icon("gears"))
                                         #menuSubItem("Usage", tabName = "usage", icon = icon("readme"))
                                         ),
                                menuItem("Pillars", icon = icon("globe"),
                                         menuItem("Geographic origin", tabName = "geo_origin", icon = icon("map-marked-alt")),
                                         menuItem("Vulnerability", tabName = "vulnerability", icon = icon("exclamation-triangle")),
                                         menuItem("Economic Importance", tabName = "economic", icon = icon("search-dollar")),
                                         menuItem("Food security", tabName = "food_sec", icon = icon("utensils")),
                                         menuItem("Summary", tabName = "summary", icon = icon("compress"))
                                
                                         ),
                                #menuItem("BGVCOL priority index", tabName = "bgv_index", icon = icon("sort-amount-up-alt")),
                                #menuItem("Data", tabName = "data", icon = icon("table")),
                                menuItem("Data", tabName = "data2", icon = icon("table")),
                                menuItem("Usage", tabName = "usage2", icon = icon("question")),
                                menuItem("About", tabName = "about", icon = icon("address-card")),
								menuItem(actionButton(inputId='customeService', label="Customer service",icon = icon("volume-control-phone"), onclick ="window.open('https://www.agrosavia.co/atención-al-usuario', '_blank')"))
								#menuItem("Customer service", tabName = "customerService", icon = icon("volume-control-phone"))

                                
                            )
                        ),
#tags$link(rel = "stylesheet", type = "text/css", href = "http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/css/custom1.css")                        
                        dashboardBody(title='Prioritizing national plant genetic resources for investment in research using indicators about the geographic origin, vulnerability status, economic benefits, and food security importance',
                                      tags$head(
                                          tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")
                                      ),
                                      tabItems(
                                          # First tab content
                                          
                                          tabItem(tabName = "intro",
                                                  h2('Prioritizing national plant genetic resources for investment in research using indicators about the geographic origin, vulnerability status, economic benefits, and food security importance',style="text-align: center"),
                                                  tags$div(
                                                      HTML(paste0("Cerón-Souza, I.",tags$sup(1),
                                                                  ", Delgadillo, D.A.",tags$sup(1),
                                                                  ", Polo-Murcia, S.M.",tags$sup(1),
                                                                  ", Sarmiento-Naizaque, Z.X.",tags$sup(1),
                                                                  ", Reyes-Herrera, P.H.",tags$sup(1))),
                                                      style ="text-align: right"
                                                      #tags$span(style="text-align: right")
                                                           #tags$span(style="text-align: right")),
                                                      #tags$style()
                                                  ),
                                                  #h5("Cerón-Souza, I1., Delgadillo, D.A1., Polo-Murcia, S.M1, Sarmiento-Naizaque, Z.X1., Reyes-Herrera, P.H.1*",style="text-align: center"),
                                                  tags$div(
                                                      HTML(paste0(tags$sup(1),"Corporación Colombiana de Investigación Agropecuaria – AGROSAVIA.")),
                                                      style="text-align: right"
                                                  ),
                                                  #h5("1Corporación Colombiana de Investigación Agropecuaria – AGROSAVIA.",style="text-align: right"),
                                                  h6("C.I. Tibaitatá. Km 14 vía Bogotá Mosquera.",style="text-align: right"),
                                                  h6("Cundinamarca, Colombia.", style="text-align: right"),
                                                  tags$h6(
                                                      HTML(paste0("*Corresponding author: ", tags$a(href="mailto:phreyes@agrosavia.co", "phreyes@agrosavia.co"))),
                                                      style="text-align: right"
                                                  ),
                                                  #h6("*Corresponding author: phreyes@agrosavia.co", style="text-align: right"),
                                                  fluidPage(
                                                      p("National germplasm banks from developing countries are the most significant repository for plant genetic resources for food and agriculture (PGRFA). They store an agrobiodiversity base to solve food security in the face of global warming at the local and regional level. Despite their strategic importance, germplasm banks that depend on national funds in tropical developing countries have extremely limited funds to support scientific research. Therefore, it is indispensable to create a straight methodology that aligns policies to the bank's priorities and research investment. We propose a data-driven approach to build an index that integrates four pillars consistent with sustainable development goals, such as zero hunger, no poverty, and climate change. We retrieved the information in open-access databases for the geographic origin, the state of vulnerability, crop's economic importance for the country, and food security importance.", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                      
                                                      
                                                      p("This study focused on creating a prioritizing index using fuzzy logic to give values and rank 275 crop species and wild relatives preserved in the Colombian's plant germplasm bank. We classified and ranked all the 275 species in either high (8.72%), medium (26.18%), and low research priority (65.09%). Because of the absence of information for many native crops and wild relatives, we coupled the index with different confidence levels depending on whether we used direct data or extrapolated it from the closest species with information", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                      br(),
                                                      div(
                                                          img(height = 250,  src='https://www.agrosavia.co/media/3535/01.jpg', alt="www.agrosavia.co" , style="vertical-align:middle"),
                                                          style="text-align: center;"),
                                                      br(),
                                                      p("Our methodology had several advantages: the data used to construct the index came from open-access databases. We used a data-driven approach independent of the bank scientists' preferences and biases. The classification of species in three prioritizing categories simplified the information for non-scientific training stakeholders and politicians that usually decide how to invest research funds. Finally, the methodology identified the most significant information gaps for native crops and wild relatives conserved in the bank. We think our method is easy to implement in other tropical developing countries with national germplasm banks and PGRFA projects that face limited fund resources", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                      
                                                  ),
      
                                                  #tags$img(height = 400, width = 500,src = "intro.png"),
                                                  #circlepackeROutput("circlepackerPlot"),
                                                  #mainPanel(
                                                  #  includeHTML("circle.html")
                                                  #).
                                                  
                                                  #includeHTML("circle.html"),
                                                  #tags$img(height = 400, width = 500,src = "intro.png"),
                                                  
                                                  #tags$p('Cras nec erat eu nisi viverra semper. Vestibulum feugiat lobortis accumsan.
                                                  #           Nunc vitae nulla id ante blandit rutrum. Sed euismod efficitur velit, 
                                                  #           quis sollicitudin nisl egestas sed. Nunc feugiat ante et nulla venenatis gravida. 
                                                  #           Vestibulum ullamcorper eleifend lectus, sit amet fermentum augue aliquet ut. 
                                                  #           Vestibulum malesuada ex cursus tortor dignissim semper. Suspendisse urna ligula, 
                                                  #           vulputate sed magna quis, porttitor dapibus mauris. Pellentesque at iaculis massa.
                                                  #           Cras sed auctor ante, vel laoreet nisl. Mauris bibendum ac magna vel consectetur.')
                                          ),
                                          
                                          tabItem(tabName = "usage2",

                                                  carousel(
                                                      id = "mycarousel",
                                                      width = 12,
                                                      carouselItem(
                                                          caption = NULL,
                                                          h2("Main page & main menu"),
                                                          fluidRow(
                                                            div(
                                                              img(height = 500,
                                                                  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/main.png',
                                                                  NULL ,
                                                                  style="vertical-align:center"
                                                              ),
                                                              style="text-align: center;"
                                                            ), 

                                                             
                                                            fluidPage(
                                                              p("In the left bar you will find some options:", style = "text-align: justify; font-size:120%"),
                                                              h4("1. Filter options:"),
                                                              p("Two dropdown lists, you can filter in two ways: by category, you can choose BGVCOL or NCB group category, and by species, you can choose a species to see information about that.", style = "text-align: justify; font-size:100%"),
                                                              h4("2. Data visualization:"),
                                                              p("A set of 5 pages to visualize data in different ways.", style = "text-align: justify; font-size:120%"),
                                                              h4("3. Button to hide the left panel"),
                                                              #p("3. Button to hide the left panel", style = "text-align: justify; font-size:120%"),
                                                              
                                                            
                                                              br(),
                                                              br(),
                                                              
                                                              
                                                            ),
                                                            
 
                                                             
                                                          )
                                                    ),
                                                    
                                                    
                                                    carouselItem(
                                                          caption = NULL,
                                                          h2("Pillars"),
                                                          br(),
                                                          br(),
                                                            
                                                          fluidRow(

                                                            div(
                                                              img(height = 220,
                                                                  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/pillars.PNG',
                                                                  NULL ,
                                                                  style="vertical-align:center"
                                                              ),
                                                              style="text-align: left;"
                                                            ), 

                                                             
                                                            fluidPage(
                                                              br(),
                                                              p("Contain five pages, in that you can visualize data about the selected species, Geographic origin shows a map with highlighted origin region. Vulnerability shows the level of vulnerability of the selected species. Economic importance shows a set of plots of economic variables in time for the selected species. Food security shows plots related to the food security importance of the species. In Summary, you can see the most important information and the priority level of the selected species. ", style = "text-align: justify; font-size:120%"),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              br(),
                                                              
                                                              
                                                            ),
                                                            
 
                                                             
                                                          )
                                                    ),
                                                    

                                                    

                                                    
                                                    carouselItem(
                                                        caption = NULL,
                                                        h3("5. Data"),
                                                        fluidRow(
                                                          div(
                                                            img(height = 80,
                                                                src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/data1.PNG',
                                                                NULL ,
                                                                style="vertical-align:center"
                                                            ),

                                                            style="text-align: center;"
                                                          ), 
                                                          br(),
                                                          div(
                                                            img(height = 200,
                                                                src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/data2.PNG',
                                                                NULL ,
                                                                style="vertical-align:center"
                                                            ),
                                                          ),
                                                          
                                                          
                                                          fluidPage(
                                                            p("This page lets you see a table that contains information related to species based on the filter criteria: BGVCOL, NCB group, FAO category, and the priority level and data for all species can be downloaded with the button Download.", style = "text-align: justify; font-size:120%"),
                                                            #h4("1. Filter options:"),
                                                            #p("Two dropdown lists, you can filter in two ways: by category, you can choose BGVCOL or NCB group category, and by species, you can choose a species to see information about that.", style = "text-align: justify; font-size:100%"),
                                                            #h4("2. Data visualization:"),
                                                            #p("A set of 5 pages to visualize data in different ways.", style = "text-align: justify; font-size:120%"),
                                                            #h4("3. Button to hide the left panel"),
                                                            #p("3. Button to hide the left panel", style = "text-align: justify; font-size:120%"),
                                                            
                                                            
                                                          ),
                                                          
                                                          
                                                          
                                                        )
                                                    )
                                                
                                                  )
                                                    

                                          ),                                          
                                          
                                          
                                          tabItem(tabName = "how",
                                                  h1("Priority index based on fuzzy logic"),
                                                  
                                                                                                     
                                                    
                                                  p("This website shows the information on the species on Colombian Vegetal Germplasm Bank (BGVCOL) and external species (NCB) retrieved from open-access databases in four pillars: the geographic origin, the state of vulnerability, crop's economic importance for the country, food security importance, and the results of the prioritization index using a data-driven approach with fuzzy logic. Users can select a species and navigate over different options in the left bar to visualize in interactive ways related data to species, a summary of the results for each species, and download data used in this study.  ", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                  br(),
                                                  div(
                                                      img(height = 250,  src='https://www.agrosavia.co/media/3535/01.jpg', alt="www.agrosavia.co" , style="vertical-align:middle"),
                                                      style="text-align: center;"),
                                                  br(),
                                                  
                                                      
                                                  
                                                  
                                                  p("We asked how to define a list of priorities species for two of our primary pre-breeding responsibilities in the Colombian germplasm banks, which are:", style = "font-family: 'arial'; font-size:100%"),
                                                  p("1. What species must conserve within the bank", style = "font-family: 'arial'; font-size:100%"),
                                                  p("2. What species we should invest money in scientific research.", style = "font-family: 'arial'; font-size:100%"), 
                                                  p("We proposed four pillars to identify priorities for the Germplasm Bank consistent with FAO sustainable development goals such as zero hunger, no poverty, and climate action. They represent both the country's opportunities and responsibilities.",style = "font-family: 'arial'; font-size:100%"),
                                                  div(
                                                    tags$img(height = 400, 
                                                             width = 600,src = "http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/mind_map_pillars.svg"),
                                                  style="text-align: center;"),
                                                  p("The general strategy for analyzing raw data of 345 Plant Genetic Resources for Food and Agriculture (PGRFA) from Colombia."),
                                                  
                                                  p("Two hundred seventy-five (275) are part of the National Plant Germplasm Bank (i.e., BGVCOL group), and 70 are essential for the Colombia government but not currently conserved in the BGVCOL (i.e., NCB group)."),
                                                  p("i. The analysis includes four data-driving pillars with diferent variables."),
                                                  p("ii. The preprocessing of the raw data for each variable within each pillar: geographic origin (green), vulnerability (yellow), economic importance (red), and food security importance (blue). Both economic importance and food security importance had several holes of information for some PGRFA. In those cases, the preprocessing included values from the phylogenetically closest PGRFA to impute and, therefore, the uncertainty calculation (purple box)."), 
                                                  p("iii. The construction of the variables membership function (fuzzy sets) is based on either singleton or trapezoid. The geographic origin and vulnerability involved a unique qualitative variable that generated a singleton fuzzy logic function. Economic and food security importance had four and ten quantitative variables, generating trapezoid fuzzy logic functions."),
                                                  p("iv. The final inference for the prioritization list and uncertainty level is based on Gaussian and singleton fuzzy logic functions."),
                                                  div(
                                                      tags$img(height = 600, 
                                                               src = "http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/diag-fl-v3.svg"),
                                                      style="text-align: center;"),
                                                  #tags$p('Cras nec erat eu nisi viverra semper. Vestibulum feugiat lobortis accumsan.
                                                  #       Nunc vitae nulla id ante blandit rutrum. Sed euismod efficitur velit, 
                                                  #       quis sollicitudin nisl egestas sed. Nunc feugiat ante et nulla venenatis gravida. 
                                                  #       Vestibulum ullamcorper eleifend lectus, sit amet fermentum augue aliquet ut. 
                                                  #       Vestibulum malesuada ex cursus tortor dignissim semper. Suspendisse urna ligula, 
                                                  #       vulputate sed magna quis, porttitor dapibus mauris. Pellentesque at iaculis massa.
                                                  #       Cras sed auctor ante, vel laoreet nisl. Mauris bibendum ac magna vel consectetur.')
                                          ),
                                          tabItem(tabName = "geo_origin",
                                                  h1("Geographic origin"),


                                                  p("The geographical origin information of 275 species was processed following the classification of crop diversity regions of Khoury et al. (2016) and the POWO database (POWO, 2019). Species were organized into 26 regions, we grouped the 26 regions in three classes (fuzzy sets): local, close, and distant.",style = "font-family: 'arial'; font-size:120%"),
                                                  div(
                                                      textOutput("chSpeciesText"),
                                                      align="right",
                                                      tags$head(tags$style("#chSpeciesText{
                                                                            color: black;
                                                                            font-size: 12px;
                                                                            font-style: italic;
                                                                        }"
                                                      )
                                                      )
                                                  ),
                                                  fluidPage(
                                                      infoBoxOutput("originBox2"),
                                                      #align = "right",
                                                      
                                                  ),
                                                  fluidPage(
                                                      plotlyOutput("origEsp",
                                                                   width = '900px',
                                                                   height = "500px",),
                                                      align="center"
                                                      
                                                  ),
                                                               #verbatimTextOutput("summary") 
                                          ),
                                          tabItem(tabName = "vulnerability",
                                                  h1("Vulnerability"),
                                                  p("",style = "font-family: 'arial'; font-size:120%"),
                                                  
                                                  p("We classified the vulnerability of 275 species following the databases of Bernal et al. (2019), the Botanical Garden for International Conservation (BGCI, 2019) and Red List platforms (Red List, 2019). We merged the national and the international vulnerability categories into three: Not assessed, Least concern, and Threatened.",style = "font-family: 'arial'; font-size:120%"),
                                                  div(
                                                      textOutput("chSpeciesText2"),
                                                      align="right",
                                                      tags$head(tags$style("#chSpeciesText2{
                                                                            color: black;
                                                                            font-size: 12px;
                                                                            font-style: italic;
                                                                        }"
                                                      )
                                                      )
                                                  ),
                                                  fluidPage(
                                                      infoBoxOutput("vulBox2"),
                                                      #align = "right",
                                                      
                                                  ),
                                                  
                                                  fluidRow(#infoBoxOutput("estUNEsp"),
                                                      #infoBoxOutput("estBGRLEsp"),
                                                      infoBoxOutput("VUL"),
                                                  ),
                                                  # fluidRow(
                                                  #     # A static infoBox
                                                  #     infoBox("Endangered", 'Red List', icon = icon("skull-crossbones"), color = 'red'),
                                                  #     # Dynamic infoBoxes
                                                  #     #infoBoxOutput("progressBox"),
                                                  #     #infoBoxOutput("approvalBox")
                                                  # ),
                                                  # fluidRow(
                                                  #     infoBox("Vulnerable", 'UN List', icon = icon("exclamation-triangle"), fill = TRUE, color = 'orange'),
                                                  #     #infoBoxOutput("progressBox2"),
                                                  #     #infoBoxOutput("approvalBox2")
                                                  # ),
                                                  # fluidRow(
                                                  #     infoBox("Least concern", 'UN List', icon = icon("seedling"), fill = TRUE, color = 'green'),
                                                  #     #infoBoxOutput("progressBox2"),
                                                  #     #infoBoxOutput("approvalBox2")
                                                  # ),
                                                  # 
                                                  
                                          ),
                                          tabItem(tabName = "economic",
                                                  h1("Economic Importance"),
                                                  #p("En Colombia la frontera agricola nacional es de 40075960 de hectareas, la agricultura representa el XX por ciento del empleo a tiempo completo, el 6 por ciento del PIB y el 27 por ciento de los ingresos totales de exportacion.",style = "font-family: 'arial'; font-size:120%"),
                                                  #p("Los cambios en el clima, las condiciones de produccion y el gusto de los consumidores determinan las dinamicas de nuestros cultivos alimentarios.",style = "font-family: 'arial'; font-size:120%"),
                                                  #p("Para capturar la importancia economica de las especies vegetales del BGVCOL, caracterizamos cada una de las 275 especies en terminos de: yield, municipality coverage, Lafay index, income. ",style = "font-family: 'arial'; font-size:120%"),
                                                  p("",style = "font-family: 'arial'; font-size:120%"),
                                                  div(
                                                      textOutput("chSpeciesText3"),
                                                      align="right",
                                                      tags$head(tags$style("#chSpeciesText3{
                                                                            color: black;
                                                                            font-size: 12px;
                                                                            font-style: italic;
                                                                        }"
                                                      )
                                                      )
                                                  ),                                                  
                                                  
                                                  fluidPage(
                                                      infoBoxOutput("ecoBox2"),
                                                      #align = "right",
                                                      
                                                  ),
                                                  
                                                  
                                                  h3("Production"),
                                                  fluidRow(box(plotlyOutput("prodEsp"),)),
                                                  h3("Harvested area"),
                                                  fluidRow(box(plotlyOutput("acEsp"),)),
                                                  h3("Production value"),
                                                  fluidRow(box(plotlyOutput("vpEsp"),)),
                                                  #fluidRow(box(textOutput("estUNEsp")),
                                                  #h1(textOutput("vpEspEt"), align = "center"),
                                                  h3("Yield"),
                                                  p("The yield (t ha -1) represents agriculture productivity per area measured as Ri=Pri/Ai. Where Ri is the Yield of the species i (in t ha -1), Pri is the average annual production in 10 years (2007-2017) of the species i (in t), and Ai is the harvested average area for the species i in 10 years (2007-2017) in ha in Colombia.",style = "font-family: 'arial'; font-size:120%"),
                                                  #fluidRow(box(textOutput("estUNEsp")),
                                                  #h1(textOutput("rendEsp"), align = "center"),
                                                  h3("Municipality Coverage"),
                                                  p("The municipality coverage (%) is the percentage of Colombian municipalities that cultivate a specific species. This coverage describes both the concentration and adaptability of each species at a regional level. A value closest to 100% indicates that the species is essential for agriculture production and, therefore, necessary for the farmers' incomes.  ",style = "font-family: 'arial'; font-size:120%"),
                                                  #fluidRow(box(textOutput("covEsp")),
                                                  #h1(textOutput("covEsp"), align = "center"),
                                                  h3("Lafay Index"),
                                                  p("Lafay index (LFI), explaining the competitive strength of individual products or product groups [1]. The LFI positive value shows that the country possesses a comparative advantage while the negative LFI value shows that the country possesses a comparative disadvantage. LFI is result of quotient between the production of a crop and its apparent consumption (i.e., production plus import minus export) in a year. If this value is higher than one, the country is a net exporter of the crop, and the higher the level, the more important are the exports as a destination for the domestic production of the crop.",style = "font-family: 'arial'; font-size:120%"),
                                                  #fluidRow(box(textOutput("indLafEsp")),
                                                  #h1(textOutput("indLafEsp"), align = "center"),
                                                  h3("Income per hectare"),
                                                  p("Increasing farmers' productivity and income is a fundamental aspect of reducing rural poverty and ensuring food security. In this study, we chose the income per unit area as a proxy for each plant species' economic efficiency at the aggregate level within Colombia.",style = "font-family: 'arial'; font-size:120%"),
                                                  h3("Economic value"),
                                                  #fluidRow(box(textOutput("estUNEsp")),
                                                  #h1(textOutput("impEspEt"), align = "center"),
                                                  #fluidRow(box(plotlyOutput("impEsp"),)),
                                                  #plotlyOutput("impEsp"),
                                                  #fluidRow(box(textOutput("Specie")),
                                                  #h4(textOutput("Specie"), align = "center"),
                                                  tags$img(height = 400, width = 600,src = "http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/compact_economic.jpg"),
                                                  br(),
                                                  
                                                  h5("References"),
                                                  p("[1] Lafay, G. The Measurement of Revealed Comparative Advantages. In International Trade Modelling; Springer: New York, NY, USA, 1992.",style = "font-family: 'arial'; font-size:90%"),
                                          ),
                                          tabItem(tabName = "food_sec",
                                                  h1("Food security"),
                                                  div(
                                                      textOutput("chSpeciesText4"),
                                                      align="right",
                                                      tags$head(tags$style("#chSpeciesText4{
                                                                            color: black;
                                                                            font-size: 12px;
                                                                            font-style: italic;
                                                                        }"
                                                      )
                                                      )
                                                  ),
                                                  
                                                  fluidPage(
                                                      infoBoxOutput("segaBox2"),
                                                      #align = "right",
                                                      
                                                  ),
                                                  
                                                  p("The second Sustainable Development Goal (SDA) challenges the world to achieve food security and improve nutrition by 2030. Consequently, we adopted a multidimensional approach to evaluate four attributes associated with food security for each one of the 344 species of this study. They are government priority list, traditional consumption, nutritional contribution, and affordability based on nutrients.",
                                                    style = "font-family: 'arial'; font-size:100%"),
                                                  
                                                  
                                                  fluidPage(
                                                      h3("Government priority list"),
                                                      p("The Colombian government's list of prioritized plant genetic resources for food and agriculture (PGRFA) and animal products to guarantee their production and supply policies to improve their stable consumption in the Colombian population diet. A unique icon represents each species or product, showing the separation by FAO classification of food groups."),
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/priority_list.jpg', style="vertical-align:middle"),
                                                    ),
                                                  
                                                  fluidPage(
                                                      h3("Traditional consumption"),
                                                      p("The Colombian map shows 11 geographic regions with different levels of grey. Within each region is a pie with the number of Plant Genetic Resources for Food and Agriculture (PGRFA) essential for each region's food tradition, separating them by different colors representing eight FAO food groups. The size of the pie is equivalent to the number of PGRFA included."),
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/trad-consumption3.png', style="vertical-align:middle"),
                                                  ),
                                                  
                                                  fluidPage(
                                                      h3("Nutritional contribution"),
                                                      p("Colombia has made significant economic and social progress in recent decades. Although its current classification is an upper-middle-income country, there are still considerable challenges in achieving a convergence towards higher living standards. In this sense, micronutrient deficiencies continue to prevail among children under five years of age and contribute to the deterioration of child development and the increase in the national disease burden. We evaluated the 275 species based on their contribution to the daily nutrient intake requirements for this pillar considering the local crops to guarantee three deficient micronutrients' dietary sufficiency in the country's child population."),
                                                      br(),
                                                      p("Boxplots indicate the median and variation of the percentage (%) of the daily nutritional target for (A) Calcium Ca,(B) Iron Fe, (C) Zinc Zn, and Energy, by FAO food categories indicates the number of species (n)."),
                                                      h4("PGRFA conserved in BGVCOL n = 275"),
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/nut-bgv.png', style="vertical-align:middle"),
                                                      br(),
                                                      h4("PGRFA not conserved in BGVCOL (i.e., NCB group) n = 70"),
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/nut-pgrfa.png', style="vertical-align:middle"),
                                                  ),
                                                  
                                                  fluidPage(
                                                      h3("Affordability based on its nutrients"),
                                                      p("In order to identify affordability of the 275 species in monetary terms and locally appropriate to fill gaps in micronutrient intake, we calculated the nutrient-price ratio as the consumer price for every 100 grams of an edible portion in four variables: by a unit of energy (kcal) and by a unit of micronutrient of calcium, iron, zinc. Using this value, we identify the species competitivity from the nutritional and market standpoint."),
                                                      br(),
                                                      p("Boxplots indicate the median and variation of the affordability in USD per 100 g edible portion for (A) Calcium Ca,(B) Iron Fe, (C) Zinc Zn, and Energy, by FAO food categories indicates the number of species (n). *1 USD = 3418 COP average annual over the last five years ."),
                                                      h4("PGRFA conserved in BGVCOL n = 275"),
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/affor-bgv.png', style="vertical-align:middle"),
                                                      br(),
                                                      h4("PGRFA not conserved in BGVCOL (i.e., NCB group) n = 70"),
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/affor-pgrfa.png', style="vertical-align:middle"),
                                                      
                                                      
                                                      
                                                  ),
                                                  

                                          ),
                                          
                                          tabItem(tabName = "summary",
                                                  h1("Summary"),
                                                  h3(
                                                      textOutput("chSpeciesText5"),
                                                      align="left",
                                                      
                                                      tags$head(tags$style("#chSpeciesText5{
                                                                            font-style: italic;
                                                                        }"
                                                      )
                                                      )                                                      

                                                  ),
                                                  fluidRow(
                                                      fluidPage(
                                                          infoBoxOutput("originBox"),
                                                      ),
                                                      fluidPage(
                                                          infoBoxOutput("ecoBox"),
                                                      ),
                                                      fluidPage(
                                                          infoBoxOutput("vulBox"),
                                                      ),
                                                      fluidPage(
                                                          infoBoxOutput("segaBox")
                                                      ),
                                                  ),
                                                  
                                                  fluidRow(
                                                      fluidPage(
                                                          h3("BGVCOL Priority Index result"),
                                                          infoBoxOutput("intBGV"),
                                                          #infoBoxOutput("segaBox3"),
                                                          textOutput("chPriority"),
                                                      )
                                                  ),

                                          ),
                                          
                                          tabItem(tabName = "bgv_index",
                                                  h1("BGV Priority Index"),
                                                  h3(
                                                      textOutput("chSpeciesText6"),
                                                      align="left",
                                                      
                                                      tags$head(tags$style("#chSpeciesText5{
                                                                            font-style: italic;
                                                                        }"
                                                      )
                                                      )                                                      
                                                      
                                                  ),
                                                  fluidPage(
                                                      infoBoxOutput("intBGV2")
                                                  ),
                                                  
                                                  fluidPage(
                                                      img(height = 400,  src='http://ec2-54-227-84-137.compute-1.amazonaws.com/bgv/img/prior-boxes.png', style="vertical-align:middle"),
                                                  )
                                          
                                          ),
                                          
                                          
                                          tabItem(tabName = "data2",
                                                  h1("Available data"),
                                                  fluidPage(
                                                    column(2,
                                                           selectInput("selectInt",
                                                                       "   Select priority:",
                                                                       choices = c("ALL", "Low", "Medium", "High"),
                                                                       multiple = FALSE,
                                                                       selected = "ALL"),
                                                    ),
                                                    column(3,
                                                           selectInput("selectCat2",
                                                                       "   Select category: ",
                                                                       choices = c("ALL"= 1, "BGVCOL" = 2,"NCB group" = 3),
                                                                       multiple = FALSE,
                                                                       selected = 1),

                                                    ),
                                                    column(7,
                                                           selectInput("selectFAO",
                                                                       "   Select FAO group:",
                                                                       choices = c("ALL", as.character(sort(unique(seleccionCriterios$clasificacion_FAO)))),
                                                                       multiple = FALSE,
                                                                       selected = "ALL"),
                                                      
                                                    ),

                                                  ),
                                                  fluidPage(
                                                    downloadButton("downloadData", "Download"),
                                                    br(),
                                                  ),
                                                  
                                                  
                                                  
                                                  fluidPage(
                                                      dataTableOutput("datatable",
                                                                      ),
                                                      style = "overflow-y: scroll;overflow-x: scroll;"
                                                  ),

                                                           

                                          ),
                                          # tabItem(tabName = "data2",
                                          #         h1("Category data"),
                                          #         
                                          #         selectInput("selectCat2",
                                          #                     NULL,
                                          #                     choices = c("BGVCOL","NCB group"),
                                          #                     multiple = FALSE,
                                          #                     selected = 1),
                                          #         selectInput("selectCat",
                                          #                     "   Select category: ",
                                          #                     choices = c("BGVCOL" = 1,"NCB group" = 2),
                                          #                     multiple = FALSE,
                                          #                     selected = 1),
                                          #         selectInput("selectEsp",
                                          #                     "   Select species:",
                                          #                     "placeholder1"),
                                          #         
                                          #         fluidRow(box(dataTableOutput("datatable2"),
                                          #                      style = "width:1100px; overflow-y: scroll;overflow-x: scroll;"
                                          #                      
                                          #         )
                                          #         
                                          #         ),
                                          # ),                                          
                                          tabItem(tabName = "about",
                                                  h1("About us"),
                                                  h3('Prioritizing national plant genetic resources for investment in research using indicators about the geographic origin, vulnerability status, economic benefits, and food security importance',style="text-align: center"),
                                                  tags$div(
                                                      HTML(paste0("Cerón-Souza, I.",tags$sup(1),
                                                                  ", Delgadillo, D.A.",tags$sup(1),
                                                                  ", Polo-Murcia, S.M.",tags$sup(1),
                                                                  ", Sarmiento-Naizaque, Z.X.",tags$sup(1),
                                                                  ", Reyes-Herrera, P.H.",tags$sup(1))),
                                                      style ="text-align: center"
                                                      #tags$span(style="text-align: right")
                                                      #tags$span(style="text-align: right")),
                                                      #tags$style()
                                                  ),
                                                  #h5("Cerón-Souza, I1., Delgadillo, D.A1., Polo-Murcia, S.M1, Sarmiento-Naizaque, Z.X1., Reyes-Herrera, P.H.1*",style="text-align: center"),
                                                  tags$div(
                                                      HTML(paste0(tags$sup(1),"Corporación Colombiana de Investigación Agropecuaria – AGROSAVIA.")),
                                                      style="text-align: center"
                                                  ),
                                                  #h5("1Corporación Colombiana de Investigación Agropecuaria – AGROSAVIA.",style="text-align: right"),
                                                  h5("C.I. Tibaitatá. Km 14 vía Bogotá Mosquera.",style="text-align: center"),
                                                  h5("Cundinamarca, Colombia.", style="text-align: center"),
                                                  tags$h5(
                                                      HTML(paste0("*Corresponding author: ", tags$a(href="mailto:phreyes@agrosavia.co", "phreyes@agrosavia.co"))),
                                                      style="text-align: center"
                                                  )
                                          )
                                          
                                      )
                        )
)

server <- function(input, output,session) {
    cat3 <- reactive ({
        as.numeric(input$selectCat2)
    })
    # dataset <- reactive({
    #     if(input$selectCat == "BGV List"){
    #         get(filter(especies, categoria == "BGV List"))
    #     } else{
    #         get(filter(especies, categoria == "External species"))
    #     }
    # })
    
    observe({
        
        
        # if(input$selectCat == "BGV List"){
        #     data <- filter(especies, categoria == "BGV List")
        # } else{
        #     data <- filter(especies, categoria == "External species")
        # }
        # print(input$selectCat)
        # print(head(data))
    })
    #observe({
        #x <- as.numeric(input$selectCat)
        #print(x)
        #chcsEsp <- input$selectEsp
        

        #bgvEsp <- filter(especies, categoria == "BGV List")
        
        #extEsp <- filter(especies, categoria == "External species")
        #print(head(data))
        #print(chcsEsp)
        
        # updateSelectInput(session,
        #                   "selectEsp",
        #                   choices = chcsEsp 
        # )
    #})
    
    # observeEvent(input$selectCat, {
    #     speciesByCat <- as.character(sort(unique(
    #         dataset$nombre_cientifico
    #     )))
    #     updateSelectInput(session, "selectEsp", choices = speciesByCat)
    # })
    observeEvent(input$selectCat, {
        print(input$selectCat)
        print(class(input$selectCat))
        dfSel <- filter(especies, categoria == "BGV List")
        if(as.numeric(input$selectCat) == 1){
            print("BGV")
            dfSel <- filter(especies, categoria == "BGVCOL")
        } else{
            print("Externa")
            dfSel <- filter(especies, categoria == "NCB")
        }
        #dfSel <- filter(especies, categoria == input$selectCat)
        print(head(dfSel))
        column_levels <- as.character(sort(unique(dfSel$nombre_cientifico)))
        print(length(column_levels))
        print(class(column_levels))
        updateSelectInput(session, "selectEsp", choices = column_levels)
        
    })


    
    output$chSpeciesText <- renderText({ 
        
        paste('Species: ', input$selectEsp)
        })
    output$chSpeciesText2 <- renderText({ 
        
        paste('Species: ', input$selectEsp)
    })
    output$chSpeciesText3 <- renderText({ 
        
        paste('Species: ', input$selectEsp)
    })
    output$chSpeciesText4 <- renderText({ 
        
        paste('Species: ', input$selectEsp)
    })
    
    

    
    output$origEsp <- renderPlotly({
        # x <- as.numeric(input$selectCat)
        # print(x)
        # 
        # if(x == 1){
        #     chcsEsp <- bgvEsp
        # } else if (x == 2){
        #     chcsEsp <- extEsp
        # }
        # print(chcsEsp)
        dff <- select(seleccionCriterios, nombre_cientifico,origen_x_regiones)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        #
        inputRegEsp <- dff$origen_x_regiones[1]
        #print(inputRegEsp)
        
        df <- dfreg
        df$fillby2 <- as.numeric(df$fillby2)
        df$fillby2[df$Region1 == inputRegEsp |df$Region2 == inputRegEsp | df$Region3 == inputRegEsp] <- 20
        
        
        titleEsp <- paste('Species Origin', input$selectEsp)
        
        plot_geo(df) %>%
            add_trace(
                z = ~fillby2, 
                color = ~fillby2, 
                text = ~paste(Country,Region1,sep = "<br />"),
                hoverinfo = "text",
                colors = 'Greens',
                #text = ~Country, 
                locations = ~Code, 
                marker = list(line = l, show = FALSE),
                opacity = 0.9
                #)
            ) %>%
            hide_colorbar()  %>% 
            #colorbar(showticklabels = FALSE) %>%
            layout(
                #title = titleEsp,
                geo = g
            ) %>%
            config(displayModeBar = F)
        
        
    })  
    output$estUNEsp <- renderText({ 
        dff <- select(seleccionCriterios, nombre_cientifico, un_estado_conservacion)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$un_estado_conservacion[1])){
            paste("Not available")
            dff$un_estado_conservacion[1]
        }else{
            paste(dff$un_estado_conservacion[1])
            print(dff$un_estado_conservacion[1])
        }
        
    })
    
    output$estBGRLEsp <- renderText({ 
        dff <- select(seleccionCriterios, nombre_cientifico, est_conservacion_BGRL)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$est_conservacion_BGRL[1])){
            paste("Not available")
            print(dff$uest_conservacion_BGRL[1])
        }else{
            paste(dff$est_conservacion_BGRL[1])
            print(dff$uest_conservacion_BGRL[1])
        }
    })
    
    output$prodEsp <- renderPlotly({
        dff <- select(seleccionCriterios, nombre_cientifico, P1,P2,P3,P4,P5,
                      P6,P7,P8,P9,P1,P10,P11,promedio_p)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        xprod <- seq(2008,2018)
        yprod <- as.vector(dff[1,2:12])
        yprod <- as.numeric(yprod)
        br()
        br()
        plot_ly(x = xprod, y = yprod, mode = 'scatter', color = I("green")) %>% 
            layout(title = "Production 2008-2018",
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Production (tonnes)')) %>% 
            config(displayModeBar = F)
    })
    
    output$acEsp <- renderPlotly({
        dff <- select(seleccionCriterios, nombre_cientifico, AC1,AC2,AC3,AC4,AC5,
                      AC6,AC7,AC8,AC9,AC1,AC10,AC11,prom_ac)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        xac <- seq(2008,2018)
        yac <- as.vector(dff[1,2:12])
        yac <- as.numeric(yac)
        br()
        plot_ly(x = xac, y = yac, mode = 'scatter', color = I("red")) %>% 
            layout(title = "Harvested area 2008-2018", 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Area (Hectares)')) %>% 
            config(displayModeBar = F)
    })  
    
    output$vpEsp <- renderPlotly({
        dff <- select(seleccionCriterios, nombre_cientifico, VP1,VP2,VP3,VP4,VP5,
                      VP6,VP7,VP8,VP9,VP1,VP10,prom_vp,rendimiento,valor_produccion_h)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        xvp <- seq(2008,2018)
        yvp <- as.vector(dff[1,2:12])
        yvp <- as.numeric(yvp)
        br()
        plot_ly(x = xvp, y = yvp, mode = 'scatter', color = I("blue")) %>% 
            layout(title = "Production value 2008-2018", 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'million COP/ha')) %>% 
            config(displayModeBar = F)
    })  
    
    output$vpEspEt <- renderText({ 
        dff <- select(seleccionCriterios, nombre_cientifico, valor_produccion_h)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$valor_produccion_h[1])){
            paste("Not available")
        }else{
            paste("$",dff$valor_produccion_h[1])
        }
    })
    
    output$rendEsp <- renderText({ 
        dff <- select(seleccionCriterios, nombre_cientifico, rendimiento)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$rendimiento[1])){
            paste("Not available")
        }else{
            paste(dff$rendimiento[1]," T/h")
        }
        
    })
    
    
    output$covEsp <- renderText({ 
        dff <- select(seleccionCriterios, nombre_cientifico, porc_municipios)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$porc_municipios[1])){
            paste("Not available")
        }else{
            paste(dff$porc_municipios[1],"% from municipalities")
        }
    })
    
    output$indLafEsp <- renderText({ 
        dff <- select(seleccionCriterios, nombre_cientifico, ind_lafay)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$ind_lafay[1])){
            paste("Not available")
        }else{
            paste(dff$ind_lafay[1])
        }
    })  
    
    ###############VULNERABILIDAD#################################  
    
    
    output$estUNEsp <- renderText({ 
        print(paste0("VUUL ", input$selectEsp))
        #dff <- select(seleccionCriterios, nombre_cientifico, un_estado_conservacion)
        #ff <- filter(dff, nombre_cientifico == input$selectEsp)
        #if(is.na(dff$un_estado_conservacion[1])){
        #    paste("Not available")
        #}else{
        #    paste(dff$un_estado_conservacion[1])
        #}
        #print(paste0("VUUL ", dff$un_estado_conservacion[1]))
        
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
            color = "purple"
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    output$circlepackerPlot <- circlepackeR::renderCirclepackeR({
        
        species_paths <- read.csv("species_paths.csv", sep=";")
        
        
        species_paths$pathString <- paste("BGV", 
                                          species_paths$g1,
                                          species_paths$g2,
                                          species_paths$g3, 
                                          sep = "/")
        dataCirc <- as.Node(species_paths)
        
        circlepackeR(dataCirc, size = "size", color_min = "hsl(56,80%,80%)", 
                     color_max = "hsl(341,30%,40%)")
        
    })
    
    output$chSpeciesText5 <- renderText({ 
        
        paste('Species: ', input$selectEsp)
    })
    
    output$chSpeciesText6 <- renderText({ 
        
        paste('Species: ', input$selectEsp)
    })
    output$geoLabel <- renderText({ 
        
        seleccionCriterios$origen_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
    })
    output$vulLabel <- renderText({ 
        
        seleccionCriterios$vulnerabilidad_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
    })
    output$ecoLabel <- renderText({ 
        
        seleccionCriterios$imp_economica_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
    })
    output$segaLabel <- renderText({ 
        
        seleccionCriterios$seg_alimentaria_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
    })
    
    # dfl <- select(seleccionCriterios, 
    #               nombre_cientifico, 
    #               origen_et,
    #               vulnerabilidad_et,
    #               imp_economica_org, 
    #               imp_economica_org_et, 
    #               seg_alimentaria_org, 
    #               seg_alimentaria_org_et, 
    #               incertidumbre, 
    #               incertidumbre_et, 
    #               imp_economica, 
    #               imp_economica_et, 
    #               seg_alimentaria, seg_alimentaria_et, pool_imp_sa, int_banco, int_banco_et)
    # dff <- filter(dff, nombre_cientifico == input$selectEsp)    
    output$originBox <- renderInfoBox({
        
        origin <- seleccionCriterios$origen_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (origin == "Local"){
            infoBox(
                "Geographic Origin: ", paste0(origin), icon = icon("globe-americas"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (origin == "Close"){
            infoBox(
                "Geographic Origin: ", paste0(origin), icon = icon("globe-americas"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (origin == "Distant"){
            infoBox(
                "Geographic Origin: ", paste0(origin), icon = icon("globe-europe"),
                color = "red", fill = FALSE
            )      
        }
        
    })
    
    output$vulBox <- renderInfoBox({
        
        vul <- seleccionCriterios$vulnerabilidad_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (vul == "Threatened"){
            infoBox(
                "Vulnerability: ", paste0(vul), icon = icon("skull-crossbones"),
                color = "red", fill = FALSE
            )
        } else if (vul == "MinorConcern"){
            infoBox(
                "Vulnerability: ", "Minor concern", icon = icon("seedling"),
                color = "green", fill = FALSE
            )      
            
        } else if (vul == "NotEvaluated"){
            infoBox(
                "Vulnerability: ", "Not evaluated", icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
    
    output$ecoBox <- renderInfoBox({
        
        eco <- seleccionCriterios$imp_economica_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (eco == "High"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("gbp"),
                color = "green", fill = FALSE
            )
        } else if (eco == "Medium"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("euro-sign"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (eco == "Low"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("dollar"),
                color = "red", fill = FALSE
            )
            
        } else if (eco == "Undetermined"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
    output$segaBox <- renderInfoBox({
        
        sa <- seleccionCriterios$seg_alimentaria_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (sa == "High"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("carrot"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (sa == "Medium"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("drumstick-bite"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (sa == "Low"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("cheese"),
                color = "red", fill = FALSE
            )
            
        } else if (sa == "Undetermined"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
    output$originBox2 <- renderInfoBox({
        
        origin <- seleccionCriterios$origen_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (origin == "Local"){
            infoBox(
                "Geographic Origin: ", paste0(origin), icon = icon("globe-americas"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (origin == "Close"){
            infoBox(
                "Geographic Origin: ", paste0(origin), icon = icon("globe-americas"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (origin == "Distant"){
            infoBox(
                "Geographic Origin: ", paste0(origin), icon = icon("globe-europe"),
                color = "red", fill = FALSE
            )      
        }
        
    })
    
    output$vulBox2 <- renderInfoBox({
        
        vul <- seleccionCriterios$vulnerabilidad_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (vul == "Threatened"){
            infoBox(
                "Vulnerability: ", paste0(vul), icon = icon("skull-crossbones"),
                color = "red", fill = FALSE
            )
        } else if (vul == "MinorConcern"){
            infoBox(
                "Vulnerability: ", "Minor concern", icon = icon("seedling"),
                color = "green", fill = FALSE
            )      
            
        } else if (vul == "NotEvaluated"){
            infoBox(
                "Vulnerability: ", "Not evaluated", icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
    
    output$ecoBox2 <- renderInfoBox({
        
        eco <- seleccionCriterios$imp_economica_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (eco == "High"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("gbp"),
                color = "green", fill = FALSE
            )
        } else if (eco == "Medium"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("euro-sign"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (eco == "Low"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("dollar"),
                color = "red", fill = FALSE
            )
            
        } else if (eco == "Undetermined"){
            infoBox(
                "Economic Importance: ", paste0(eco), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
    output$segaBox2 <- renderInfoBox({
        
        sa <- seleccionCriterios$seg_alimentaria_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (sa == "High"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("carrot"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (sa == "Medium"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("drumstick-bite"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (sa == "Low"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("cheese"),
                color = "red", fill = FALSE
            )
            
        } else if (sa == "Undetermined"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
        output$segaBox3 <- renderInfoBox({
        
        sa <- seleccionCriterios$seg_alimentaria_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (sa == "High"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("carrot"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (sa == "Medium"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("drumstick-bite"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (sa == "Low"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("cheese"),
                color = "red", fill = FALSE
            )
            
        } else if (sa == "Undetermined"){
            infoBox(
                "Food security: ", paste0(sa), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })
    
    output$chPriority <- renderText({
    
        pri <- seleccionCriterios$int_banco_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        
        paste('Priority: ', pri)
    })
    
    
    ##################################################
    
    output$intBGV <- renderInfoBox({
        
        int <- seleccionCriterios$int_banco_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        
        if (int == "High"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("check-circle"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (int == "Medium"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("circle"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (int == "Low"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("exclamation"),
                color = "red", fill = FALSE
            )
            
        } else if (int == "Undetermined"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }

        
    })
    
    ##################################################
    

    
    output$intBGV2 <- renderInfoBox({
        
        int <- seleccionCriterios$int_banco_et[seleccionCriterios$nombre_cientifico == input$selectEsp]
        if (int == "High"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("check-circle"),
                color = "green", fill = FALSE, width = 6
            )
        } else if (int == "Medium"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("circle"),
                color = "yellow", fill = FALSE
            )      
            
        } else if (int == "Low"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("exclamation"),
                color = "red", fill = FALSE
            )
            
        } else if (int == "Undetermined"){
            infoBox(
                "BGVCOL Priority: ", paste0(int), icon = icon("question"),
                color = "black", fill = FALSE
            )      
        }
        
    })  

#########################################################################################
    output$downloadData <- downloadHandler(
        data <- seleccionCriterios,
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data, file, row.names = FALSE)
        }
    )
    
    datasetInput <- reactive({
        switch(input$dataset,
               "rock" = rock,
               "pressure" = pressure,
               "cars" = cars)
    })
    
    # Table of selected dataset ----

    # Downloadable csv of selected dataset ----

    
    output$datatable = renderDataTable({
        cat <- as.numeric(input$selectCat2)
        
        print(input$selectFAO)
        #fao <- as.character(input$selectFAO)
        
        #print(paste0("#### "), fao," ###")
        
        dff <- select(seleccionCriterios, nombre_cientifico,
                      categoria, clasificacion_FAO, cultivo, origen_et, vulnerabilidad_et, imp_economica_et, seg_alimentaria_et,int_banco_et)
        
        #colnames(dff) <- c("scientific name","group","FAO group","crop","origin","vulnerability","economic benefits","food security importance","prioriy index")
        
        if (input$selectFAO != "ALL"){
            print("NOT ALL FAO")
            dff <- filter(dff, clasificacion_FAO == input$selectFAO)
        }

        #dff <- filter(seleccionCriterios, nombre_cientifico == input$selectEsp)
        
        #dff <- filter(dff, clasificacion_FAO == fao)
        if (cat == 2){
            print("##### BGV ####")
            dff <- filter(dff, categoria == "BGVCOL")
        } else if ( cat == 3) {
            print("##### NCB ####")
            dff <- filter(dff, categoria == "NCB")
        }
        
        if (input$selectInt != "ALL"){
          print("NOT ALL INT")
          dff <- filter(dff, int_banco_et == input$selectInt)
        }
        
        
        
        
        
        
        colnames(dff) <- c("scientific name","group","FAO group","crop","origin","vulnerability","economic benefits","food security importance","priority index")
        
        
        
        
        dff
    })
    
    output$downloadData <- downloadHandler(

        
        filename = function() {
            paste("bgvcol-", Sys.Date(), "-", ".csv", sep="")
        },
        content = function(file) {
            write.csv(seleccionCriterios, file)
        }
    )
    
}

shinyApp(ui, server)