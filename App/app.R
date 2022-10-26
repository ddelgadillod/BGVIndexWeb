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

bgvEsp <- filter(especies, categoria == "BGV List")
bgvEsp <- as.vector(bgvEsp$nombre_cientifico)
extEsp <- filter(especies, categoria == "External species")
extEsp <- as.vector(extEsp$nombre_cientifico)

chcsEsp <- bgvEsp

dfreg <- read.csv2('Countries&Regions.csv', encoding="UTF8", stringsAsFactors = FALSE)






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
                                           tags$img(src='logoCorporativoColor2.svg',height='60',width='200'))



ui <- dashboardPagePlus(skin = "blue",
                        title='Agrosavia - BGV priorization Index',
                        dbHeader,
                        dashboardSidebar(
                            width = 220,
                            
                            sidebarMenu(
                                menuItem("Background", icon = icon("monument"),
                                         menuSubItem("Abstract", tabName = "abstract", icon = icon("journal-whills")),
                                         menuSubItem("Usage", tabName = "usage", icon = icon("journal-whills"))),
                                menuItem("Pillars", icon = icon("globe"),
                                         menuItem("Geographic origin", tabName = "geo_origin", icon = icon("map-marked-alt")),
                                         menuItem("Vulnerability", tabName = "vulnerability", icon = icon("exclamation-triangle")),
                                         menuItem("Economic Importance", tabName = "economic", icon = icon("search-dollar")),
                                         menuItem("Food security", tabName = "food_sec", icon = icon("utensils"))),
                                menuItem("BGV priority index", tabName = "bgv_index", icon = icon("sort-amount-up-alt")),
                                #menuItem("Data", tabName = "data", icon = icon("table")),
                                menuItem("Data", tabName = "data2", icon = icon("table")),
                                menuItem("About", tabName = "about", icon = icon("address-card")),
                                menuItem("Filter Options", tabName = "filter", icon = icon("filter"),
                                         # radioButtons("radio", NULL,
                                         #              choices = list("BGV List"= 1,
                                         #                             "External species"= 2),
                                         #              selected = 1),
                                         
                                         selectInput("selectCat",
                                                     NULL,
                                                     choices = c("BGV List"= 1,"External species"= 2),
                                                     multiple = FALSE,
                                                     selected = 1),

                          
                                         
                                         
                                         selectInput("selectEsp",
                                                     NULL,
                                                     choices = dfp$nombre_cientifico,
                                                     multiple = FALSE,
                                                     selected = 2))

                            )
                        ),
                        
                        dashboardBody(title='Prioritizing species research in a diverse national plant germplasm bank',
                                      tags$head(
                                          tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")
                                      ),
                                      tabItems(
                                          # First tab content
                                          
                                          tabItem(tabName = "abstract",
                                                  h1('Prioritizing species research in a diverse national plant germplasm bank',style="text-align: center"),
                                                  h5("Cerón-Souza, I1., Delgadillo, D.A1., Polo-Murcia, S.M1, Sarmiento-Naizaque, Z.X1., Reyes-Herrera, P.H.1*",style="text-align: center"),
                                                  h5("1Corporación Colombiana de Investigación Agropecuaria – AGROSAVIA.",style="text-align: right"),
                                                  h6("C.I. Tibaitatá. Km 14 vía Bogotá Mosquera.",style="text-align: right"),
                                                  h6("Cundinamarca, Colombia.", style="text-align: right"),
                                                  h6("*Corresponding author: phreyes@agrosavia.co", style="text-align: right"),
                                                  
                                                  p("National germplasm banks from developing countries are the most significant repository for plant genetic resources for food and agriculture (PGRFA). They store an agrobiodiversity base to solve food security in the face of global warming at the local and regional level. Despite their strategic importance, germplasm banks that depend on national funds in tropical developing countries have extremely limited funds to support scientific research. Therefore, it is indispensable to create a straight methodology that aligns policies to the bank's priorities and research investment. We propose a data-driven approach to build an index that integrates four pillars consistent with sustainable development goals, such as zero hunger, no poverty, and climate change. We retrieved the information in open-access databases for the geographic origin, the state of vulnerability, crop's economic importance for the country, and food security importance.", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                  
                                                  
                                                  p("This study focused on creating a prioritizing index using fuzzy logic to give values and rank 275 crop species and wild relatives preserved in the Colombian's plant germplasm bank. We classified and ranked all the 275 species in either high (8.72%), medium (26.18%), and low research priority (65.09%). Because of the absence of information for many native crops and wild relatives, we coupled the index with different confidence levels depending on whether we used direct data or extrapolated it from the closest species with information", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                  
                                                  img(height = 350,  src='https://www.agrosavia.co/media/3535/01.jpg', alt="www.agrosavia.co" , style="vertical-align:middle"),
                                                  #br(),
                                                  p("Our methodology had several advantages: the data used to construct the index came from open-access databases. We used a data-driven approach independent of the bank scientists' preferences and biases. The classification of species in three prioritizing categories simplified the information for non-scientific training stakeholders and politicians that usually decide how to invest research funds. Finally, the methodology identified the most significant information gaps for native crops and wild relatives conserved in the bank. We think our method is easy to implement in other tropical developing countries with national germplasm banks and PGRFA projects that face limited fund resources", style = "text-align: justify;font-family: 'arial'; font-size:100%"),
                                                  
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

                                          tabItem(tabName = "usage",
                                                  h1('Blog Usage',style="text-align: center"),
                                                  h2('1. Menu: ',style="text-align: left"),
                                                  img(height = 350,  src='leftBar.PNG', NULL , style="vertical-align:middle"),
                                                  
                                                  h3('Blog Usage',style="text-align: left"),
                                                  
                                                  
                                          ),                                          
                                          
                                          
                                          tabItem(tabName = "background",
                                                  h1("Priority index based on fuzzy logic"),
                                                  p("We asked how to define a list of priorities species for two of our primary pre-breeding responsibilities in the Colombian germplasm banks, which are:", style = "font-family: 'arial'; font-size:120%"),
                                                  p("1. What species must conserve within the bank", style = "font-family: 'arial'; font-size:120%"),
                                                  p("2. What species we should invest money in scientific research.", style = "font-family: 'arial'; font-size:120%"), 
                                                  p("We proposed four pillars to identify priorities for the Germplasm Bank consistent with FAO sustainable development goals such as zero hunger, no poverty, and climate action. They represent both the country's opportunities and responsibilities.",style = "font-family: 'arial'; font-size:120%"),
                                                  
                                                  tags$img(height = 400, width = 600,src = "mind_map_pillars.svg"),
                                                  
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
                                                  # fluidRow(box(#selectInput("selectCat", 
                                                  #               #           h5("Category"), 
                                                  #               #           choices = c("BGV List"= 1,"External species"= 2),
                                                  #               #           #choices = unique(dfp$categoria)
                                                  #                #          selected = 2
                                                  # #),
                                                  # ),
                                                  # box(selectInput("selectEsp", 
                                                  # h5("Specie:"), 
                                                  # choices = chcsEsp
                                                  # #choices = levels(dfp$nombre_cientifico),
                                                  #selected = 1
                                                  # ),
                                                  # ),
                                                  # ),
                                                  p("The geographical origin information of 275 species was processed following the classification of crop diversity regions of Khoury et al. (2016) and the POWO database (POWO, 2019). Species were organized into 26 regions, we grouped the 26 regions in three classes (fuzzy sets): local, close, and distant.",style = "font-family: 'arial'; font-size:120%"),
                                                  fluidRow(box(plotlyOutput("origEsp",width = '900px',height = "500px",),
                                                               #verbatimTextOutput("summary") 
                                                  ),
                                                  )
                                          ),
                                          tabItem(tabName = "vulnerability",
                                                  h1("Vulnerability"),
                                                  p("",style = "font-family: 'arial'; font-size:120%"),
                                                  
                                                  p("We classified the vulnerability of 275 species following the databases of Bernal et al. (2019), the Botanical Garden for International Conservation (BGCI, 2019) and Red List platforms (Red List, 2019). We merged the national and the international vulnerability categories into three: Not assessed, Least concern, and Threatened.",style = "font-family: 'arial'; font-size:120%"),
                                                  
                                                  fluidRow(#infoBoxOutput("estUNEsp"),
                                                      #infoBoxOutput("estBGRLEsp"),
                                                      infoBoxOutput("VUL"),
                                                  ),
                                                  fluidRow(
                                                      # A static infoBox
                                                      infoBox("Endangered", 'Red List', icon = icon("skull-crossbones"), color = 'red'),
                                                      # Dynamic infoBoxes
                                                      #infoBoxOutput("progressBox"),
                                                      #infoBoxOutput("approvalBox")
                                                  ),
                                                  fluidRow(
                                                      infoBox("Vulnerable", 'UN List', icon = icon("exclamation-triangle"), fill = TRUE, color = 'orange'),
                                                      #infoBoxOutput("progressBox2"),
                                                      #infoBoxOutput("approvalBox2")
                                                  ),
                                                  fluidRow(
                                                      infoBox("Least concern", 'UN List', icon = icon("seedling"), fill = TRUE, color = 'green'),
                                                      #infoBoxOutput("progressBox2"),
                                                      #infoBoxOutput("approvalBox2")
                                                  ),
                                                  
                                                  
                                          ),
                                          tabItem(tabName = "economic",
                                                  h1("Economic Importance"),
                                                  p("En Colombia la frontera agricola nacional es de 40075960 de hectareas, la agricultura representa el XX por ciento del empleo a tiempo completo, el 6 por ciento del PIB y el 27 por ciento de los ingresos totales de exportacion.",style = "font-family: 'arial'; font-size:120%"),
                                                  p("Los cambios en el clima, las condiciones de produccion y el gusto de los consumidores determinan las dinamicas de nuestros cultivos alimentarios.",style = "font-family: 'arial'; font-size:120%"),
                                                  p("Para capturar la importancia economica de las especies vegetales del BGVCOL, caracterizamos cada una de las 275 especies en terminos de: yield, municipality coverage, Lafay index, income. ",style = "font-family: 'arial'; font-size:120%"),
                                                  p("",style = "font-family: 'arial'; font-size:120%"),
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
                                                  p("Se evalua la relacion entre la produccion de una especie y su consumo aparente (es decir, produccion mas importacion menos exportacion) en un anno. Si este valor es superior a uno, el pais es un exportador neto de la especie, y cuanto mayor es el nivel, mas importantes son las exportaciones como destino de la produccion nacional de la especie.",style = "font-family: 'arial'; font-size:120%"),
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
                                                  tags$img(height = 400, width = 600,src = "compact_economic.jpg"),
                                                  br(),
                                          ),
                                          tabItem(tabName = "food_sec",
                                                  h1("Food security"),
                                                  
                                                  p("The second Sustainable Development Goal (SDA) challenges the world to achieve food security and improve nutrition by 2030. Consequently, we adopted a multidimensional approach to evaluate four attributes associated with food security for each one of the 344 species of this study. They are government priority list, traditional consumption, nutritional contribution, and affordability based on nutrients.",
                                                    style = "font-family: 'arial'; font-size:120%"),
                                                  
                                                  
                                                  h3("Government priority list"),
                                                  p("In 2013, the Colombian government created a list of plant genetic resources to improve their stable consumption in the Colombian population's diet and to guarantee policies to improve their production and supply (PNSAN, 2013).",
                                                    style = "font-family: 'arial'; font-size:120%"),
                                                  tags$img(height = 400, width = 600,src = "priority_list.jpg"),
                                                  
                                                  h3("Traditional consumption"),
                                                  p("This variable linked the agricultural and food traditions that reflect the multicultural, multi-ethnic, and biodiverse nature of Colombia's geographic eleven regions (Ministry of Culture in 2012) (Fig. 6). We created a numerical variable based on this information that shows the number of regions where each species is essential for the food tradition. Based on the number of regions where each species is listed, we assigned a label with three categories narrow use, medium use, and large use across regions.",
                                                    style = "font-family: 'arial'; font-size:120%"),
                                                  tags$img(height = 400, width = 600,src = "traditional_map2.jpg"),
                                                  
                                                  h3("Nutritional contribution"),
                                                  p("Colombia has made significant economic and social progress in recent decades. Although its current classification is an upper-middle-income country, there are still considerable challenges in achieving a convergence towards higher living standards. In this sense, micronutrient deficiencies continue to prevail among children under five years of age and contribute to the deterioration of child development and the increase in the national disease burden. We evaluated the 275 species based on their contribution to the daily nutrient intake requirements for this pillar considering the local crops to guarantee three deficient micronutrients' dietary sufficiency in the country's child population.",
                                                    style = "font-family: 'arial'; font-size:120%"),
                                                  tags$img(height = 400, width = 600,src = "nutrition.jpg"),
                                                  
                                                  h3("Affordability based on its nutrients"),
                                                  p("In order to identify affordability of the 275 species in monetary terms and locally appropriate to fill gaps in micronutrient intake, we calculated the nutrient-price ratio as the consumer price for every 100 grams of an edible portion in four variables: by a unit of energy (kcal) and by a unit of micronutrient  of calcium, iron, zinc. Using this value, we identify the species competitivity from the nutritional and market standpoint.",
                                                    style = "font-family: 'arial'; font-size:120%"),
                                                  tags$img(height = 400, width = 600,src = "affordability.jpg"),
                                                  
                                                  
                                                  p("",style = "font-family: 'arial'; font-size:120%"),
                                                  p("",style = "font-family: 'arial'; font-size:120%"),
                                                  
                                                  
                                                  
                                                  
                                          ),
                                          tabItem(tabName = "bgv_index",
                                                  h1("BGV Priority Index"),
                                                  
                                          ),
                                          # tabItem(tabName = "data",
                                          #         h1("Available data"),
                                          #         
                                          #         fluidRow(downloadButton("downloadData", "Download"),
                                          #                  box(dataTableOutput("datatable"),
                                          #                      style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                          #                  )
                                          #                  
                                          #         ),
                                          # ),
                                          tabItem(tabName = "data2",
                                                  h1("Category data"),
                                                  
                                                  selectInput("selectCat2",
                                                              NULL,
                                                              choices = c("BGV List"= 1,"External species"= 2),
                                                              multiple = FALSE,
                                                              selected = 1),        
                                                  
                                                  fluidRow(box(dataTableOutput("datatable2"),
                                                               style = "width:1100px; overflow-y: scroll;overflow-x: scroll;"
                                                               
                                                  )
                                                  
                                                  ),
                                          ),                                          
                                          tabItem(tabName = "about",
                                                  h1("About us")
                                          )
                                          
                                      )
                        )
)

server <- function(input, output,session) {
    observe({
        x <- as.numeric(input$selectCat2)
        print(x)
        
        chcsEsp <- input$selectEsp
        
        print(chcsEsp)
        
        # updateSelectInput(session,
        #                   "selectEsp",
        #                   choices = chcsEsp 
        # )
    })
    
    
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    output$datatable = renderDataTable({
        dff <- filter(seleccionCriterios, nombre_cientifico == input$selectEsp)
        dff
        #dfg
    })
    
    output$datatable2 = renderDataTable({
        if(as.numeric(input$selectCat2) == 1){
            dfc <- filter(seleccionCriterios, categoria == "BGVCOL")
        } else {
            dfc <- filter(seleccionCriterios, categoria == "NCB")
        }
        #dff <- filter(seleccionCriterios, nombre_cientifico == input$selectEsp)
        dfc
        #dfg
    })
    output$origEsp <- renderPlotly({
        x <- as.numeric(input$selectCat)
        print(x)
        
        if(x == 1){
            chcsEsp <- bgvEsp
        } else if (x == 2){
            chcsEsp <- extEsp
        }
        print(chcsEsp)
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
            layout(title = "Production in TON 2008-2018",
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Quantity')) %>% 
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
                   yaxis = list(title = 'Area [HA]')) %>% 
            config(displayModeBar = F)
    })  
    
    output$vpEsp <- renderPlotly({
        dff <- select(seleccionCriterios, nombre_cientifico, VP1,VP2,VP3,VP4,VP5,
                      VP6,VP7,VP8,VP9,VP1,VP10,VP11,prom_vp,rendimiento,valor_produccion_h)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        xvp <- seq(2008,2018)
        yvp <- as.vector(dff[1,2:12])
        yvp <- as.numeric(yvp)
        br()
        plot_ly(x = xvp, y = yvp, mode = 'scatter', color = I("blue")) %>% 
            layout(title = "Production value 2008-2018", 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Value')) %>% 
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
        dff <- select(seleccionCriterios, nombre_cientifico, un_estado_conservacion)
        dff <- filter(dff, nombre_cientifico == input$selectEsp)
        if(is.na(dff$un_estado_conservacion[1])){
            paste("Not available")
        }else{
            paste(dff$un_estado_conservacion[1])
        }
        
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
    
    
    
    
}

shinyApp(ui, server)
