library("shiny")
library("shinydashboard")
library("ggplot2")
library("keras")
library("tidyverse")

ui <- dashboardPage(
  skin = "green", dashboardHeader(title = "F o l i u m"),
  dashboardSidebar(
    sidebarMenu(width = 10,
                menuItem(text = "Description | Opis", tabName = "description", icon = icon("comment-alt")),
                menuItem(text = "Species | Gatunki", tabName = "species", icon = icon("brain")),
                menuItem(text = "Recognition | Rozpoznawanie", tabName = "recognition", icon = icon("leaf"), badgeLabel = "98% accuracy"),
                menuItem(text = "Creator | Twórca", tabName = "creator", icon = icon("pen"))
    )
  ),
  
  dashboardBody(
    tags$head( 
      tags$style(HTML("
                      .skin-green .sidebar-menu>li:hover>a { background-color: #02ac46; }
                      .skin-green .main-header .logo { width: 320px; }
                      .main-header .navbar { margin-left: 320px; }
                      .btn { width: 150px; height: 40px; font-size: 17px; color: white; background-color: #01bc4c; }
                      .form-control { height: 40px; text-align: center; }
                      .main-sidebar { min-width: 320px; font-size: 18px; background: #bec4ce; }
                      .content-wrapper { margin-left: 320px; }
                      .skin-green .sidebar-menu>li.active>a { background-color: #02ac46; }
                      .skin-green .sidebar-menu a:hover { font-size: 19px; }
                      .fa { margin-right: 18px; color: white;}
                      .fa:hover { font-size: 20px; }
                      .sidebar-toggle { display: none; }
                      .qt pre { font-family: inherit !important; font-size: 15px; }
                      .table { font-family: monospace; font-weight: bold; }
                      .skin-green .main-header .logo { font-weight: bold; font-size: 25px; background-color: #01bc4c; color: white; }
                      .skin-green .main-header .logo:hover { background-color: #01bc4c; }
                      .skin-green .main-header .navbar { background-color: #02ac46; }
                      .irs-bar { border-top: 1px solid #02ac46; background: #02ac46; }
                      .irs-bar-edge { border-top: 1px solid #02ac46; background: #02ac46; }
                      .irs-single { background: #02ac46; }
                      .progress-bar { background: #02ac46; }
                      .sidebar-menu>li .badge { position: relative; z-index: 1; font-size: 11.5px; }
                      td { padding: 10px; font-style: italic; padding-right: 50px; }
                      th { padding: 10px; font-size: 14px; padding-right: 50px; }
                      table { font-size: 14px; }
                      hr { border: 1px solid #02ac46; margin: 3px 0 4px 0; }
                      .shiny-output-error { color: white; }
                      p { max-width: 700px; text-align: justify; }
                      "))
      ),
    tabItems(
      
      tabItem(tabName = "description", htmlOutput("description"), width = 10),
      tabItem(tabName = "species", htmlOutput("species"), width = 10),
      
      tabItem(tabName = "recognition",
              fileInput(inputId = 'file', label = 'Wybierz obraz do rozpoznania',
                        accept = c('image/jpeg', '.jpg', '.jpeg'),
                        buttonLabel = "Wybierz"
              ),
              h1("Rozpoznanie"),
              fluidRow(
                box(
                  plotOutput("preview"), width = 12)
              ),
              fluidRow(
                box(div(tableOutput("tab"), style = "box-shadow: 0px 0px 57px -5px rgba(0,0,0,0.76);"), title = "Rozpoznanie", width = 5)
              ),
              fluidRow(
                box(tableOutput("tab2"), title = "Opis",
                    actionButton("button", "Pokaż opis"), width = 12)
              ),
              fluidRow(
                box(title = "Parametr", 
                    sliderInput("num", "Liczba predykcji:", 1, 6, 3))
              )
      ),
      tabItem(tabName = "creator", htmlOutput("creator"), width = 10)
    )
      )
    )

server <- function(input, output) {
  
  image <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    image_load(inFile$datapath) %>% image_to_array()
  })
  
  output$description <- renderUI({
    HTML("<h2>Opis</h2><hr>
         <p>Aplikacja została stworzona w celu rozpoznawania gatunków drzew na podstawie zdjęcia liścia.
         Oprócz wskazania jednego z dwudziestu gatunków &#40pełna lista gatunków w zakładce: Gatunki&#41,
         narzędzie podaje dodatkowe informacje, takie jak nazwa łacińska, czy parametry dendrologiczne.<br><br>
         Głównym celem leżącym u podstaw powstania narzędzia jest wspomaganie inżyniera budownictwa,
         w projektowaniu i realizacji obiektów budowlanych. Aplikacja powstała jako wynik pracy inżynierskiej 
         na Wydziale Budownictwa i Inżynierii Środowiska &#40kierunek Budownictwo&#41 Politechniki Poznańskiej.<br><br>
         Aplikacja działa w oparciu o rozwiązania uczenia maszynowego - sieci neuronowe. Uściślając, do
         budowy modelu rozpoznającego obraz wykorzystano warstwy konwolucyjne w połączeniu ze standardowymi.
         <h2>Rekomendacje</h2><hr>
         <p>Skuteczność rozpoznawania będzie najwyższa jeśli zostaną spełnione następujące warunki:
         <br>- na zdjęciu powinien znajdować się tylko jeden liść, bez obiektów innego typu,
         <br>- pozycja liścia względem krawędzi zdjęcia powinna być możliwie ortogonalna,
         <br>- tło zdjęcia powinno być możliwie jednolite, najlepiej jasne, o białej barwie, pozwoli to najlepiej odseparować liść,
         <br>- zdjęcie powinno być w kolorze o umiarkowanej jasności,
         <br>- krawędź zdjęcia nie może przecinać krawędzi liścia,
         <br>- liść winien trafić do rozpoznania jako całkowity &#40łącznie z ogonkiem&#41,
         <br>- należy fotografować stronę liścią z unerwieniem mniej wiczocznym,
         <br>- dla liści dłoniastych do rozpoznania powinien trafić pojedyńczy, niepodzielny element liścia,
         <br>- w przypadku błędnego rozpoznania, należy sprawdzić wynik drugi w kolejności lub zajrzeć do listy gatunków.</p>")
  })
  
  output$species <- renderUI({
    HTML("<table><tr>
         <th>Nazwa gatunku<hr></th>
         <th>Nazwa łacińska<hr></th>
         </tr>
         <tr>
         <td>Klon polny</td>
         <td>Acer campestre</td>
         </tr>
         <tr>
         <td>Klon ginnala</td>
         <td>Acer ginnala</td>
         </tr>
         <tr>
         <td>Klon palmowy</td>
         <td>Acer palmatum</td>
         </tr>
         <tr>
         <td>Klon zwyczajny</td>
         <td>Acer platanoides</td>
         </tr>
         <tr>
         <td>Klon jawor</td>
         <td>Acer peudoplatanus</td>
         </tr>
         <tr>
         <td>Brzoza brodawkowata</td>
         <td>Betula pendula</td>
         </tr>
         <tr>
         <td>Brzoza omszona</td>
         <td>Betula pubescens</td>
         </tr>
         <tr>
         <td>Grab pospolity</td>
         <td>Carpinus betulus</td>
         </tr>
         <tr>
         <td>Kasztan jadalny</td>
         <td>Castanea sativa</td>
         </tr>
         <tr>
         <td>Wiązowiec zachodni</td>
         <td>Celtis occidentalis</td>
         </tr>
         <tr>
         <td>Leszczyna turecka</td>
         <td>Corylus colurna</td>
         </tr>
         <tr>
         <td>Buk zwyczajny</td>
         <td>Fagus sylvatica</td>
         </tr>
         <tr>
         <td>Miłorząb dwuklapowy</td>
         <td>Ginko biloba</td>
         </tr>
         <tr>
         <td>Orzech włoski</td>
         <td>Juglans regia</td>
         </tr>
         <tr>
         <td>Sosna czarna</td>
         <td>Pinus nigra</td>
         </tr>
         <tr>
         <td>Platan klonolistny</td>
         <td>Platanus acerifolia</td>
         </tr>
         <tr>
         <td>Topola osika</td>
         <td>Populus tremula</td>
         </tr>
         <tr>
         <td>Dąb burgundzki</td>
         <td>Quercus cerris</td>
         </tr>
         <tr>
         <td>Dąb wielkoowocowy</td>
         <td>Quercus macrocarp</td>
         </tr>
         <tr>
         <td>Dąb błotny</td>
         <td>Quercus palustris</td>
         </tr>
         <tr>
         <td>Dąb szypupułkowy</td>
         <td>Quercus robur</td>
         </tr>
         <tr>
         <td>Wierzba biała</td>
         <td>Salix alba</td>
         </tr>
         <tr>
         <td>Lipa drobnolistna</td>
         <td>Tilia cordata</td>
         </tr>
         <tr>
         <td>Choina kanadyjska</td>
         <td>Tsuga canadensis</td>
         </tr>
         <tr>
         <td>Brzostownica japońska</td>
         <td>Zelkova serrata</td>
         </tr>
         </table>")
  })
  
  output$preview <- renderPlot({
    # validate(
    #   need(input$image, "Please choose an image!")
    # )
    plot(as.raster(image(), max = 255))
  })
  
  sliderInput(inputId = "num", label = "Wybierz liczbę", value = 3, min = 1, max = 6)
  
  names <- NULL
  
  output$tab <- renderTable({
    # req(input$img)
    model <- load_model_hdf5("leaf2.h5")
    leafs <- readxl::read_excel("leaf_species.xlsx")$`Nazwa gatunku`
    img <- image()
    img <- img %>% image_array_resize(128, 128) 
    img <- img/255
    img <- array_reshape(img, c(1, 128, 128, 3))
    probs <- model %>% predict(img)
    withProgress(message = 'Proces w trakcie',
                 detail = 'Może to zająć chwilę...', value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     sum(runif(10000000,0,1))
                   }
                 })
    ind <- order(probs, decreasing = TRUE)[1:input$num]
    probs <- probs[ind]
    names <<- leafs[ind]
    results <- data.frame(Nazwa = names, Prawdopodobieństwo = probs)
    results
  }, digits = 3)
  
  observeEvent(input$button, {
    output$tab2 <- renderTable({
      desc <- readxl::read_excel("leaf_full_description.xlsx")
      desc %>% filter(`Nazwa gatunku` == names[1]) %>% select(-`Nazwa gatunku`)
    })
  })
  
  output$creator<- renderUI({
    HTML("<b>Author</b> | <b>Autor</b><br>Tomasz Olejniczak<br><br><b>Thesis Advisor</b> | <b>Promotor</b><br>Dr inż. Tomasz Jeż<br><br>
         &copy 2019")
  })
  }

shinyApp(ui = ui, server = server)

