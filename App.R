# Application interactive pour la publication de l'activite aux noeuds
# 
# Juin 2018 par JF Cantin
#
# donnees source via Enquete OD2013
# pre-compile via P:\6.05 DONNEES DE REFERENCE\6.05.05 ENQUETES OD\2013\Analyses\Matrice d'activites aux noeuds 2018-06-04.Rmd
# 


library(shiny)
library(circlize)
library(readxl)
library(tidyverse)


Matrice_mode_avant_apres_totale <- read_xlsx ("Matrice_mode3.xlsx")
Matrice_mode_avant_apres_totale$X <-NULL
Matrice_mode_avant_apres_totale$FEXP <- as.integer (trunc(as.numeric(Matrice_mode_avant_apres_totale$FEXP )/25)*25)



ui <- fluidPage(
  titlePanel("Activites aux noeuds"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Principaux equipements de transport collectif. Source : Enquete OD2013 "),
      
      selectInput("Periode", 
                  label = "Pour quelle periode ?",
                  
                  choices = c("24hr", 
                              "PPAM"),
                  
    
                  selected = "PPAM"),
      
      
      selectInput("station", 
                  label = "Quel noeud ?",
                  choices = c("GARE BAIE D'URFE" ,"GARE BEACONSFIELD" ,"GARE BEAUREPAIRE" ,"GARE BLAINVILLE" ,"GARE BOIS-DE-BOULOGN" ,"GARE BOIS-FRANC" ,"GARE CANDIAC" ,"GARE CANORA" ,"GARE CEDAR PARK" ,"GARE CHABANEL" ,"GARE DELSON" ,"GARE DEUX-MONTAGNES" ,"GARE DORION" ,"GARE DORVAL" ,"GARE DU RUISSEAU" ,"GARE GRAND-MOULIN" ,"GARE HUDSON" ,"GARE ILE BIGRAS" ,"GARE ILE PERROT" ,"GARE LACHINE" ,"GARE LASALLE" ,"GARE MCMASTERVILLE" ,"GARE MONT ST- HILAIR" ,"GARE MONTPELLIER" ,"GARE MONTREAL-OUEST" ,"GARE MONT-ROYAL" ,"GARE PINCOURT/T.-VAU" ,"GARE PINE BEACH" ,"GARE POINTE-CLAIRE" ,"GARE ROSEMERE" ,"GARE ROXBORO/PIERREF" ,"GARE SAINT-BASIL-LE-" ,"GARE SAINT-HUBERT" ,"GARE SAINT-JEROME" ,"GARE ST-BRUNO" ,"GARE ST-CONSTANT" ,"GARE STE-ANNE-DE BEL" ,"GARE STE-CATHERINE" ,"GARE STE-DOROTHEE" ,"GARE STE-ROSE" ,"GARE STE-THERESE" ,"GARE ST-LAMBERT" ,"GARE SUNNYBROOKE" ,"GARE VALOIS" ,"GARE VAUDREUIL" ,"GARE VIMONT" ,"ST ACADIE" ,"ST ANGRIGNON" ,"ST ASSOMPTION" ,"ST ATWATER" ,"ST BEAUBIEN" ,"ST BEAUDRY" ,"ST BERRI/UQAM" ,"ST BONAVENTURE" ,"ST CADILLAC" ,"ST CARTIER" ,"ST CHAMP-DE-MARS" ,"ST CHARLEVOIX" ,"ST CONCORDE" ,"ST COTE-DES-NEIGES" ,"ST COTE-VERTU" ,"ST CREMAZIE" ,"ST C-STE-CATHERINE" ,"ST DE CASTELNEAU" ,"ST DE LA SAVANE" ,"ST DE L'EGLISE" ,"ST D'IBERVILLE" ,"ST DU COLLEGE" ,"ST EDOUARD-MONTPETIT" ,"ST FABRE" ,"ST FRONTENAC" ,"ST GEORGES-VANIER" ,"ST GUY-CONCORDIA" ,"ST HENRI-BOURASSA" ,"ST HONORE-BEAUGRAND" ,"ST JARRY" ,"ST JEAN-DRAPEAU" ,"ST JEAN-TALON" ,"ST JOLICOEUR" ,"ST JOLIETTE" ,"ST LANGELIER" ,"ST LASALLE" ,"ST LAURIER" ,"ST LIONEL-GROULX" ,"ST LONGUEUIL" ,"ST LUCIEN-L'ALLIER" ,"ST MCGILL" ,"ST MONK" ,"ST MONTMORENCY" ,"ST MONT-ROYAL" ,"ST NAMUR" ,"ST OUTREMONT" ,"ST PAPINEAU" ,"ST PARC" ,"ST PEEL" ,"ST PIE-IX" ,"ST PLACE-D'ARMES" ,"ST PLACE-DES-ARTS" ,"ST PLACE-SAINT-HENRI" ,"ST PLAMONDON" ,"ST PREFONTAINE" ,"ST RADISSON" ,"ST ROSEMONT" ,"ST SAINT-LAURENT" ,"ST SAINT-MICHEL" ,"ST SAUVE" ,"ST SHERBROOKE" ,"ST SNOWDON" ,"ST SQUARE-VICTORIA" ,"ST U-DE-MONTREAL" ,"ST VENDOME" ,"ST VERDUN" ,"ST VIAU" ,"ST VILLA-MARIA" ,"STAT. INC. CHAMBLY" ,"STAT. INC. CHATEAUGUAY" ,"STAT. INC. CHEVRIER" ,"STAT. INC. G-GAGNE" ,"STAT. INC. LA PRAIRIE" ,"STAT. INC. STE-JULIE" ,"TERM. STAT. MONTARVILLE" ,"TERM. STAT. PANAMA" ,"TERM. STAT. REPENTIGNY" ,"TERM. STAT. ST-EUSTACHE" ,"TERM. STAT. TERREBONNE" ,"TERMINUS FAIRVIEW" ,"TERMINUS S.JEAN"),
                  
                  selected = "ST HENRI-BOURASSA")
    ),
    
    mainPanel(
      textOutput("selected_station"),
      textOutput("periode"),
      plotOutput("graph"),
      tableOutput("results")
    )
  )
)
# Define UI for application that draws a histogram

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$selected_station <- renderText({ 
    paste("Pour la station", input$station)
  })
  
  output$periode <- renderText({ 
    paste("et pour la periode",
          input$Periode)
  })
  
  output$graph <- renderPlot({
  Matrice_mode_avant_apres<- Matrice_mode_avant_apres_totale[Matrice_mode_avant_apres_totale$noorig==input$station,]
  
  
  if (input$Periode=="24hr") {
    Matrice_mode_avant_apres2 <-aggregate.data.frame(Matrice_mode_avant_apres[c("FEXP")], by=list(mode_avant=Matrice_mode_avant_apres$mode_avant,mode_apres=Matrice_mode_avant_apres$mode_apres),FUN = sum )
    
    }
  
  if (input$Periode=="PPAM") {
    Matrice_mode_avant_apres2 <- Matrice_mode_avant_apres[Matrice_mode_avant_apres$grhre==2,]
  }
   
  Matrice_mode_avant_apres2$grhre<- NULL
  Matrice_mode_avant_apres2$noorig<- NULL
  circos.clear()
  grid.col = c('Auto conducteur' = "gray16",
               'AutoCond' = "gray16",
               'Passager auto' = "grey56",
               'AutoPass' = "grey56",
               'BusSTM'= "dodgerblue2",
               Metro = "deepskyblue2",
               RTL = "darkred",
               STL = "yellow",
               TRAIN="darkorchid4",
               Marche = "seagreen4",
               velo = "olivedrab",
               'exo Sud' = "tan1",
               'exo Nord' = "Brown4"
              )
  
  chordDiagram(Matrice_mode_avant_apres2,grid.col = grid.col, self.link = 2, directional = 1, direction.type = "arrows",keep.diagonal =TRUE)
  
  })  
  

  output$results <- renderTable({ 
  
    Matrice_mode_avant_apres<- Matrice_mode_avant_apres_totale[Matrice_mode_avant_apres_totale$noorig==input$station,]
  
    
    if (input$Periode=="24hr") {
      Matrice_mode_avant_apres2 <-aggregate.data.frame(Matrice_mode_avant_apres[c("FEXP")], by=list(mode_avant=Matrice_mode_avant_apres$mode_avant,mode_apres=Matrice_mode_avant_apres$mode_apres),FUN = sum )
      
    }
    
    if (input$Periode=="PPAM") {
      Matrice_mode_avant_apres2 <- Matrice_mode_avant_apres[Matrice_mode_avant_apres$grhre==2,]
    }
    
    
    
    Matrice_mode_avant_apres3 <- spread(Matrice_mode_avant_apres2,key=mode_apres, value=FEXP)
    Matrice_mode_avant_apres3[is.na(Matrice_mode_avant_apres3)] <- 0
    Matrice_mode_avant_apres3$grhre <- NULL
    Matrice_mode_avant_apres3$noorig <- NULL
    Matrice_mode_avant_apres3
   
  })  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

