library(shinythemes)
library(shiny)
library(dplyr)

calculo<-function(ci){
        
        aux2= as.numeric(strsplit(as.character(ci), "")[[1]])
        
        if (!any(is.na(aux2))) {
            
            if (length(aux2)<8){
                
                ceros=8-length(aux2)
                CI.def=paste0(c(rep(0,ceros),ci),collapse = "")} else {CI.def=ci}}
    
    aux=c(2,9,8,7,6,3,4)
    CI.comp= as.numeric(strsplit(as.character(CI.def), "")[[1]])
    CIsinverf= head(CI.comp,7)
    dv1= sum(CIsinverf*aux) 
    dv2= dv1/10
    dv3= ceiling(dv2)
    dv4= dv3*10
    dv.def=dv4-dv1
    
    if (dv.def == tail(CI.comp,1)) {
        print("CI válida")} else print(paste0("CI no válida. Dígito verificador válido: ",dv.def))
        
}


ui <- fluidPage( theme = shinytheme("cosmo"),
    
    titlePanel("Verificador de documento de identidad"),
    numericInput("ci","Ingresar documento sin puntos ni guiones:",value=0),
    textOutput("resultado"),
    actionButton("verificar","Verificar")
    )


server <- function(input, output, session) {
    
    output$resultado <- renderText(string())
    string <- eventReactive(input$verificar,{calculo(input$ci)})
    
}


shinyApp(ui = ui, server = server)

