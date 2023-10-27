library(shiny)
library(shinythemes)
library(DT)
library(dplyr)

calculo.mult<-function(ci){

    CI<-unique(ci[,1])
    listado<-ci
    aux=c(2,9,8,7,6,3,4)
    aux2=NULL
    CI.def=vector()
    listado$CIdef<-c(NA)
    listado$verificador.val<-c(NA)
    listado$verificador.real<-c(NA)
    listado$diferencia<-c(NA)
    listado$Resultado<-c(NA)
    
    for (i in 1:length(CI)) {
        
        aux2= as.numeric(strsplit(as.character(CI[i]), "")[[1]])
        
        if (!any(is.na(aux2))) {
            
            if (length(aux2)<8){
                
                ceros=8-length(aux2)
                CI.def[i]=paste0(c(rep(0,ceros),CI[i]),collapse = "")
                
            } else { CI.def[i]=CI[i] }
            
            CI.comp= as.numeric(strsplit(as.character(CI.def[i]), "")[[1]])
            CIsinverf= head(CI.comp,7)
            dv1= sum(CIsinverf*aux) 
            dv2= dv1/10
            dv3= ceiling(dv2)
            dv4= dv3*10
            
            dv.def=dv4-dv1
            listado$CIdef[i]<-CI.def[i]
            listado$diferencia[i]=dv.def-tail(CI.comp,1)
            listado$verificador.val[i]= dv.def
            listado$verificador.real[i]= tail(CI.comp,1)}
        
        else {listado$diferencia[i]=99}
        
    }
    
    
    listado$Resultado=ifelse(listado$diferencia==0,"Válida","No válida")
    
    invalidas<-listado %>% 
        filter(diferencia!=0 | is.na(diferencia)) %>% 
        select(V1,verificador.real,verificador.val,Resultado) %>% 
        rename(Documento=V1,`Verificador ingresado`=verificador.real,`Verificador válido`=verificador.val)
    
    validas<-listado %>% 
        filter(diferencia==0) %>% 
        select(V1,verificador.real,verificador.val,Resultado) %>% 
        rename(Documento=V1,`Verificador ingresado`=verificador.real,`Verificador válido`=verificador.val)
    
    listado<-listado %>% 
        select(V1,verificador.real,verificador.val,Resultado) %>% 
        rename(Documento=V1,`Verificador ingresado`=verificador.real,`Verificador válido`=verificador.val)
    
    return(listado)
}


# 

ui <- fluidPage( theme = shinytheme("cosmo"),
    
    titlePanel("Verificador de documento de identidad"),
    sidebarLayout(
        sidebarPanel( 
            helpText("Ingresa un listado de documentos de identidad en formato .txt para su verificación"),
            br(),
            fileInput("ci",label = "Listado con CI",buttonLabel = "Cargar",
              placeholder = "Cargar archivo"),
            br(),
            checkboxGroupInput("res","Mostrar:", choices=c("Válida", "No válida"),
                selected=NULL),
            br(),
            textOutput("clasf"),
            tags$head(tags$style("#clasf{font-size: 15px;
                                 font-weight: bold;
                                 }")),
            br(),
            br(),
            downloadButton("dwld","Descargar selección")),
    mainPanel(dataTableOutput("listadot")))
)

server <- function(input, output, session) {
    
    dataci <-reactive({
        data<-req(input$ci)
        read.table(data$datapath,sep="")
        })
    
    total<-reactive({calculo.mult(dataci())})
    
    val<-reactive({dim(total() %>% filter(Resultado=="Válida"))[1]})
    
    inval<-reactive({dim(total() %>% filter(Resultado=="No válida"))[1]})
    
    leyenda<-reactive({
        paste0("Se encontraron ",val()," cédulas válidas y ",inval()," cédulas inválidas")})
    
    output$clasf<-renderText({leyenda()})
    
    listado<- reactive({ total() %>% filter(Resultado %in% input$res)})
    
    output$listadot<- renderDataTable({
        datatable(listado(),
                  options=list(paging = TRUE,    ## paginate the output
                               pageLength = 15,  ## number of rows to output for each page
                               scrollY = TRUE,   ## enable scrolling on Y axis
                               autoWidth = TRUE),
                  rownames = F)})
    
    output$dwld <- downloadHandler(
        
        filename = function() {
            if (length(input$res)==1) {
                paste("Listado_",input$res, ".xlsx", sep = "")    
            } else {if (length(input$res)>1){
                paste("Listado_completo",".xlsx",sep="")
            }}},
        
        content = function(file) {
            writexl::write_xlsx(listado(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)