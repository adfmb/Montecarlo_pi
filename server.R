
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(reshape2)
library(ggplot2)
require(gridExtra)

montecarlo_1<-function(nsim,alpha){
  
  x<- runif(nsim,0,2)
  phi<-2*sqrt(4-x^2)
  thetaF<-mean(phi)
  zetha<-qnorm(alpha/2,lower.tail = FALSE)
  Scuadrada<-var(phi)
  limsup<-thetaF + zetha*sqrt(Scuadrada/nsim)
  liminf<-thetaF - zetha*sqrt(Scuadrada/nsim)
  
  return(data.frame(est=thetaF, LimiteSuperior=limsup, LimiteInferior=liminf))
  
}

montecarlo_2<-function(nsim,alpha){
  
  x<- runif(nsim,0,1)
  phi<-4/(1+x^2)
  thetaF<-mean(phi)
  zetha<-qnorm(alpha/2,lower.tail = FALSE)
  Scuadrada<-var(phi)
  limsup<-thetaF + zetha*sqrt(Scuadrada/nsim)
  liminf<-thetaF - zetha*sqrt(Scuadrada/nsim)
  
  return(list(est=thetaF, LimiteSuperior=limsup, LimiteInferior=liminf))
  
}

montecarlo_3<-function(nsim,alpha){
  
  x<- runif(nsim,0,1)
  phi<-(6/sqrt(4-x^2))
  thetaF<-mean(phi)
  zetha<-qnorm(alpha/2,lower.tail = FALSE)
  Scuadrada<-var(phi)
  limsup<-thetaF + zetha*sqrt(Scuadrada/nsim)
  liminf<-thetaF - zetha*sqrt(Scuadrada/nsim)
  
  return(list(est=thetaF, LimiteSuperior=limsup, LimiteInferior=liminf))
  
}

funcion_plot<-function(X,y1,y2,y3){

  plot(x=X,y=y1,type="l")
  lines(x=X,y=y2,col="green4") 
  lines(x=X,y=y3,col="green4") 
  abline(h=pi,col="red4")  
  
}


funcion_plot0<-function(X,y1){
  
  plot(x=X,y=y1,type="l")
  abline(h=pi,col="red4")  
  
}

function_lines<-function(p,X,yi){
  
  lines(x=X,y=yi,col="green4") 
  
}



shinyServer(function(input, output) {
  
  alpha<-reactive(input$alpha)
  MuestraMinima<-reactive(input$MinM)
  NumSimulaciones<-reactive(input$NSimulaciones)
  brincosSim<-reactive(10)
  MuestraMax<-reactive(MuestraMinima()+(brincosSim()*(NumSimulaciones()-1)))

  N<-reactive(seq(MuestraMinima(),MuestraMax(),by=brincosSim()))
  
  a_1<-reactive(sapply(N(),alpha(),FUN=montecarlo_1))
  a_2<-reactive(sapply(N(),alpha(),FUN=montecarlo_2))
  a_3<-reactive(sapply(N(),alpha(),FUN=montecarlo_3))
  b_1<-reactive(data.frame(t(a_1())))
  
  
  output$plot1 <- renderPlot({
    
    #a1<-a_1()
    qf<-function(x=seq(-2,2,.005)){
      
      sqrt(4-x^2)
      
    }
    
    qf_neg<-function(x=seq(-2,2,.005)){
      
      -1*sqrt(4-x^2)
      
    }
    
    cord.x <- c(0,seq(0,2,0.01),2) 
    cord.y <- c(0,qf(seq(0,2,0.01)),0) 
    curve(qf,ylab="f(x)=(1/2)*sqrt(4-x^2)",xlim=c(-4,4),ylim=c(-3,3),main='Aproximando el Area sombreada:
Integral 1')
    curve(qf_neg,add=TRUE)
    polygon(cord.x,cord.y,col='skyblue')
    legend(2,2.8,paste("Aprox. del area
en la ultima simulacion: ", round(as.numeric(b_1()[[1]][length(N())]),5)),xjust=.5,fill="skyblue")
    
    #round(data.frame(t(a_1))[[1]][length(N)],5)
    #class(data.frame(t(a_1))[[1]][length(N)])
    #round(as.numeric(data.frame(t(a_1))[[1]][length(N)]),5)
    #dist <- input$dist
   
    
    #hist(data(), 
     #    main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  output$plot1_1 <- renderPlot({

    #p<-funcion_plot0(N(),b_1()[,1],b_1()[,2],b_1()[,3])
    #p<-function_lines(function_lines(funcion_plot(N(),b_1()[,1]),N(),b_1()[,2]),N(),b_1()[,3])
    #p
    a<-a_1()
    N<-N()
    e1<-matrix(cbind(as.numeric(a[1,]),as.numeric(a[2,]),as.numeric(a[3,])),ncol=3)
    colnames(e1)=c("Estimador","Limite_Superior","Limite_Inferior")

    e4<-data.frame(N=N,e1,h=rep(1,length(N)))
    e4_1<-data.frame(N=N,e1)
    
    limiteI<-e4_1[-3]
    limiteI<-limiteI[-2]
    
    limiteS<-e4_1[-4]
    limiteS<-limiteS[-2]
    
    esti<-e4_1[-4]
    esti<-esti[-3]
    
    limiteI_2<-limiteI
    limiteS_2<-limiteS
    
    names(limiteI_2)[2]="Limites"
    names(limiteS_2)[2]="Limites"
    
    limites_2<-rbind(limiteI_2,limiteS_2)
    limites_3<-melt(limites_2,id="N")
    
    
    esti_2<-melt(esti,id="N")
    dpi<-data.frame(N=N,Valor_Correcto=pi)
    dpi_2<-melt(dpi,id="N")
    
    tf<-rbind(limites_3,esti_2,dpi_2)
    
    p<-ggplot(tf,aes(x=N,y=value,fill=variable,colour = variable))+geom_line(alpha=.6) +scale_colour_manual(values=c("dodgerblue3","hotpink1","yellow1"))+scale_y_continuous("")+scale_x_continuous("Simulaciones")
    
    p
    
  
  
  })
  
  output$plot2 <- renderPlot({
    

    #Para grfica 2
    hf<-function(x=seq(-5,5,.005)){
      
      4/(1+x^2)
      
    }
    
    cord.x <- c(0,seq(0,1,0.01),1) 
    cord.y <- c(0,hf(seq(0,1,0.01)),0) 
    curve(hf,ylab="f(x)=4/(1+x^2)",xlim=c(-2,3),ylim=c(0,5),main='Aproximando el Area sombreada:
Integral 2')
    #curve(qf_neg,add=TRUE)
    polygon(cord.x,cord.y,col='green4')
    legend(2,4,paste("Aprox. del area
en la ultima simulacion: ", round(as.numeric(data.frame(t(a_2()))[[1]][length(N())]),5)),xjust=.5,fill="green4")
    
  })
  
  output$plot2_1 <- renderPlot({
    
    #p<-funcion_plot0(N(),b_1()[,1],b_1()[,2],b_1()[,3])
    #p<-function_lines(function_lines(funcion_plot(N(),b_1()[,1]),N(),b_1()[,2]),N(),b_1()[,3])
    #p
    a<-a_2()
    N<-N()
    e1<-matrix(cbind(as.numeric(a[1,]),as.numeric(a[2,]),as.numeric(a[3,])),ncol=3)
    colnames(e1)=c("Estimador","Limite_Superior","Limite_Inferior")
    
    e4<-data.frame(N=N,e1,h=rep(1,length(N)))
    e4_1<-data.frame(N=N,e1)
    
    limiteI<-e4_1[-3]
    limiteI<-limiteI[-2]
    
    limiteS<-e4_1[-4]
    limiteS<-limiteS[-2]
    
    esti<-e4_1[-4]
    esti<-esti[-3]
    
    limiteI_2<-limiteI
    limiteS_2<-limiteS
    
    names(limiteI_2)[2]="Limites"
    names(limiteS_2)[2]="Limites"
    
    limites_2<-rbind(limiteI_2,limiteS_2)
    limites_3<-melt(limites_2,id="N")
    
    
    esti_2<-melt(esti,id="N")
    dpi<-data.frame(N=N,Valor_Correcto=pi)
    dpi_2<-melt(dpi,id="N")
    
    tf<-rbind(limites_3,esti_2,dpi_2)
    
    p<-ggplot(tf,aes(x=N,y=value,fill=variable,colour = variable))+geom_line(alpha=.6) +scale_colour_manual(values=c("dodgerblue3","hotpink1","yellow1"))+scale_y_continuous("")+scale_x_continuous("Simulaciones")
    
    p
    
    
    
  })
  
  output$plot3 <- renderPlot({
    

    
    #Para grfica 2
    #Para grfica 3
    gf<-function(x=seq(-1.999999,1.999999,.005)){
      
      (6/sqrt(4-x^2))
      
    }
    
    
    
    cord.x <- c(-0,seq(0,1,0.01),1) 
    cord.y <- c(0,gf(seq(0,1,0.01)),0)
    curve(gf,ylab="f(x)=6/sqrt(4-x^2)",xlim=c(-2.04,1.95),ylim=c(0,30),main='Aproximando el Area sombreada:
Integral 3')
    #curve(qf_neg,add=TRUE)
    polygon(cord.x,cord.y,col="purple")
    legend(1,25,paste("Aprox. del area
en la ultima simulacion: ", round(as.numeric(data.frame(t(a_3()))[[1]][length(N())]),5)),xjust=.5,fill="purple")
    
  })
  
  output$plot3_1 <- renderPlot({
    
    #p<-funcion_plot0(N(),b_1()[,1],b_1()[,2],b_1()[,3])
    #p<-function_lines(function_lines(funcion_plot(N(),b_1()[,1]),N(),b_1()[,2]),N(),b_1()[,3])
    #p
    a<-a_3()
    N<-N()
    e1<-matrix(cbind(as.numeric(a[1,]),as.numeric(a[2,]),as.numeric(a[3,])),ncol=3)
    colnames(e1)=c("Estimador","Limite_Superior","Limite_Inferior")
    
    e4<-data.frame(N=N,e1,h=rep(1,length(N)))
    e4_1<-data.frame(N=N,e1)
    
    limiteI<-e4_1[-3]
    limiteI<-limiteI[-2]
    
    limiteS<-e4_1[-4]
    limiteS<-limiteS[-2]
    
    esti<-e4_1[-4]
    esti<-esti[-3]
    
    limiteI_2<-limiteI
    limiteS_2<-limiteS
    
    names(limiteI_2)[2]="Limites"
    names(limiteS_2)[2]="Limites"
    
    limites_2<-rbind(limiteI_2,limiteS_2)
    limites_3<-melt(limites_2,id="N")
    
    
    esti_2<-melt(esti,id="N")
    dpi<-data.frame(N=N,Valor_Correcto=pi)
    dpi_2<-melt(dpi,id="N")
    
    tf<-rbind(limites_3,esti_2,dpi_2)
    
    p<-ggplot(tf,aes(x=N,y=value,fill=variable,colour = variable))+geom_line(alpha=.6) +scale_colour_manual(values=c("dodgerblue3","hotpink1","yellow1"))+scale_y_continuous("")+scale_x_continuous("Simulaciones")
    
    p
    
    
    
  })
  
})
  