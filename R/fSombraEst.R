fSombraEst<-function(angGen, distances, struct)
{
    stopifnot(is.list(struct),is.data.frame(distances))
    ## Preparo datos de partida
    dist <- with(struct, distances/L)
    Alfa <- angGen$Alfa
    Beta <- angGen$Beta
    AlS <- angGen$AlS
    AzS <- angGen$AzS
    cosTheta <- angGen$cosTheta
    h <- dist$H                   #Debe estar previamente normalizada
    d <- dist$D                   
    ## Cálculos
    s=cos(Beta)+cos(Alfa-AzS)*(sin(Beta)+h)/tan(AlS)
    FC=sin(AlS)/sin(Beta+AlS)
    SombraCond=(s-d>0)
    FS=(s-d)*SombraCond*FC*(cosTheta>0)
    ## Resultado
    FS=FS*(FS>0)
    FS[FS>1]<-1
    return(FS)
}


