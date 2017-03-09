
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Crea tabla de resumen descriptivo para variables numéricas
#'
#' Toma un data frame, una lista de noombres de variables numéricas, una traducción de los nombres de las variables a descripciones más
generaTablaDescriptivaNumericas=function(df,vNumericas,traduccion,columnas=c("n","mediaet","gauss","rango","out3SD","out5SD")){
  listaLineas=lapply(vNumericas,function(vNum)desc1vn(df,vNum)[columnas])
  longitud=sapply(listaLineas,length)
  listaLineasOk=listaLineas[longitud==max(longitud)]
  tablaRes=Reduce(rbind,listaLineasOk) %>%
    mutate(Variable=traduccion) %>%   select(Variable, everything())
  tablaRes}



generaTablaDescriptivaNumericasPorTiempo=function(df,TiempoFactor,vNumericas,traduccion,columnas=c("n","mediaet","gauss","rango","out3SD","out5SD")){
  names(traduccion)=vNumericas
  tablaRes=Reduce(rbind,lapply(vNumericas, function(vNum)Reduce(rbind,lapply(
    split(df,df[,c(TiempoFactor)]),
    function(df)desc1vn(df,vNum)[,columnas])) %>%
      mutate(Tiempo=levels(df[,TiempoFactor])) %>%
      select(Tiempo,everything()) %>%
      mutate(Variable=traduccion[vNum]) %>%
      select(Variable, everything()))
  )
  tablaRes
}





generaTablatTestPorTiempoGrupo=function(df,TiempoFactor,vGrupo,vNumericas,traduccion,columnas=c("n","mediaet")){
  names(traduccion)=vNumericas
  tablaRes=Reduce(rbind,
                  lapply(vNumericas,FUN=function(vNum)
                    Reduce(rbind, lapply(
                      split(df,df[,TiempoFactor]),
                      FUN = function(dfTiempo)
                        Reduce(cbind, append(lapply(
                          split(dfTiempo,dfTiempo[,vGrupo]),
                          FUN = function(dfGr){
                            res=desc1vn(dfGr, vNum)[, columnas]
                            names(res)=sprintf("%s.%s",names(res),as.character(dfGr[1,vGrupo]))
                            res
                          }
                        ),#AQUI VA LA LISTA t-test
                        list(descTtest(dfTiempo,vNum,vGrupo)[,c("p.t","ci95")])
                        )#Aquí termina t´test
                        )
                    ))%>% mutate(Variable=traduccion[vNum],
                                 Tiempo=levels(dfLong[,TiempoFactor])) %>% select(Variable,Tiempo,everything())
                  )
  )
  tablaRes
}




generaTablaBinariasPorTiempoGrupo=function(df,TiempoFactor,vGrupo,vBinarias,traduccion){
  names(traduccion)=vBinarias
  tablaRes = Reduce(rbind,
                    lapply(vBinarias,
                           function(vFila)
                             Reduce(rbind,
                                    lapply(
                                      split(df, list(df[, TiempoFactor]))
                                      , function(dfx) desc2x2(dfx, vFila, vGrupo)
                                    )
                             ) %>% mutate(Variable=traduccion[Variable],Tiempo=levels(df[,TiempoFactor]))
                    )
  )%>%
    select(Variable, Tiempo, everything())
  tablaRes
}



























desc1vn=function(df,vNum,formato="%1.1f±%1.1f",formatoIntervalo="%1.1f-%1.1f"){
  shapiro50="-"
  shapiro.p=1
  datos=df[!is.na(df[,vNum]),vNum]
  resultado=data.frame("Fallo"=1,"n"=0,"media"=NA,"mediasd"="", "mediadt"="","mediaet"="","medianaRI"="","rango"="","gauss"="","out3SD"=NA,"out5SD"=NA,"shapiro"="","p25"=NA,"p50"=NA,"p75"=NA,"p.intra"="")

  if(length(datos)>0){
    if(length(datos)>4){
      set.seed(10)
      try({
        shapiro50.p=shapiro.test(sample(datos,min(40,length(datos))))$p.value
      })
      subdatos=datos
      if(length(datos)>4900) subdatos=subdatos[1:4900]
      try({
        shapiro.p=shapiro.test(subdatos)$p.value
        shapiro50=c("No","Ok")[1+as.integer(max(shapiro50.p,shapiro.p)>0.01)]
      })
    }
    zdatos=abs(scale(datos))
    zout3SD=sum(zdatos>3,na.rm=T)
    zout5SD=sum(zdatos>5,na.rm=T)

    resultado=df %>% mutate(numerica=df[,vNum]) %>% summarise(
      n=length(datos),
      media=mean(numerica,na.rm=T),
      dt=sd(numerica,na.rm=T),
      et=dt/sqrt(n-1),
      minimo=min(numerica,na.rm=T),
      maximo=max(numerica,na.rm=T),
      p25=round(quantile(numerica,.25,na.rm=T),1),
      p50=round(quantile(numerica,.50,na.rm=T),1),
      p75=round(quantile(numerica,.75,na.rm=T),1),
      mediadt=sprintf(formato,media,dt),
      mediaet=sprintf(formato,media,et),
      medianaRI=sprintf(formato,p50,p75-p25),
      rango=sprintf(formatoIntervalo,minimo,maximo),
      gauss=shapiro50,
      shapiro=pvalores(shapiro.p),
      out3SD=zout3SD,
      out5SD=zout5SD,
      p.intra=pvalores(2*pt(abs(media/et),n-1,lower.tail = F))
    )
  }
  resultado
}


descTtest=function(df,vNum,vFac,formato="%1.2f"){
  res=data.frame("Error"=1,p.t="-","t"="-","dif"="-","ci_min"=NA,"ci_max"=NA,"ci95"="-")
  try({
    modelo=t.test(formula(sprintf("%s ~ %s",vNum,vFac)),data=df)
    res=data.frame("p.t"=pvalores(modelo$p.value),
                   "t"=round(modelo$parameter,2),
                   "dif"=modelo$estimate[2]-modelo$estimate[1],
                   "ci_min"=modelo$conf.int[1],
                   "ci_max"=modelo$conf.int[2],
                   "ci95"=sprintf(sprintf("%s[%s,%s]",formato,formato,formato),modelo$estimate[1]-modelo$estimate[2],modelo$conf.int[1],modelo$conf.int[2])
    )
  })
  rownames(res)=NULL
  res
}


descAnova1F=function(df,vNum,vFac,formato="%1.2f"){
  res=data.frame("Error"=1,"p.F"="-","F"="-","p.kw"="-","gauss"="-","shapiro"="-")
  shapiro50=shapiro.p=p.kw=NA

  laFormula=formula(sprintf("%s ~ %s",vNum,vFac))
  modelo=summary(lm(laFormula,data=df))
  datos=modelo$residuals[!is.na(modelo$residuals)]


  if(length(datos)>0){
    if(length(datos)>4){
      set.seed(10)
      try({
        shapiro50.p=shapiro.test(sample(datos,min(40,length(datos))))$p.value
      })
      subdatos=datos
      if(length(datos)>4900) subdatos=subdatos[1:4900]
      try({
        shapiro.p=shapiro.test(subdatos)$p.value
        shapiro50=c("No","Ok")[1+as.integer(max(shapiro50.p,shapiro.p)>0.01)]
      })
    }
  }
  try ({
    kw=kruskal.test(laFormula, data = df)
    p.kw=kw$p.value
  })

  try({
    res=data.frame("p.F"=pvalores(1-pf(modelo$fstatistic[1],modelo$fstatistic[2],modelo$fstatistic[3])),
                   "F"=round(modelo$fstatistic[1],2), "p.kw"=pvalores(p.kw),"gauss"=shapiro50,"shapiro"=pvalores(shapiro.p))
  })
  rownames(res)=NULL
  res
}






descAnova1Fijo1Random=function(df,vCodigo,vNum,vFacFijo,vFacRandom){
  nFilas=length(unique(df[,vFacRandom]))
  #Por si falla ANOVA
  res=data.frame(
    "F.FacFijo"=rep("-",nFilas),
    "p.FacFijo"=rep("-",nFilas),
    "F.FacRandom"=rep("-",nFilas),
    "p.FacRandom"=rep("-",nFilas),
    "F.Inter"=rep("-",nFilas),
    "p.inter"=rep("-",nFilas)
  )

  try({
    modelo=lme(formula(sprintf("%s ~ %s*%s", vNum,vFacFijo,vFacRandom)),random= formula(sprintf("~1|%s",vCodigo)) , correlation=corCompSymm(form=formula(sprintf("~1|%s",vCodigo))),data=df[complete.cases(df[,c(vCodigo,vFacFijo,vFacRandom,vNum)]),])
    analisis=anova(modelo)


    res=data.frame(
      "F.FacFijo"=rep(sprintf("F(%d,%d)=%1.1f",analisis$numDF[2],analisis$denDF[2],analisis[["F-value"]][2]),nFilas),
      "p.FacFijo"=rep(pvalores(analisis[["p-value"]][2]),nFilas),
      "F.FacRandom"=rep(sprintf("F(%d,%d)=%1.1f",analisis$numDF[3],analisis$denDF[3],analisis[["F-value"]][3]),nFilas),
      "p.FacRandom"=rep(pvalores(analisis[["p-value"]][3]),nFilas),
      "F.Inter"=rep(sprintf("F(%d,%d)=%1.1f",analisis$numDF[4],analisis$denDF[4],analisis[["F-value"]][4]),nFilas),
      "p.inter"=rep(pvalores(analisis[["p-value"]][4]),nFilas)
    )

  })
  rownames(res)=NULL
  names(res)=paste(c("F","p","F","p","F","p"),c(vFacFijo,vFacFijo,vFacRandom,vFacRandom,"Interaccion","Interaccion"),sep=".")
  res=res %>% mutate(Variable=vNum,Tiempo=unique(df$Tiempo)) %>% select(Variable, Tiempo,everything())
  res
}

descMWtest=function(df,vNum,VFac){
  modelo=wilcox.test(formula(sprintf("%s ~ %s",vNum,vFac)),data=df)
  data.frame("p.w"=pvalores(modelo$p.value),
             "w"=modelo$statistic
  )
}



lineaNumerica=function(df,vNum,vFac,descriptoresDesc=c("n","mediaet"),descriptoresInf=c("p.t","ci95")){
  extra_t=data.frame("Vacio"=1)
  bloques1f=lapply(split(df,df[,c(vFac)]),FUN = function(df){desc1vn(df,vNum)[,descriptoresDesc]})
  try({
    extra_t=descTtest(df,vNum,VFac,"%1.3f")[descriptoresInf]
  })
  res=Reduce(cbind,list(Reduce(cbind,bloques1f),extra_t))
  cbind(Variable=vNum,res)
}


desc2x2=function(df,vFila,vCol,fila=2 ){
  tabla=xtabs( formula(sprintf("~ %s+ %s" ,vFila,vCol)),data=df)
  tabla.total=addmargins(tabla,2)
  tabla.total.total=addmargins(tabla.total,1)
  tabla.prop=prop.table(tabla.total,2)
  res=data.frame(C1="",C2="",C3="")
  names(res)=dimnames(tabla.total)[[2]]
  res[1,]=sprintf("%1.3f(%s/%s)",tabla.prop[2,],tabla.total.total[2,],tabla.total.total[3,])
  res$Variable=vFila;res$p="";res$dif="";res$RR="";#res$OR=""
  try({
    res.dif=binomMeld.test(tabla[2,1],tabla.total.total[3,1],tabla[2,2],tabla.total.total[3,2],conf.int = T,parmtype = "difference")
    res.rr=binomMeld.test(tabla[2,1],tabla.total.total[3,1],tabla[2,2],tabla.total.total[3,2],conf.int = T,parmtype = "ratio")
    #   res.or=binomMeld.test(tabla[2,1],tabla.total.total[3,1],tabla[2,2],tabla.total.total[3,2],conf.int = T,parmtype = "odds")


    res=res %>% mutate(p=pvalores(res.dif$p.value),
                       dif=sprintf("%1.3f[%1.3f-%1.3f]",res.dif$estimate,res.dif$conf.int[1],res.dif$conf.int[2]),
                       RR=sprintf("%1.3f[%1.3f-%1.3f]",res.rr$estimate,res.rr$conf.int[1],res.rr$conf.int[2])
                       #OR=sprintf("%1.3f[%1.3f-%1.3f]",res.or$estimate,res.or$conf.int[1],res.or$conf.int[2])
    )
  })
  res=res %>% select(Variable,everything())
  rownames(res)=NULL
  res
}
