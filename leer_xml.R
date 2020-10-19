#Leer diccionario xml
require(XML)
arch = wd

#Convertimos el documento en un xmldocument
doc <- xmlTreeParse(arch) #,getDTD=T,addAttributeNamespaces=T)
#nos quedamos con el contenido del documento
arriba = xmlRoot(doc)
child <- xmlChildren(arriba,addNames=TRUE)

info <- c("level","pos","pol","std","word","polarity")
info.basic <- c("word","polarity")

#para cada level
for(i in 1:xmlSize(arriba)) 
{ 
  #para positive y negative  
  for(j in 1:2)
  {
    #para cada lema
    print(i)
    print(j)
    
    for(h in 1:xmlSize(child[[i]][[j]]))
    {
      attChild <- xmlAttrs(child[[i]][[j]][[h]])
      attValue <- xmlValue(child[[i]][[j]][[h]])
      
      if(j==1){
        variable <- c(i,attChild[1],attChild[2],attChild[3],attValue,"positive")
        variable.basic <- c(attValue,"positive")
      }
      else{
        variable <- c(i,attChild[1],attChild[2],attChild[3],attValue,"negative")
        variable.basic <- c(attValue,"negative")
      }
      info <- rbind(info,variable)
      info.basic <- rbind(info.basic,variable.basic)
      
    }
    
  }
}

rm(arriba)
rm(doc)
rm(child)
rm(arch)
rm(attChild)
rm(attValue)
rm(h)
rm(i)
rm(j)
rm(variable)
rm(variable.basic)