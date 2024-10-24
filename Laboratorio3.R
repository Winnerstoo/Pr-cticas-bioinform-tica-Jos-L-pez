library(ape)
library(phangorn)
library(phytools)
#Vamos a hacer el archivo compatible con R:
fraxatin <- read.phyDat(file = "fraxatin_aligned.fasta.txt", 
                        format = "FASTA", type = "AA")
fraxatin
#Transformamos el objeto fraxatin el tipo de clase debe ser AAbin 
matrizdist <- as.AAbin(fraxatin)
matrizdist <- dist.aa(matrizdist)
matrizdist
#Vamos a hacer un árbol con el método UPGMA
#Si la longitud de dos ramas es indéntica, significa que las secuancias también son indénticas y en la matriz de distancia la diferencia es de 0.
arbolUPGMA <- upgma(matrizdist)
plot(arbolUPGMA)
#Árbol método NJ:
arbolNJ <- nj(matrizdist)
plot(arbolNJ)
#Podemos personalizarlos:
plot(arbolUPGMA, type= "p", cex=0.8, edge.width=2, edge.color="red", font=3)

plot(arbolUPGMA, type= "c", cex=0.8, edge.width=2, edge.color="blue", font=3)
#Ahora el árbol está centrado
plot(arbolUPGMA, type= "p", label.offset=0,0005, edge.lty=1, node.pos=2, cex=0.8, edge.width=2, edge.color="black", font=3)
#Las líneas se pueden cambiar con el comando "edge.lty":
plot(arbolUPGMA, type= "p", label.offset=0.0005, edge.lty=2, node.pos=2, cex=0.8, edge.width=2, edge.color="black", font=3)
#Las ramas se pueden mover con el comando "node.pos":
plot(arbolUPGMA, type= "p", label.offset=0.0005, edge.lty=1, node.pos=2, cex=0.8, edge.width=2, edge.color="black", font=3)
#Se puede graficar también con phytool con un plot:
plotTree(arbolNJ)
#También podemos modificar:
plotTree(arbolNJ, ftype="b", fsize=0.8, offset=1, color="red", lwd=2)
#Podemos ordenar las ramas por orden alfabético o escalerizar las ramas:
plotTree(ladderize(arbolNJ))
#Para guardar un árbol usamos:
write.tree(arbolNJ, file = "arbolNJ.nex")
#Para leer el archivo usamos:
read.tree(file = "arbolNJ.nex")
#Para enraizar el arbol utilizamos la fución root y el parámetro outgroup:
arbolNJraiz <-root(arbolNJ, outgroup = "Ornitorrinco", r = TRUE)
plot(arbolNJraiz)
#También lo podemos utilizar con el método UPGMA:
arbolUPGMAraiz <-root(arbolUPGMA, outgroup = "Ornitorrinco", r=TRUE)
plot(arbolUPGMAraiz)
#Además podemos visualizar los dos árboles a la vez con los siguientes comandos:
layout(matrix(c(1,2)), height=c(10,10))
par(mar=c(1,1,1,1))
plot(arbolUPGMAraiz, label.offset=0.0005, main="ARBOL UPGMA", cex=0.4)
plot(arbolNJraiz, label.offset=0.0005, main="ARBOL NJ", cex=0.4)
#Árboles de parsimonia:
#Contamos el número de pasos de el árbol con raiz:
parsimony(arbolUPGMAraiz, fraxatin)
#Contamos el número de pasos de el árbol sin raiz:
parsimony(arbolUPGMA, fraxatin)
#Árbol con mejor parsimonia:
mejorUPGMA <- optim.parsimony(arbolUPGMAraiz, fraxatin)
#Ahora con el árbol NJ:
mejorNJ <- optim.parsimony(arbolNJraiz, fraxatin)
#Otra estrategia para hacer el proceso de búsqueda de árbol con mayor parsimonia es con el algoritmo de búsqueda pratchet
fraxatin_parsimonia <- pratchet(fraxatin, all = TRUE)
#El algoritmo ha encontrado múltiples ocasiones que el árbol más corto es de 307 pasos pero sólo 4 árboles con igual longitud y número de pasos aunque presenten diferente topología.
fraxatin_parsimonia
#Para poderlos comparar es necesario enraizarlos:
fraxatin_parsimoniaR <- root(phy = fraxatin_parsimonia, outgroup = "Ornitorrinco")
plot(fraxatin_parsimoniaR, cex = 0.6)
#Vamos a hacer un árbol consenso con el parámetro p=1:
estrictode100 <- consensus(fraxatin_parsimoniaR, p = 1)
plot(estrictode100, cex = .6)
#Para hacer un árbol menos estricto podemos bajar el valor de p:
estrictode30 <- consensus(fraxatin_parsimoniaR, p = 0.3)
plot(estrictode30, cex = .6)
#Utilizamos bootstrap para ver cual es el árbol con menos cambios; ese es el mejor:
arbolesbootstrap <- bootstrap.phyDat(fraxatin, FUN = pratchet, bs = 10)
# 10 árboles pseudoréplicas:
plot(arbolesbootstrap, cex = .6)
#Generamos un consenso al 60% (p=0,6):
estricto60 <- consensus(arbolesbootstrap, p = 0.6)
plot(estricto60, cex = .6)
#Verosimilitud máxima de un árbol (probabilidad de obtener un dato según un modelo)
#Vamos a hacer un árbol al azar de 11 ramas, clase phy:
arbolazar <- rtree(n = 11, tip.label = names(fraxatin))
plot(arbolazar, cex = .5)
#Escalamos y enraizamos:
arbolazarR <- root(phy = arbolazar, outgroup = "Ornitorrinco")
plot(ladderize(arbolazarR), cex = .5); add.scale.bar()
#Con pml (Phylogenetic maximum likelihood), podemos computar tal verosimilitud:
ajustado <- pml(arbolazarR, fraxatin)
ajustado
#Lo que hay que hacer es encontrar un árbol que optimice la verosimilitud usando un modelo de sustitución; para esto vamos a usar el método optim.pml del paquete phangorn
ajustadoconDay <- optim.pml(object = ajustado, model = "Dayhoff", rearrangement = "ratchet")
#Para ver el árbol oculto usamos $tree:
ajustadoconDay$tree
#También lo enraizamos:
ajustadoconDayraíz <- root(ajustadoconDay$tree, outgroup = "Ornitorrinco")
plot(ladderize(ajustadoconDayraíz), cex = .5); add.scale.bar()
#Existen otros modelos:
ajustadoconBlo <- optim.pml(object = ajustado, model = "Blosum62", rearrangement = "ratchet")

ajustadoconJTT <- optim.pml(object = ajustado, model = "JTT", rearrangement = "ratchet")
#Comparamos los modelos con:
AIC(ajustadoconDay, ajustadoconBlo, ajustadoconJTT)
#El mejor modelo que se ajusta con los datos (con el AIC más bajo) es JTT modelo de Jones-Taylor-Thornton para evaluar la distancia entre secuencias de proteínas y optimiza la verosimilitud
mejorarbol <- optim.pml(
  object = ajustadoconDay, 
  model = "JTT", 
  rearrangement = "ratchet")
#Este es el mejor árbol:
mejorarbol

mejorarbolR <- root(mejorarbol$tree, outgroup = "Ornitorrinco")
plot(ladderize(mejorarbolR), cex = 0.5); add.scale.bar()
