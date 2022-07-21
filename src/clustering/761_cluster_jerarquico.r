#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")


setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./datasets/paquete_premium.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202003  & foto_mes<=202011 & foto_mes!=202006, ]
gc()

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "mdescubierto_preacordado_delta3", "ctrx_quarter_rank", "mprestamos_personales_ratioavg6	", 
                     "rf_011_101", "mcaja_ahorro_ratiomax12",
                     "rf_014_108", "rf_034_091", "rf_043_093", "mtarjeta_visa_consumo", "cproductos_ratioavg6",
                     "mprestamos_personales_lag4", "mprestamos_personales_ratioavg9", "rf_050_071", "cproductos_ratioavg12", 
                     "rf_021_110",
                     "rf_017_101", "mcuentas_saldo_delta2", "ccallcenter_trx_ratioavg12", "mpayroll", "rf_058_114",
                     "foto_mes", "rf_021_140", "mdescubierto_preacordado_tend6", "rf_010_100",
                     "cpayroll_trx_ratioavg6", "mpayroll_delta5", "mrentabilidad_avg9", "ccomisiones_mantenimiento_ratioavg9",
                     "mcuentas_saldo_ratiomax3", "thomebanking_ratioavg9", "Visa_fechaalta_tend3", "rf_015_103", "rf_013_116",
                     "mcomisiones_mantenimiento_delta6", "Visa_Finiciomora", "mcuentas_saldo_rank", "rf_051_108", "mactivos_margen_min3",
                     "rf_047_152")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#primero, creo la carpeta donde van los resultados
dir.create( "./exp/", showWarnings= FALSE )
dir.create( "./exp/ST7610", showWarnings= FALSE )
setwd( "~/buckets/b1/exp/ST7610" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

#dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
#dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
#dataset[  , mean(mcuentas_saldo),  cluster2 ]
#dataset[  , mean(chomebanking_trx),  cluster2 ]
