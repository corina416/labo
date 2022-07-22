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


setwd( "gs://bukalar/exp/8134FEa/" )  #cambiar por la carpeta local

#leo el dataset
dataset  <- fread( "./paquete_premium_ext.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202003  & foto_mes<=202011 & foto_mes!=202006, ]
gc()

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter_rank", "ctrx_quarter", "mtarjeta_visa_consumo_rank", 
                     "mcaja_ahorro_rank", "ctrx_quarter_ratioavg12",
                     "mcaja_ahorro", "mtarjeta_visa_consumo", "ctrx_quarter_ratiomax12", "mcuentas_saldo_rank", 
                     "mtarjeta_visa_consumo_min3","mcaja_ahorro_avg3", "cpayroll_trx", "cproductos_ratioavg6", 
                     "ctarjeta_visa_trx_ratioavg12","ctarjeta_visa_trx_ratiomax12",
                     "mtarjeta_visa_consumo_ratioavg12", "mdescubierto_preacordado_ratioavg9", 
                     "ctarjeta_visa_ratioavg6", "ctrx_quarter_lag6", "ctarjeta_visa_trx",
                     "mpasivos_margen", "cproductos_ratioavg9", "mdescubierto_preacordado_tend6", "mcuentas_saldo",
                     "cpayroll_trx_min3", "mcuenta_corriente_delta6", "mpasivos_margen_avg6", "mpayroll_rank",
                     "mprestamos_personales_ratioavg6", "mdescubierto_preacordado_ratioavg12", "mprestamos_personales_ratioavg12",
                     "mprestamos_personales_ratiomax12", "cproductos_ratioavg12",
                     "mcomisiones_mantenimiento_ratioavg12", "mdescubierto_preacordado_delta4", "mpayroll", "cpayroll_trx_ratioavg6", 
                     "mcaja_ahorro_ratioavg12",
                     "mdescubierto_preacordado_tend3")



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
