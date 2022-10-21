#Parametros del script
kexperimento  <- "DR7141"

kexp_input  <- "CA7060"

#valores posibles  "ninguno" "rank_simple" , "rank_cero_fijo" , "deflacion"
kmetodo  <- "deflacion"
# FIN Parametros del script


#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  
  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]
  
  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
  
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , vm_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , vm_status02       := Master_status +  Visa_status ]
  dataset[ , vm_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , vm_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , vm_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  
  dataset[ , vm_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]
  
  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]
  
  
  #combino MasterCard y Visa
  dataset[ , vm_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  
  dataset[ , vm_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , vm_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , vm_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , vm_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , vm_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , vm_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , vm_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , vm_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , vm_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , vm_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , vm_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , vm_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , vm_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , vm_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  
  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , vmr_Master_mlimitecompra:= Master_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_Visa_mlimitecompra  := Visa_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_msaldototal         := vm_msaldototal / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos         := vm_msaldopesos / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos2        := vm_msaldopesos / vm_msaldototal ]
  dataset[ , vmr_msaldodolares       := vm_msaldodolares / vm_mlimitecompra ]
  dataset[ , vmr_msaldodolares2      := vm_msaldodolares / vm_msaldototal ]
  dataset[ , vmr_mconsumospesos      := vm_mconsumospesos / vm_mlimitecompra ]
  dataset[ , vmr_mconsumosdolares    := vm_mconsumosdolares / vm_mlimitecompra ]
  dataset[ , vmr_madelantopesos      := vm_madelantopesos / vm_mlimitecompra ]
  dataset[ , vmr_madelantodolares    := vm_madelantodolares / vm_mlimitecompra ]
  dataset[ , vmr_mpagado             := vm_mpagado / vm_mlimitecompra ]
  dataset[ , vmr_mpagospesos         := vm_mpagospesos / vm_mlimitecompra ]
  dataset[ , vmr_mpagosdolares       := vm_mpagosdolares / vm_mlimitecompra ]
  dataset[ , vmr_mconsumototal       := vm_mconsumototal  / vm_mlimitecompra ]
  dataset[ , vmr_mpagominimo         := vm_mpagominimo  / vm_mlimitecompra ]
  
  #Aqui debe usted agregar sus propias nuevas variables
  
  dataset [ , edad_antiguedad        := (cliente_edad-18)/cliente_antiguedad]

  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }
  
  
  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }
  
}




#------------------------------------------------------------------------------

drift_rank_simple  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , paste0(campo,"_rank") :=  (frank( get(campo), ties.method="random") - 1) / ( .N -1 ), by= foto_mes]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#El cero se transforma en cero
#los positivos se rankean por su lado
#los negativos se rankean por su lado

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", kexp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", kexperimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", kexperimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#primero agrego las variables manuales
AgregarVariables( dataset )

setorder( dataset, foto_mes, numero_de_cliente )

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]

#aqui aplico un metodo para atacar el data drifting
#hay que probar experimentalmente cual funciona mejor
switch( 
  kmetodo,
  "ninguno"        = cat( "No hay correccion del data drifting" ),
  "rank_simple"    = drift_rank_simple( campos_monetarios ),
  "rank_cero_fijo" = drift_rank_cero_fijo( campos_monetarios ),
 # "deflacion"      = drift_deflacion( campos_monetarios ) 
)


ipc <- read_excel("datasets/ipc.xlsx")

dataset <- dataset %>% 
  left_join(ipc)%>% 
  mutate (mrentabilidad = mrentabilidad /ipc,
          mrentabilidad_annual = mrentabilidad_annual /ipc,
          mcomisiones = mcomisiones /ipc,
          mactivos_margen = mactivos_margen /ipc,
          mpasivos_margen = mpasivos_margen /ipc,
          mcuenta_corriente_adicional = mcuenta_corriente_adicional /ipc,
          mcuenta_corriente = mcuenta_corriente /ipc,
          mcaja_ahorro = mcaja_ahorro /ipc,
          mcaja_ahorro_adicional = mcaja_ahorro_adicional /ipc,
          mcaja_ahorro_dolares = mcaja_ahorro_dolares /ipc,
          mcuentas_saldo = mcuentas_saldo /ipc,
          mautoservicio = mautoservicio /ipc,
          mtarjeta_visa_consumo = mtarjeta_visa_consumo /ipc,
          mtarjeta_master_consumo = mtarjeta_master_consumo /ipc,
          mprestamos_personales = mprestamos_personales /ipc,
          mprestamos_prendarios = mprestamos_prendarios /ipc,
          mprestamos_hipotecarios = mprestamos_hipotecarios /ipc,
          mplazo_fijo_dolares = mplazo_fijo_dolares /ipc,
          mplazo_fijo_pesos = mplazo_fijo_pesos /ipc,
          minversion1_pesos = minversion1_pesos /ipc,
          minversion1_dolares = minversion1_dolares /ipc,
          minversion2 = minversion2 /ipc,
          mpayroll = mpayroll /ipc,
          mpayroll2 = mpayroll2 /ipc,
          mcuenta_debitos_automaticos = mcuenta_debitos_automaticos /ipc,
          # mtarjeta_visa_debitos_automaticos = mtarjeta_visa_debitos_automaticos /ipc,
          mttarjeta_master_debitos_automaticos = mttarjeta_master_debitos_automaticos /ipc,
          mpagodeservicios = mpagodeservicios /ipc,
          mpagomiscuentas = mpagomiscuentas /ipc,
          mcajeros_propios_descuentos = mcajeros_propios_descuentos /ipc,
          mtarjeta_visa_descuentos = mtarjeta_visa_descuentos /ipc,
          mtarjeta_master_descuentos = mtarjeta_master_descuentos /ipc,
          mcomisiones_mantenimiento = mcomisiones_mantenimiento /ipc,
          mcomisiones_otras = mcomisiones_otras /ipc,
          mforex_buy = mforex_buy /ipc,
          mforex_sell = mforex_sell /ipc,
          mtransferencias_recibidas = mtransferencias_recibidas /ipc,
          mtransferencias_emitidas = mtransferencias_emitidas /ipc,
          mextraccion_autoservicio = mextraccion_autoservicio /ipc,
          mcheques_depositados = mcheques_depositados /ipc,
          mcheques_emitidos = mcheques_emitidos /ipc,
          mcheques_depositados_rechazados = mcheques_depositados_rechazados /ipc,
          mcheques_emitidos_rechazados = mcheques_emitidos_rechazados /ipc,
          matm = matm /ipc,
          matm_other = matm_other /ipc,
          Master_mfinanciacion_limite = Master_mfinanciacion_limite /ipc,
          Master_msaldototal = Master_msaldototal /ipc,
          Master_msaldopesos = Master_msaldopesos /ipc,
          Master_msaldodolares = Master_msaldodolares /ipc,
          Master_mconsumospesos = Master_mconsumospesos /ipc,
          Master_mconsumosdolares = Master_mconsumosdolares /ipc,
          Master_mlimitecompra = Master_mlimitecompra /ipc,
          Master_madelantopesos = Master_madelantopesos /ipc,
          Master_madelantodolares = Master_madelantodolares /ipc,
          Master_mpagado = Master_mpagado /ipc,
          Master_mpagospesos = Master_mpagospesos /ipc,
          Master_mpagosdolares = Master_mpagosdolares /ipc,
          Master_mconsumototal = Master_mconsumototal /ipc,
          Master_mpagominimo = Master_mpagominimo /ipc,
          Visa_mfinanciacion_limite = Visa_mfinanciacion_limite /ipc,
          Visa_msaldototal = Visa_msaldototal /ipc,
          Visa_msaldopesos = Visa_msaldopesos /ipc,
          Visa_msaldodolares = Visa_msaldodolares /ipc,
          Visa_mconsumospesos = Visa_mconsumospesos /ipc,
          Visa_mconsumosdolares = Visa_mconsumosdolares /ipc,
          Visa_mlimitecompra = Visa_mlimitecompra /ipc,
          Visa_madelantopesos = Visa_madelantopesos /ipc,
          Visa_madelantodolares = Visa_madelantodolares /ipc,
          Visa_mpagado = Visa_mpagado /ipc,
          Visa_mpagospesos = Visa_mpagospesos /ipc,
          Visa_mpagosdolares = Visa_mpagosdolares /ipc,
          Visa_mconsumototal = Visa_mconsumototal /ipc,
          Visa_mpagominimo = Visa_mpagominimo /ipc) 



fwrite( dataset,
        file="dataset.csv.gz",
        sep= "," )


