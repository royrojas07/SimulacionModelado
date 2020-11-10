
#Structs para las estadisticas
Mensaje <- setClass("Mensaje",
slots = c(
  ID="numeric",
  PC_origen="numeric",
  tiempo_en_cola="numeric",
  llegada_a_cola="numeric",
  tiempo_en_transmision="numeric",
  tiempo_C1="numeric",
  tiempo_Cx="numeric",
  num_total_devuelto="numeric",
  en_cola="logical")
)

Tupla <- setClass("Tupla",
slots = c(
  evento = "character",
  tiempo = "numeric")
) 



#estructura de datos cola de prioridad para usar en los eventos 

PriorityQueue <- function() {
  keys <<- values <<- NULL
  insert <- function(key, value) {
    temp <- c(keys, key)
    ord <- order(temp)
    keys <<- temp[ord]
    values <<- c(values, list(value))[ord]
  }
  pop <- function() {
    head <- values[[1]]
    headKey <- keys[[1]]
    tupla <<- new("Tupla",evento = head, tiempo=headKey)
    values <<- values[-1]
    keys <<- keys[-1]
    return(tupla)
  }
  empty <- function() length(keys) == 0
  clear <- function() keys <<- values <<- NULL
  list(insert = insert, pop = pop, empty = empty, clear = clear)
}

#estructura de datos cola para guardar los mensajes de cada computadora, igualmente los rechazados y los aceptados
Queue <- function() {
  values <- NULL
  insert <- function(value, pos = 1) {
    if( pos == 0 )
    {
      values <<- c(list(value), values)
    }
    else
    {
      values <<- c(values, list(value))
    }
  }
  pop <- function() {
    head <- values[[1]]
    values <<- values[-1]
    return(head)
  }
  empty <- function() length(values) == 0
  clear <- function()  values <<- NULL
  list(insert = insert, pop = pop, empty = empty, clear = clear)
}



#lectura del archivo de texto


#    Estadisticas Globales    #
# Tiempo procesadores ocupados(Por procesador)
# Tiempo en cola total
# Tiempo procesadores ocupados en msj rechazados(Por procesador C1 y C3)
# mensajes rechazados total 
# Numero de veces msj devueltos

cola_de_eventos <- PriorityQueue()

#colas de mensajes
cola_msj_C1 <- Queue()
cola_msj_C2 <- Queue()
cola_msj_C3 <- Queue()

#colas transmision
cola_trans_C1_a_C2 <- Queue()
cola_trans_C1_a_C3 <- Queue()
cola_trans_C2_a_C1 <- Queue()
cola_trans_C3_a_C1 <- Queue()
cola_msj_destino <- Queue()
cola_msj_rechazados <- Queue()

x2 <- 0.0  # probabilidad x2, que PC3 descarte un msj
x1 <- 0.0  # probabilidad x1, msj devuelto a PC2
x3 <- 0.0  # probabilidad x3, msj devuelto a PC3

C1_ocupado <- FALSE
C2_N1_ocupado <- FALSE
C2_N2_ocupado <- FALSE
C3_ocupado <- FALSE
reloj <- 0
msj_ID <- 0
C2_N1_trabajo <- 0
C2_N2_trabajo <- 0

tiempoMaximo <- 0

entradaDatos = read.csv( "input.csv", header=FALSE )



simular <- function() {
  asignarDistribuciones()
  x1 <- entradaDatos[7, 2]
  x2 <- entradaDatos[8, 2]
  x3 <- entradaDatos[9, 2]
  repeticiones <- entradaDatos[11, 2]
  tiempoMaximo <- entradaDatos[10, 2] #cuantos segundos se quiere la simulacion
  for(i in 1:repeticiones) #el 10 indica cuantas veces quiero que se repita las simulaciones
  {
    # se programan los primeros eventos
    cola_de_eventos$insert( 0, "0" )
    cola_de_eventos$insert( 0, "1" )
    #print
    while (reloj < tiempoMaximo)
    {
      print(reloj)
      siguiente <- cola_de_eventos$pop()
      reloj <- siguiente@tiempo
      asociar(siguiente@evento)
    }
    #Sacar y guardar las estadisticas por simulacion 

    #inicializacion para la siguiente simulacion 
    cola_de_eventos$clear()
    cola_msj_C1$clear()
    cola_msj_C2$clear()
    cola_msj_C3$clear()
    cola_trans_C1_a_C2$clear()
    cola_trans_C1_a_C3$clear()
    cola_trans_C2_a_C1$clear()
    cola_trans_C3_a_C1$clear()
    cola_msj_destino$clear()
    cola_msj_rechazados$clear()
    C1_ocupado <- FALSE
    C2_N1_ocupado <- FALSE
    C2_N2_ocupado <- FALSE
    C3_ocupado <- FALSE
    msj_ID <- 0
    reloj <- 0
    C2_N1_trabajo = 0
    C2_N2_trabajo = 0
  }
  #imprimir las estadisticas 
}

# evento número 0
arr_a_C2 <- function() {
  # Aurelio
  # Crear mensaje
  msj <- new ("Mensaje",
    ID=msj_ID,
    PC_origen=2,
    tiempo_en_cola=0,
    llegada_a_cola=0,
    tiempo_en_transmision=0,
    tiempo_C1=0,
    tiempo_Cx=0,
    num_total_devuelto=0,
    en_cola=FALSE
  )

  msj_ID = msj_ID + 1 # aumentar el contador global de mensajes

  if( identical(FALSE, C2_N1_ocupado) | identical(FALSE, C2_N2_ocupado) ){ # if( !C2_N1_ocupado | !C2_N2_ocupado ){
    print("Núcleo disponible")
    msj@en_cola = FALSE
    
    # se insertará a la cola para tenerlo en un lugar donde guardar el mensaje
    if( identical(FALSE, C2_N1_ocupado) ){ # if( !C2_N1_ocupado ){
      print("Núcleo 1 disponible")
      r_D = D2(2)
      print(r_D)
      cola_de_eventos$insert( reloj+r_D, "3" ) # cuando de procesará el mensaje
      C2_N1_ocupado = TRUE # el procesador tiene trabajo de procesar el mensaje
      
      msj@tiempo_Cx = msj@tiempo_Cx + r_D # simulación de que ha sido procesado
      C2_N1_trabajo <- C2_N1_trabajo + r_D #Se agrega el tiempo trabajado al nucleo correspondiente
      msj@en_cola = FALSE # ya fue procesado, no está en cola

    }
    else{ # si el núcleo 1 está ocupado, el núcleo 2 no lo está
      print("Núcleo 2 disponible")
      r_D = D3(3)
      cola_de_eventos$insert( reloj+r_D, "3" )
      C2_N2_ocupado = TRUE

      # tiempo de procesamiento
      msj@tiempo_Cx = msj@tiempo_Cx + r_D
      C2_N2_trabajo <- C2_N2_trabajo + r_D #Se agrega el tiempo trabajado al nucleo correspondiente
      msj@en_cola = FALSE

    }
    
  }
  else{
    #print("No se programó")
    # si núcleos ocupados, el mensaje está en cola
    msj@en_cola = TRUE
    msj@llegada_a_cola = reloj

  }

  # en esta implementación siempre colocaremos el mensaje "en cola" para guardarlo en algún lado
  # y el acumulador del tiempo en cola, se controla con el booleano en mensaje
  cola_msj_C2$insert ( msj )
  cola_de_eventos$insert( reloj+D1(1) , "0") # próximo arribo de mensaje en tiempo aleatorio, programarse a sí mismo

}

#evento numero 1
arr_a_C3 <- function() {
  #Roy
  #se genera un nuevo mensaje
  mensaje <- new( "Mensaje",
    ID=msj_ID,
    PC_origen=3,
    tiempo_en_cola=0,
    llegada_a_cola=0,
    tiempo_en_transmision=0,
    tiempo_C1=0,
    tiempo_Cx=0,
    num_total_devuelto=0,
    en_cola=FALSE
  )
  if( identical(FALSE, C3_ocupado) )
  {
    # generar v.a. para D5
    D5_va <- D5( 5 )
    # se programa el evento C3_termina
    cola_de_eventos$insert( reloj+D5_va, "4" )
    C3_ocupado = TRUE
    # tiempo de procesamiento
    mensaje@tiempo_Cx = mensaje@tiempo_Cx + D5_va
    mensaje@en_cola = FALSE
  }
  else
  {
    mensaje@llegada_a_cola = reloj
    mensaje@en_cola = TRUE
  }
  cola_msj_C3$insert( mensaje )

  # se auto-programa el evento
  # se genera v.a. para D4
  D4_va <- D4( 4 )
  cola_de_eventos$insert( reloj+D4_va, "1" )
}

# evento número 2
C1_termina <- function() {
  # #Aurelio
  # toma el mensaje por procesar
  msj <- cola_msj_C1$pop()
  
  r = runif(1, min = 0, max = 1) # valor aleatorio para saber qué se hará con el mensaje (enviar al destino o devolver a la PC de origen)
  # 0 - x1: msj devuelto a PC2
  # 0 - x3: msj devuelto a PC3
  # mensajes devueltos ++
  # resto de probabilidad: msj a destino
  # cola_msj_destino$insert( msj )
  
  # ver el origen del mensaje y según la probabilidad,
  # ver si se devuelve al origen o se envía al destino
  #? ¿Dónde se agrega el tiempo que duró procesando, en el main "simulación"?
  # si es la PC 2
  if( identical( mensaje@PC_origen, 2 ) ){ # if( mensaje@PC_origen == 2 ){
    if( r > x1 ){
      cola_msj_destino$insert(msj)
    }
    else{
      cola_trans_C1_a_C2$insert(msj) # meter en cola devolución a C2
      cola_de_eventos$insert(reloj+3,"5") # se programa el evento de devolución el mensaje y dura 3 segundos
      msj@num_total_devuelto = msj@num_total_devuelto+1
    }
  }
  else{ # si es la PC 3
    if( r > x3 ){
      cola_msj_destino$insert(msj)
    }
    else{
      cola_trans_C1_a_C3$insert(msj) # meter en cola devolución a C3
      cola_de_eventos$insert(reloj+3,"6")
      msj@num_total_devuelto = msj@num_total_devuelto+1
    }
  }


  if( identical( FALSE, cola_msj_C1$empty() ) ){ # si hay mensajes en cola
    msj = cola_msj_C3$pop()
    
    if( msj@en_cola ){ # si mensaje estaba en cola
      msj@tiempo_en_cola = msj@tiempo_en_cola + (reloj - msj@llegada_a_cola) # aucumula tiempo en cola
    }

    cola_de_eventos$insert(reloj+D6(6), "2") # ver cuando los atiende
    #? analizar esto, parece que el ocupado no se asigna aquí # // ocupado = true porque se le asigna lo ocupado en el evento llega msj
  }
  else{ #? C3_termina se está desprogramando?
    cola_de_eventos$insert(tiempoMaximo*4, "2") #? definir el T_MAX como el tiempo final de la simulación # desprogramar evento
    C1_ocupado = FALSE
  }

}

#evento numero 3
C2_termina <- function() {
  #Carlos
  cola_de_eventos$insert(reloj+20,"7") #Se programa el evento llega_a_C1_de_C2 que corresponde al 7
  mensaje <- cola_msj_C2$pop() #Se saca mensaje de la cola de mensajes
  cola_trans_C2_a_C1$insert(mensaje) #se inserta en la cola de transmision 
  if(identical(FALSE,cola_msj_C2$empty()) & C2_N1_ocupado) #Si la cola no esta vacia y el nucleo se le asigno ocupacion en el evento devuelto C2
  {
    d2 <- D2(2) #se obtiene el random de la distribucion
    nuevo_mensaje <- cola_msj_C2$pop() #Se casa un nuevo mensaje de la cola
    nuevo_mensaje@tiempo_en_cola <- nuevo_mensaje@tiempo_en_cola + reloj - nuevo_mensaje@llegada_a_cola # Si estaba en la cola se obtiene el tiempo que estuvo ahi
    nuevo_mensaje@tiempo_Cx <- nuevo_mensaje@tiempo_Cx + d2 #Se le agrega el tiempo de procesamiento que va a tener
    C2_N1_trabajo <- C2_N1_trabajo + d2 #Se agrega el tiempo trabajado al nucleo correspondiente
    cola_msj_C2$insert(nuevo_mensaje,0) #Se inserta en el head de la cola
    cola_de_eventos$insert(reloj+d2,"3") #Se program este evento asi mismo
  }
  else{ #Entra aqui si la cola esta vacia o N1 no ocupado
    C2_N1_ocupado = FALSE 
  }
  if(identical(FALSE,cola_msj_C2$empty()) & C2_N2_ocupado){ #Si la cola no esta vacia y el nucleo se le asigno ocupacion en el evento devuelto C2
    d3 <- D3(3) #se obtiene el random de la distribucion
    nuevo_mensaje <- cola_msj_C2$pop() #Se casa un nuevo mensaje de la cola
    nuevo_mensaje@tiempo_en_cola <- nuevo_mensaje@tiempo_en_cola + reloj - nuevo_mensaje@llegada_a_cola # Si estaba en la cola se obtiene el tiempo que estuvo ahi
    nuevo_mensaje@tiempo_Cx <- nuevo_mensaje@tiempo_Cx + d3  #Se le agrega el tiempo de procesamiento que va a tener
    C2_N2_trabajo <- C2_N2_trabajo + d3 #Se agrega el tiempo trabajado al nucleo correspondiente
    cola_msj_C2$insert(nuevo_mensaje,0) #Se inserta en el head de la cola
		cola_de_eventos$insert(reloj+d3,"3")  #Se programa este evento asi mismo
  }
  else{
    C2_N2_ocupado = FALSE
  }
}

#evento numero 4
C3_termina <- function() {
  #Roy
  # se saca el mensaje
  mensaje = cola_msj_C3$pop()
  # se genera random [0,1]
  r = runif( 1, min = 0, max = 1 )
  if( r < x2 ) # se rechaza
  {
    cola_msj_rechazados$insert( mensaje )
  }
  else # se envia el mensaje a C1
  {
    # se programa llega_a_C1_de_C3
    cola_de_eventos$insert( reloj+20, "8" )
    cola_trans_C3_a_C1$insert( mensaje )
  }
  
  # se procede a revisar si hay mensajes en cola para procesar
  if( identical(FALSE, cola_msj_C3$empty()) )
  {
    msj = cola_msj_C3$pop()
    # se incrementa tiempo en cola
    msj@tiempo_en_cola = msj@tiempo_en_cola + reloj - msj@llegada_a_cola
    D5_va <- D5( 5 )
    cola_de_eventos$insert( reloj+D5_va, "4" )
    # tiempo de procesamiento
    msj@tiempo_Cx = msj@tiempo_Cx + D5_va
    # se debe volver a poner al mensaje en la E.Datos
    cola_msj_C3$insert( nuevo_mensaje, 0 )
  }
  else
  {
    C3_ocupado = FALSE
  }
}

#evento numero 5
devuelto_a_C2 <- function() {
  mensaje = cola_trans_C1_a_C2$pop() #se saca de la cola de transmision el msj
  mensaje@tiempo_en_transmision <- mensaje@tiempo_en_transmision + 3 #Se agrega a la estructura mensaje el tiempo de transmision
  if(identical(FALSE,C2_N1_ocupado) | identical(FALSE,C2_N2_ocupado)) #Pregunto si ambos nucleos no estan ocupados
  {
    if(identical(FALSE,C2_N1_ocupado) ) #Pregunto si no esta ocupado el nucleo 1
    {
      d2 <- D2(2)
      C2_N1_ocupado <- TRUE #Ahora esta ocupado
      cola_de_eventos$insert(reloj+d2,"3") #Se programa el evento C2 termina que corresponde al 3
      mensaje@tiempo_Cx <- mensaje@tiempo_Cx + d2 #Se agrega a la estructura el tiempo que va a durar procesandose
      C2_N1_trabajo <- C2_N1_trabajo + d2 #Se agrega el tiempo trabajado al nucleo correspondiente
      cola_msj_C2(mensaje) #Se agrega a la cola de mensajes para que C2 termina lo pueda acceder
      mensaje@en_cola <- FALSE #Se pone en falso porque no va a estar en la cola, solo se agrega para que C2 termina lo pueda acceder
    }
    else #Si que no esta ocupado es el nucleo 2
    {
      d3 <- D3(3)
      C2_N2_ocupado <- TRUE  #Ahora esta ocupado
		  cola_de_eventos$insert(reloj+d3,"3") #Se programa el evento C2 termina que corresponde al 3
      mensaje@tiempo_Cx <- mensaje@tiempo_Cx + d3 #tiempo que dura procesandose
      C2_N2_trabajo <- C2_N2_trabajo + d3 #Se agrega el tiempo trabajado al nucleo correspondiente
      cola_msj_C2(mensaje) #Se agrega a la cola de mensajes para que C2 termina lo pueda acceder
      mensaje@en_cola <- FALSE  #Se pone en falso porque no va a estar en la cola, solo se agrega para que C2 termina lo pueda acceder
    }
  }
  else #los 2 estan ocupados
  {
     cola_msj_C2(mensaje) #Se agrega a la cola de mensajes para que C2 termina lo pueda acceder
     mensaje@inicio_en_cola <- reloj #Se empieza a tomar el tiempo en cola
     mensaje@en_cola <- TRUE #En este caso el mensaje de verdad esta en cola, asi que se pone verdadero
  }
}

#evento #6
devuelto_a_C3 <- function() {
  #Roy
  mensaje = cola_trans_C1_a_C3$pop()
  mensaje@tiempo_en_transmision = mensaje@tiempo_en_transmision + 3
  if( !C3_ocupado ) # se empieza a procesar mensaje
  {
    D5_va <- D5( 5 )
    # se programa evento C3_termina
    cola_de_eventos$insert( reloj + D5_va, "4" )
    C3_ocupado = TRUE
    mensaje@tiempo_Cx = mensaje@tiempo_Cx + D5_va
    # aca igual habria que agregar a la cola tal vez con una bandera?
    mensaje@en_cola = FALSE
  }
  else # esta ocupado
  {
    mensaje@llegada_a_cola = reloj
    mensaje@en_cola = TRUE
  }
  cola_msj_C3$insert( mensaje )
}

# evento número 7
llega_a_C1_de_C2 <- function() {
  # #Aurelio
  msj <- cola_trans_C2_a_C1$pop()
  msj@tiempo_en_transmision = msj@tiempo_en_transmision+20 # tiempo empleado en enviarse
  
  # llegó el mensaje, ver si el procesador lo atiende o espera en cola
  if( identical(FALSE, C1_ocupado) ){ # !C1_ocupado
    msj@en_cola = FALSE # el mensaje será el próximo en ser atendido
    C1_ocupado = TRUE # procesador estará ocupado con este mensaje
    #? No hace falta que se programe que C2 termine de procesar el mensaje ?
  }
  else{ # si el procesador está ocupado, espera en la cola
    msj@en_cola = TRUE
  }
  cola_msj_C1$insert( msj ) # insertar mensaje en cola o listo para ser atendido por el procesador
  
  if( intentical( FALSE, cola_trans_C2_a_C1$empty() ) ){#? no de debería de programar solo si la cola no está vacía
    cola_de_eventos$insert( reloj+D6() ) # próxima llegada de msj a C1 de parte de C2
  }

}

#evento numero 8
llega_a_C1_de_C3 <- function() {
  mensaje <- cola_trans_C3_a_C1$pop() #Se saca mensaje de la cola de transmision
  mensaje@tiempo_en_transmision <- mensaje@tiempo_en_transmision + 20 #Se suma el tiempo que se estuvo transmitiendo
  if(identical(FALSE,C1_ocupado)) #Pregunto si el C1 no esta ocupado
  {
    d6 = D6(6)
    C1_ocupado <- TRUE #Ahora esta ocupado
    cola_de_eventos$insert(reloj+d6,"2") #Se programa el evento C1 termina 
    mensaje$tiempo_C1 <- mensaje$tiempo_C1+d6 #Tiempo que duraria procesandose
    cola_msj_C1$insert(mensaje) #Se inserta en la cola para que C1 termina lo pueda acceder
    mensaje$en_cola <- FALSE #Se pone en falso porque en verdad no esta en cola es solo para el acceso en C1 termina
  }
  else 
  {
    cola_msj_C1$insert(mensaje) #Se inserta en la cola para que C1 termina lo pueda acceder
    mensaje$llegada_a_cola <- reloj #Se empieza a tomar el tiempo en cola
    mensaje$en_cola <- TRUE #En este caso si esta en cola entonces se pone verdadero
  }
}

# FUNCIONES MATEMATICAS PARA LAS DISTRIBUCIONES
exponencial <- function( num_distribucion ){
  lambda <- as.numeric(entradaDatos[num_distribucion, 2])
  r = runif(1, min = 0, max = 1)
  return (-log(1-r)/lambda)
}

normal_metodo_directo <- function(num_distribucion){
   media <- as.numeric(entradaDatos[num_distribucion, 2])
   varianza <- as.numeric(entradaDatos[num_distribucion, 3])
   sigma = sqrt( varianza ) # desviación estándar = sigma
   r1 = runif(1, min = 0, max = 1)
   r2 = runif(1, min = 0, max = 1)
   # en R, log es logaritmo natural
   x = ( sqrt( -2*log(r1) ) * cos(2*pi*r2) ) # 2*pi RAD = 360°
   x = (sigma * x) + media  # paréntesis por claridad
   return (x)
}

# se toma k=12 como se sugiere en el libro
normal_tlc <- function( num_distribucion ){
  r_sum <- 0
  media <- as.numeric(entradaDatos[num_distribucion, 2])
  varianza <- as.numeric(entradaDatos[num_distribucion, 3])
  for( i in 1:12 )
    r_sum = r_sum + runif( 1, min = 0, max = 1 )
  return (sqrt( varianza )*( r_sum-6 ) + media)
}

funcion_densidad <- function(num_distr)
{
  k <- as.numeric(entradaDatos[num_distr, 4])
  a <- as.numeric(entradaDatos[num_distr, 2])
  b <- as.numeric(entradaDatos[num_distr, 3])
  r = runif(1,min=0,max=1)
  random <- sqrt(r*(k/2) + a^2)
  if(a <= random & random <= b)
  {
    return(random)
  }
  else {
     return(a) #el valor de a es el valor default a retornar en caso de salirse del rango  
  }
}

uniforme <- function( num_distribucion ){
  a <- as.numeric(entradaDatos[num_distribucion, 2])
  b <- as.numeric(entradaDatos[num_distribucion, 3])
  r = runif(1, min = 0, max = 1)
  x = r*(b-a)+a
  return(x)
}

D1 <- function(...){}
D2 <- function(...){}
D3 <- function(...){}
D4 <- function(...){}
D5 <- function(...){}
D6 <- function(...){}

#funcion encargada de asociar el respectivo id con la funcion correspondiente
asociar <- function(id) 
{
  switch(id,
        "0" = arr_a_C2(),
        "1" = arr_a_C3(),
        "2" = C1_termina(),
        "3" = C2_termina(),
        "4" = C3_termina(),
        "5" = devuelto_a_C2(),
        "6" = devuelto_a_C3(),
        "7" = llega_a_C1_de_C2(),
        "8" = llega_a_C1_de_C3())
}

asignarDistribuciones <- function()
{
  # se iteran las filas del csv
  for( row in 1:6 ) # cómo sacar las filas de la matriz?
  {
    nombre_distr = entradaDatos[row,1]
    switch( row,
      {D1 <<- distribucionPorNombre( nombre_distr )},
      {D2 <<- distribucionPorNombre( nombre_distr )},
      {D3 <<- distribucionPorNombre( nombre_distr )},
      {D4 <<- distribucionPorNombre( nombre_distr )},
      {D5 <<- distribucionPorNombre( nombre_distr )},
      {D6 <<- distribucionPorNombre( nombre_distr )})
  }
}

# asocia el nombre ingresado por el usuario con la función
distribucionPorNombre <- function( nombre )
{
  switch( nombre,
    "normal_metodo_directo" = normal_metodo_directo, # cualquier cosa cambiar el nombre
    "normal_TLC" = normal_tlc,
    "uniforme" = uniforme,
    "exponencial" = exponencial,
    "func_densidad" = funcion_densidad)
}

simular()