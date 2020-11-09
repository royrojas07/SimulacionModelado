
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

Procesador <- setClass("Procesador",
  slots= c(
    tiempo_trabajado="numeric",
    inicio_trabajo="numeric"
  )
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
    values <<- values[-1]
    keys <<- keys[-1]
    return(head)
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


#    Colas     #

# Rechazados
# Aceptados
# Cola de mensajes por Computadora
# Cola de transmision.... En proceso

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
# x1  # probabilidad x1, msj devuelto a PC2
# x3  # probabilidad x3, msj devuelto a PC3

C1_ocupado = "logical"
C2_N1_ocupado = "logical"
C2_N2_ocupado = "logical"
C3_ocupado = "logical"
reloj <- 0
msj_ID <- 0

simular <- function() {
  count <- 1
  for(i in 1:10) #el 10 indica cuantas veces quiero que se repita las simulaciones
  { 
    period <- 30 #cuantos segundos se quiere la simulacion
    while (reloj < period)
    {
       
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
  }
  #imprimir las estadisticas 
}

#evento numero 0
arr_a_C2 <- function() {
  # #Aurelio
  # Crear adentro # crear mensaje m # lo creamos aquí o desde afuera? Piendo que mejor aquí
  #mensaje 
  msj <- new("Mensaje",
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
  # msj <- Mensaje(
    # ID=msj_ID,
    # PC_origen=2,
    # tiempo_en_cola=0,
    # llegada_a_cola=0,
    # tiempo_en_transmision=0,
    # tiempo_C1=0,
    # tiempo_Cx=0,
    # num_total_devuelto=0,
    # en_cola=FALSE
  # )
  print(msj@ID)
  # # ID=msj_id, PC_origen= 2, tiempo_en_cola=0, llegada_a_cola=0, tiempo_en_transmision=0, tiempo_C1=0  tiempo_Cx=0, num_total_devuelto=0, en_cola=false)
  # # msj@ID = msj_id
  # # contador de id mensajes + 1
  # # msj_id += 1
  # if( !C2_N1_ocupado | !C2_N2_ocupado ){
  #   if( !C2_N1_ocupado ){
  #     cola_de_eventos$insert(reloj+D2, "3")
  #     C2_N1_ocupado = TRUE
  #   }
  #   else{
  #     cola_de_eventos$insert(reloj+D3, "3") #? aumentar el tiempo en el evento 3
  #     C2_N2_ocupado = TRUE
  #   }
  # }
  # else{ # si núcleos ocupados, a la cola
  #   cola_msj_C2()$insert(msj) #? verificar si el mensaje se inserta de esta manera
  #   #? sumar aquí tiempo en cola # programar a sí mismo
  # }
}

#evento numero 1
arr_a_C3 <- function() {
  #Roy
  """#se genera un nuevo mensaje
  mensaje <- new(
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
  if( !C3_ocupado )
  {
    # generar v.a. para D5
    # se programa el evento C3_termina
    cola_eventos$insert( reloj+D5, "4" )
    C3_ocupado = TRUE
    # tiempo de procesamiento
    mensaje@tiempo_Cx += D5
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
  cola_de_eventos$insert( reloj+D4, "1" )"""
}

#evento numero 2
C1_termina <- function() {
  # #Aurelio
  # R/ hacer pop de la cola #? cómo se toma el mensaje?
  # r = runif(n, min = 0, max = 1)
  # mensajes devueltos ++
  # if(mensaje@PC_origen == 2 ){
  #   if( r > x1 ){
  #     # cola_msj_destino$insert(msj)
  #   }
  #   else{
  #     # cola_eventos$insert(reloj+3,"5")
  #     if( r > x3 ){
  #       # cola_msj_destino$insert(msj)
  #     }
  #     else{
  #       # cola_eventos$insert(reloj+3,"6")
  #     }
  #   }
  # }
  # else{ #? No está en el drive, pero puede recibir también de PC 3
  #   #? se puede hacer más corto este código
  #   if( r > x1 ){
  #     # cola_msj_destino$insert(msj)
  #   }
  #   else{
  #     # cola_eventos$insert(reloj+3,"6")
  #     if( r > x3 ){ #? verificar el manejo de esta probabilidad
  #       # cola_msj_destino$insert(msj)
  #     }
  #     else{
  #       # cola_eventos$insert(reloj+3,"6")
  #     }
  #   }
  # }
  # #? Si un mensaje entra aquí, se procesa y después de consumir ese tiempo se decide qué hacer con él

  # if(cola_msj_C1$queuelength){ # crear función, creo el metodo es si se usara la librería # buscar método para saber el length
  #   #? analizar esto, parece que el ocupado no se asigna aquí # // ocupado = true porque se le asigna lo ocupado en el evento llega msj
  #   cola_eventos$insert(reloj+D6, "2")
  # }
  # else{
  #   cola_eventos$insert(T_MAX*4, "2") #? definir el T_MAX como el tiempo final de la simulación # desprogramar evento
  #   C1_ocupado = false
  # }

  # ? agreagr tiempo a mensaje
}

#evento numero 3
C2_termina <- function() {
  #Carlos
  cola_de_eventos$insert(reloj+20,"7")
  mensaje <- cola_msj_C2$pop() #DEBERIA PREGUNTA POR EL BOOL EN_COLA
  mensaje@tiempo_en_cola += reloj - mensaje@llegada_a_cola                            
  cola_trans_C2_a_C1$insert(mensaje)
  if(!cola_msj_C2$empty() & C2_N1_ocupado)
  {
    #C2_N1_ocupado = TRUE # es redudante
    nuevo_mensaje <- cola_msj_C2$pop()
    if(nuevo_mensaje@en_cola)
    {
      nuevo_mensaje@tiempo_en_cola += reloj - nuevo_mensaje@llegada_a_cola
    }
    nuevo_mensaje@tiempo_Cx += D2
    cola_msj_C2$insert(nuevo_mensaje,0)
    cola_de_eventos$insert(reloj+D2,"3")
  }
  else{
    C2_N1_ocupado = FALSE
  }
	#ifelse(!cola_msj_C2$empty() & C2_N2_ocupado){ #esto no seria un if?
  if(!cola_msj_C2$empty() & C2_N2_ocupado){ 
		#C2_N2_ocupado = TRUE # es redundante
    nuevo_mensaje <- cola_msj_C2$pop()
    if(nuevo_mensaje@en_cola)
    {
      nuevo_mensaje@tiempo_en_cola += reloj - nuevo_mensaje@llegada_a_cola
    }
    nuevo_mensaje@tiempo_Cx += D3
    cola_msj_C2$insert(nuevo_mensaje,0)
		cola_de_eventos$insert(reloj+D3,"3")
  }
  else{
    C2_N2_ocupado = FALSE
  }
}

#evento numero 4
C3_termina <- function() {
  #Roy
  """# se saca el mensaje
  mensaje = cola_msj_C3$pop()
  # se genera random [0,1]
  rand = random()
  if( rand < x2 ) # se rechaza
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
  if( !cola_msj_C3$empty() )
  {
    nuevo_mensaje = cola_msj_C3$pop()
    if( nuevo_mensaje@en_cola ) # mensaje estaba en cola
    {
      # se incrementa tiempo en cola
      nuevo_mensaje@tiempo_en_cola += reloj - nuevo_mensaje@llegada_a_cola
      nuevo_mensaje@en_cola = FALSE # este booleano no esta sobrando? #Si creo que si 
    }
    cola_eventos$insert( reloj+D5, "4" )
    # tiempo de procesamiento
    nuevo_mensaje@tiempo_Cx += D5
    # se debe volver a poner al mensaje en la E.Datos
    cola_msj_C3$insert( nuevo_mensaje )
  }
  else
  {
    C3_ocupado = FALSE
  }"""
}

#evento numero 5
devuelto_a_C2 <- function() {
  #Carlos
  mensaje = cola_trans_C1_a_C2$pop()
  mensaje@tiempo_en_transmision += 3
  if(!C2_N1_ocupado | !C2_N2_ocupado)
  {
    if(!C2_N1_ocupado)
    {
      C2_N1_ocupado = TRUE
      cola_de_eventos$insert(reloj+D2,"3")
      mensaje@tiempo_Cx += D2
      cola_msj_C2(mensaje)
      mensaje@en_cola <- FALSE
    }
    else 
    {
      C2_N2_ocupado <- TRUE
		  cola_de_eventos$insert(reloj+D3,"3")   
      mensaje@tiempo_Cx += D3 #tiempo que dura procesandose
      cola_msj_C2(mensaje)
      mensaje@en_cola <- FALSE
    }
  }
  else 
  {
     cola_msj_C2(mensaje)
     mensaje@inicio_en_cola <- reloj #Se empieza a tomar el tiempo en cola
     mensaje@en_cola <- TRUE
  }
}

#evento #6
devuelto_a_C3 <- function() {
  #Roy
  mensaje = cola_trans_C1_a_C3$pop()
  mensaje@tiempo_en_transmision += 3
  if( !C3_ocupado ) # se empieza a procesar mensaje
  {
    # se programa evento C3_termina
    cola_de_eventos$insert( reloj + D5, "4" )
    C3_ocupado = TRUE
    mensaje@tiempo_Cx += D5
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

#evento numero 7
llega_a_C1_de_C2 <- function() {
  # #Aurelio
  # if(C1_ocupado){
  #   cola_de_eventos$insert(reloj+D6)
  #   C1_ocupado = TRUE
  # }
  # else{
  #   #cola_msj_C1$insert( msj )
  # }
}

#evento numero 8
llega_a_C1_de_C3 <- function() {
  #Carlos
  mensaje = cola_trans_C3_a_C1$pop()
  mensaje@tiempo_en_transmision += 20
  if(!C1_ocupado)
  {
     C1_ocupado <- TRUE
     cola_de_eventos$insert(reloj+D6,"2")
     mensaje$tiempo_C1 += D6 #Tiempo que duraria procesandose
     cola_msj_C1$insert(mensaje)
     mensaje$en_cola <- FALSE
  }
  else 
  {
    cola_msj_C1$insert(mensaje) 
    mensaje$llegada_a_cola <- reloj #Se empieza a tomar el tiempo en cola
    mensaje$en_cola <- TRUE
  }
}

<<<<<<< HEAD
=======
# FUNCIONES MATEMATICAS PARA LAS DISTRIBUCIONES
exponencial <- function( lambda ){
  r = runif(1, min = 0, max = 1)
  return (-log(1-r)/lambda)
}

normal_metodo_directo <- function(media, varianza){
   sigma = sqrt( varianza ) # desviación estándar = sigma
   r1 = runif(1, min = 0, max = 1)
   r2 = runif(1, min = 0, max = 1)
   # en R, log es logaritmo natural
   x = ( sqrt( -2*log(r1) ) * cos(360*r2) ) # 2*pi RAD = 360°
   x = (sigma * x) + media  # paréntesis por claridad
   return (x)
}

# se toma k=12 como se sugiere en el libro
normal_tlc <- function( media, varianza ){
  r_sum <- 0
  for( i in 1:12 )
    r_sum = r_sum + runif( 1, min = 0, max = 1 )
  return (sqrt( varianza )*( r_sum-6 ) + media)
}

>>>>>>> 4deaa0ae16391768e05868afdfe25b7fdcf64789
funcion_densidad <- function(k,a,b)
{
  r = runif(1,min=0,max=1)
  random <- sqrt(r*(k/2)a^2)
  if(a <= random & random <= b)
  {
    return(random)
  }
  else {
     return(a) #el valor de a es el valor default a retornar en caso de salirse del rango  
  }
}

uniforme <- function(a,b){
  r = runif(1, min = 0, max = 1)
  x = r*(b-a)+a
  return(x)
}

#funcion encargada de asociar el respectivo id con la funcion correspondiente
matching <- function(id) 
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

#Ejemplo de usar las estructuras
mensaje1 <- new("mensaje",ID=1,origen=2)

print(mensaje1@origen)