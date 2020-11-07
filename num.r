
#Structs para las estadisticas
setClass("Mensaje",
slots = c(
  ID="numeric",
  PC_origen="numeric",
  tiempo_en_cola="numeric",
  llegada_a_cola="numeric",
  tiempo_en_transmision="numeric",
  tiempo_C1="numeric",
  tiempo_Cx="numeric",
  num_total_devuelto="numeric"
  en_cola="logical")
)

setClass("Procesador",
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
  insert <- function(value) {
    values <<- c(values, list(value))
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
cola_msj_C1 <- Queue()
cola_msj_C2 <- Queue()
cola_msj_C3 <- Queue()
cola_trans_C1_a_C3 <- Queue()
# cola_msj_destino <- Queue()
# x1  # probabilidad x1, msj devuelto a PC2
# x3  # probabilidad x3, msj devuelto a PC3

C1_ocupado = "logical"
C2_N1_ocupado = "logical"
C2_N2_ocupado = "logical"
C3_ocupado = "logical"
reloj = 0

simular <- function() {
  
}

#evento numero 0
arr_a_C2 <- function() {
  # #Aurelio
  # # crear mensaje m # lo creamos aquí o desde afuera? Piendo que mejor aquí
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
  #   #? sumar aquí tiempo en cola
  # }
}

#evento numero 1
arr_a_C3 <- function() {
  #Roy
}

#evento numero 2
C1_termina <- function() {
  # #Aurelio
  # #? cómo se toma el mensaje?
  # r = runif(n, min = 0, max = 1)
  
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
  #   #? analalizar esto, parece que el ocupado no se asigna aquí # // ocupado = true porque se le asigna lo ocupado en el evento llega msj
  #   cola_eventos$insert(reloj+D6, "2")
  # }
  # else{
  #   cola_eventos$insert(T_MAX*4, "2") #? definir el T_MAX como el tiempo final de la simulación # desprogramar evento
  #   C1_ocupado = false
  # }
}

#evento numero 3
C2_termina <- function() {
  cola_de_eventos$insert(reloj+20,7)
  if(!cola_msj_C2$empty() & C2_N1_ocupado)
  {
    C2_N1_ocupado = TRUE
    cola_de_eventos$insert(reloj+D2,"3")
  }
	ifelse(!cola_msj_C2$empty() & C2_N2_ocupado) #esto no seria un if?
		C2_N2_ocupado = TRUE
		cola_de_eventos$insert(reloj+D3,"3")
  }
}

#evento numero 4
C3_termina <- function() {
  #Roy
}

#evento numero 5
devuelto_a_C2 <- function() {
  #Carlos
  if(!C2_N1_ocupado | !C2_N2_ocupado)
  {
    if(!C2_N1_ocupado)
    {
      C2_N1_ocupado = TRUE
      cola_de_eventos$insert(reloj+D2,"3")
    }
    else 
    {
      C2_N2_ocupado = TRUE
		  cola_de_eventos$insert(reloj+D3,"3")   
    }
  }
  else 
  {
     cola_msj_C2("nuevo msj")
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
  if(!C1_ocupado)
  {
     C1_ocupado = TRUE
     cola_de_eventos$insert(reloj+D6,"2")  
  }
  else 
  {
    cola_msj_C1$insert("nuevo mensaje")
  }
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
