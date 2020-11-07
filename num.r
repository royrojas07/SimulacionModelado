
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

C1_ocupado = "logical"
C2_N1_ocupado = "logical"
C2_N2_ocupado = "logical"
C3_ocupado = "logical"
reloj = 0

simular <- function() {
  
}

#evento numero 0
arr_a_C2 <- function() {
  #Aurelio
}

#evento numero 1
arr_a_C3 <- function() {
  #Roy
}

#evento numero 2
C1_termina <- function() {
  #Aurelio
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
  #Aurelio
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
