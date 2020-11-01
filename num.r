
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
  num_total_devuelto="numeric")
)

setClass("Procesador",
  slots= c(
    tiempo_trabajado="numeric",
    inicio_trabajo="numeric"
  )
)


#lectura del archivo de texto


#    Colas     #
# Prioridad de eventos
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

C1_ocupado = "logical"
C2_N1_ocupado = "logical"
C2_N2_ocupado = "logical"
C3_ocupado = "logical"


simular <- function() {
  
}

arr_a_C2 <- function() {
  #Aurelio
}

arr_a_C3 <- function() {
  #Roy
}

C1_termina <- function() {
  #Aurelio
}

C2_termina <- function() {
  #Carlos
}

C3_termina <- function() {
  #Roy
}

devuelto_a_C2 <- function() {
  #Carlos
}

devuelto_a_C3 <- function() {
  #Roy
}

llega_a_C1_de_C2 <- function() {
  #Aurelio
}

llega_a_C1_de_C3 <- function() {
  #Carlos
}

mensaje1 <- new("mensaje",ID=1,origen=2)

print(mensaje1@origen)



