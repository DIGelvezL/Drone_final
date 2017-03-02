package Dominio

import Tecnico.{Archivo, Mensajes}
import scala.util.Try

case class Posicion(x: Int, y: Int, direccion: Direccion)

class Dron {
  val limitePositivo = 10
  val limiteNegativo = -10
  var posicion = Posicion(0, 0, Direccion("Norte"))

  def validarCatidadRutas(direccionList: List[String]) = {
    if (direccionList.size <= 3) Right(direccionList) else Left("El Dron solo puede realizar 3 domicilios por archivo!!")
  }

  def limiteCuadras(p: Posicion) = {
    if(p.x > limitePositivo || p.x < limiteNegativo ||
      p.y > limitePositivo || p.y < limiteNegativo) Left(false) else Right(true)
  }

}

object ControladorDron extends Acciones {
  val dron = new Dron()

  def iniciar(domicilioList: List[String] ) = {
    dron.validarCatidadRutas(domicilioList).fold(l1 => Mensajes.msgError(l1),
      r1 => {
        r1.map(s => println(s"Domicilio: $s"))
        if(!domicilioList.map(s => Archivo.validarArchivo(s)).contains(false)){
          val domiciliosList = domicilioList.map(x => x.map(c => Movimiento(c.toUpper)))
          /*val posicionesList = domiciliosList.foldLeft((posicionInicial, List[Posicion]())){
            (acumulado, item) => {
              val poscicionFinal = obtenerNuevaPosicion(item.toList, acumulado._1)
              (poscicionFinal, acumulado._2 :+ poscicionFinal)
            }
          }._2*/
          val posicionesList = domiciliosList.map(domicilio =>
            domicilio.foldLeft(dron.posicion){(posicionAcumulada, Item) =>
              dron.posicion = cambiarMovimiento(Item, posicionAcumulada)
              dron.posicion
            }
          )
          val entregaFinal = posicionesList.map(p => List(s"(${p.x}, ${p.y}) dirección ${p.direccion}")).flatten
          entregaFinal.map(println)
          Try {Archivo.crearArchivoOut(entregaFinal)}.recover { case e: Exception => Mensajes.msgError("La ubicación del archivo está mal!!") }
        }
      }
    )

    def obtenerNuevaPosicion(direccion: List[Movimiento], p: Posicion):Posicion = {
      direccion.foldLeft(p){(acu, item) =>
        cambiarMovimiento(item, acu)
      }
    }

    def cambiarMovimiento(direccion: Movimiento, p: Posicion):Posicion = {
      direccion match {
        case Avanzar => dron.limiteCuadras(p).fold(l => stop(), r => adelante(p))
        case GirarDerecha => derecha(p)
        case GirarIzquierda => izquierda(p)
      }
    }
  }
}
