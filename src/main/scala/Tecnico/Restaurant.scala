package Tecnico

import Dominio.{ControladorDron}

import scala.io.Source
import scala.util.Try


object User extends App {

  val res = Try {Source.fromFile("/home/daniel/in.txt").getLines.toList}.recover{case e: Exception => Mensajes.msgError("La carpeta del archivo está mal!!")}
  res.map(l => ControladorDron.iniciar(l.asInstanceOf[List[String]]))

}

object Mensajes{

  def msgSuccess(msg:String) = {println(msg)}
  def msgError(msg:String) = {println(msg)}
}

