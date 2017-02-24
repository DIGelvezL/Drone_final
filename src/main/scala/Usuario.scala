import akka.actor.{Actor, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.collection.mutable.ListBuffer

/**
  * Created by daniel on 24/02/17.
  */
object Listas {

  val listMailEnv: mutable.MutableList[MensajeEnviado] = mutable.MutableList()
  val listMailRec: mutable.MutableList[MensajeRecibido] = mutable.MutableList()
}

class Usuario extends Actor {
  val listBuzon: ListBuffer[MensajeRecibido] = ListBuffer()

  override def preStart() = println("Enviando el correo")
  override def postStop() = println("Se detuvo el envío del correo")

  val server = context.actorOf(Props[Server], "mensajeServer")
  def receive = {
    case MensajeEnviado(id, to, asunto, msg) => {
      implicit val t = Timeout(5.second)
      val originalSender = sender()
      val validar = server ? ValidarCorreos(id, to)

      Listas.listMailEnv += MensajeEnviado(id, to, asunto, msg)
      server ! MensajeEnviado(id, to, asunto, msg)
    }
    case MensajeRecibido(to, id, asunto, msg) => {
      Listas.listMailRec += MensajeRecibido(to, id, asunto, msg)
    }
    case ConsultarMail(m) =>{
      implicit val t = Timeout(5.second)
      val originalSender = sender()
      val fut = server ? ConsultarMail(m)
      fut.map(x => x.asInstanceOf[mutable.MutableList[MensajeRecibido]].map(y => listBuzon += y))
      //fut.foreach(x => println(x))
      Thread.sleep(1000)
      listBuzon.map(x => println(s"Correo enviado por ${x.from} - asunto: ${x.asunto} - mensaje: ${x.msg}"))
      //listBuzon -= MensajeRecibido("ivan@seven4n.com", "daniel@seven4n.com", "probando", "Este es un correo de prueba... xD")
      /*listBuzon.filter(x => x.from == "daniel@seven4n.com").remove(0, 1)
      Thread.sleep(5000)
      listBuzon.map(x => println("borrado: " + x))*/
    }
    case CrearMail(mail) => {
      implicit val t = Timeout(1.second)
      val fut = server ? CrearMail(mail)
      fut.foreach(x => println(x))
    }
    case ErrorEnviarMensaje(mail) => println(mail)
  }
}
