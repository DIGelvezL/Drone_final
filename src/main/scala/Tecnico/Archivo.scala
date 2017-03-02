package Tecnico

import java.io.{File, PrintWriter}


object Archivo {

  def crearArchivoOut(domiciliosList: List[String]) = {
    val pw = new PrintWriter(new File("/home/daniel/out.txt"))
    val stringBuilder = new StringBuilder
    stringBuilder.append("== Reporte de entregas ==\n")
    stringBuilder.append("\n")
    domiciliosList.map(x => stringBuilder.append(x + "\n"))

    pw.write(stringBuilder.toString)
    pw.close
  }

  def validarArchivo(domicilio: String): Boolean = {
    if (domicilio.filterNot(c => c.toUpper == 'A' || c.toUpper == 'I' || c.toUpper == 'D').isEmpty)
      true
    else {
      Mensajes.msgError(s"El domicilio $domicilio no es valido")
      false
    }
  }

}
