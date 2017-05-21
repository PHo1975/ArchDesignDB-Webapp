package clientbase.control

import definition.typ.{ DataType, DialogQuestion, ParamQuestion }
import org.scalajs.dom.html.{ Div, Paragraph, Span }
import scalatags.JsDom.all._
/**
  * Created by Peter Holzer on 12.01.2016.
  */
class AnswerController {
  val questionLabel: Paragraph = p(`class` := "questionLabel").render
  val panel: Div = div(`class` := "sidepanelpart")(questionLabel).render

  lazy val eitherLab: Span = span(`class` := "orlabel")("Entweder:").render

  def loadAnswerDefinitions(question:ParamQuestion):Unit= {
    reset()
    panel.appendChild(questionLabel)

    question match  {
      case d: DialogQuestion =>
        questionLabel.innerHTML=d.name
        if(d.possibleAnswers.size>1) panel.appendChild(eitherLab)
        var counter=0
        for(answerDef<-d.possibleAnswers) {
          val apanel=answerDef.dataType match {
            case DataType.StringTyp => new StringAnswerPanel
            case DataType.IntTyp=> new IntAnswerPanel
            case DataType.DoubleTyp=> new DoubleAnswerPanel
            case o => throw new IllegalArgumentException("unbekannter Typ "+ o)
          }
          apanel.loadAnswerParam(answerDef)
          panel.appendChild(apanel.panel)
          counter+=1
          if(counter<d.possibleAnswers.size) panel.appendChild(span(`class`:="orlabel")("oder:").render)
          apanel.focus()
        }
      case o=> println("unbekannter Questiontyp "+o)
    }
  }

  def reset(): Unit = {
    while(panel.hasChildNodes())panel.removeChild(panel.firstChild)
  }
}
