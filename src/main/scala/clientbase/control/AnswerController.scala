package clientbase.control

import definition.typ.{DataType, DialogQuestion, ParamQuestion}
import org.scalajs.dom.html.{Button, Div, Paragraph, Span}
import scalatags.JsDom.all._
/**
  * Created by Peter Holzer on 12.01.2016.
  */
class AnswerController {
  val questionLabel: Paragraph = p(`class` := "questionLabel").render
  val panel: Div = div(id:="answercontroller",`class` := "sidepanelpart")(questionLabel).render
  val cancelButton:Button= button(`class`:="cancel-button")("Abbruch").render

  lazy val eitherLab: Span = span(`class` := "orlabel")("Entweder:").render
  lazy val pointPanel=new PointAnswerPanel

  cancelButton.onclick= _ =>{
    DialogManager.reset()
  }

  def loadAnswerDefinitions(question:ParamQuestion):Unit= {
    reset()
    panel.appendChild(questionLabel)

    question match  {
      case d: DialogQuestion =>
        questionLabel.innerHTML=d.name
        if(d.possibleAnswers.size>1) panel.appendChild(eitherLab)
        var counter=0
        for(answerDef<-d.possibleAnswers) {
          val apanel: AnswerPanel =answerDef.dataType match {
            case DataType.StringTyp => new StringAnswerPanel
            case DataType.IntTyp=> new IntAnswerPanel
            case DataType.DoubleTyp=> new DoubleAnswerPanel
            case DataType.VectorTyp=> pointPanel
            case o => throw new IllegalArgumentException("unbekannter Typ "+ o)
          }
          panel.appendChild(apanel.loadAnswerParam(answerDef))
          counter+=1
          if(counter<d.possibleAnswers.size) panel.appendChild(span(`class`:="orlabel")("oder:").render)
          apanel.focus()
        }
      case o=> println("unbekannter Questiontyp "+o)
    }
    panel.appendChild(cancelButton)
  }

  def reset(): Unit = {
    while(panel.hasChildNodes())panel.removeChild(panel.firstChild)
  }
}
