package clientbase.control

import definition.expression.{Constant, DoubleConstant, IntConstant, StringConstant}
import definition.typ.AnswerDefinition
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.raw.Node
import scalatags.JsDom.all._
import util.{StrToDouble, StrToInt}

/**
  * Created by Peter Holzer on 17.01.2016.
  */
abstract class AnswerPanel {
  protected val panel: Div = div(`class` := "answer-panel").render
  val infoLabel: Span = span(`class`:="answer-label").render
  panel.appendChild(infoLabel)
  var answerDefinition:AnswerDefinition= _

  def loadAnswerParam(nanswerDefinition: AnswerDefinition): Node = {
    answerDefinition=nanswerDefinition
    if(answerDefinition.name.length>0) {
      infoLabel.innerHTML=answerDefinition.name
    }
    panel
  }

  def reset():Unit

  def focus():Unit
}

class StringAnswerPanel extends AnswerPanel {
  def typeString="text"
  val inputField=ActiveEditField(typeString,"",text=> parseText(text) match {
    case Some(const)=> DialogManager.answerGiven(answerDefinition,const)
    case None=> println("cant parse "+text)
  })

  override def loadAnswerParam(nanswerDefinition: AnswerDefinition): Node = {
    super.loadAnswerParam(nanswerDefinition)
    panel.appendChild(inputField)
    panel
  }


  def parseText(st:String):Option[Constant]=Some(StringConstant(st))

  override def reset(): Unit = inputField.value=""

  override def focus(): Unit = inputField.focus()
}

class DoubleAnswerPanel extends StringAnswerPanel {
  override def parseText(st: String): Option[Constant] = st match {
    case StrToDouble(d) => Some(new DoubleConstant(d))
    case _ => None
  }
}

class IntAnswerPanel extends StringAnswerPanel {
  override def typeString="number"
  override def parseText(st: String): Option[Constant] = st match {
    case StrToInt(d) => Some(IntConstant(d))
    case _ => None
  }
}
