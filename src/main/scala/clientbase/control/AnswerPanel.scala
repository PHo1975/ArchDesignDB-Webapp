package clientbase.control

import definition.expression.{IntConstant, DoubleConstant, StringConstant, Constant}
import definition.typ.AnswerDefinition
import util.{StrToInt, StrToDouble}
import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 17.01.2016.
  */
abstract class AnswerPanel {
  val panel=div(`class`:="answer-panel").render
  var answerDefinition:AnswerDefinition= _

  def loadAnswerParam(nanswerDefinition: AnswerDefinition)= {
    answerDefinition=nanswerDefinition
    if(answerDefinition.name.length>0) {
      val infoLabel = span(`class`:="answer-label").render
      infoLabel.innerHTML=answerDefinition.name
      panel.appendChild(infoLabel)
    }
  }

  def reset():Unit

  def focus():Unit
}

class StringAnswerPanel extends AnswerPanel {
  def typeString="text"
  val inputField=ActiveEditField(typeString,"",(text)=> parseText(text) match {
    case Some(const)=> DialogManager.answerGiven(answerDefinition,const)
    case None=> println("cant parse "+text)
  })

  override def loadAnswerParam(nanswerDefinition: AnswerDefinition)= {
    super.loadAnswerParam(nanswerDefinition)
    panel.appendChild(inputField)
  }


  def parseText(st:String):Option[Constant]=Some(new StringConstant(st))

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
    case StrToInt(d) => Some(new IntConstant(d))
    case _ => None
  }
}
