package clientbase.control

import definition.typ.{AnswerDefinition, CommandQuestion, DataType, DialogQuestion, ParamQuestion}

trait CustomQuestionHandler {
  def load(question:CommandQuestion,container:FocusContainer):Unit

  def singlePointQuestion(actionText: String, questionText: String, strict: Option[Boolean], repeat: Boolean = false) =
    DialogQuestion(actionText, Seq(new AnswerDefinition(questionText, DataType.VectorTyp, None, strict match {
      case Some(true) => PointAnswerPanel.STRICT_HIT
      case Some(false) => PointAnswerPanel.NOSTRICT_HIT
      case None => ""
    })), repeat)

  def singleNumberQuestion(actionText: String, questionText: String, repeat: Boolean = false) = DialogQuestion(actionText, Seq(new AnswerDefinition(questionText, DataType.DoubleTyp, None)), repeat)

  def singleTextQuestion(actionText: String, questionText: String, repeat: Boolean = false) = DialogQuestion(actionText, Seq(new AnswerDefinition(questionText, DataType.StringTyp, None)), repeat)

  def singleIntQuestion(actionText: String, questionText: String, repeat: Boolean = false) = DialogQuestion(actionText,
    Seq(new AnswerDefinition(questionText, DataType.IntTyp, None, AnswerDefinition.NonNullConstraint)), repeat)

  lazy val moveStartAnswers: Seq[AnswerDefinition] =Seq(new AnswerDefinition("'von Punkt' angeben",DataType.VectorTyp, None ),
    new AnswerDefinition("Delta X eingeben:",DataType.DoubleTyp,None)
  )

}


