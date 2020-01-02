package clientbase.viewer2d

import clientbase.control.{CustomQuestionHandler, DialogManager, FocusContainer, PointAnswerPanel}
import definition.data.EMPTY_REFERENCE
import definition.expression.{DoubleConstant, VectorConstant}
import definition.typ.{AnswerDefinition, CommandQuestion, DataType, DialogQuestion, ParamQuestion}
import org.denigma.threejs.Vector3
import org.scalajs.dom.CanvasRenderingContext2D
import util.Log

object GraphQuestionHandler extends CustomQuestionHandler {
  val lineToText="Linie zeichnen"
  val polyToText="Polygon zeichnen"
  val dimHelpLineToText="Hilfslinien bis"
  val lineDistance = 1.3d

  val funcMap: collection.immutable.HashMap[String, Viewer2DController => Unit] = collection.immutable.HashMap[String, Viewer2DController => Unit](
    "LineTo" -> lineTo(lineToText, create = true, separateElements = true))

  override def load(question: CommandQuestion, container: FocusContainer): Unit = {
    container match {
      case v:Viewer2DController=> if(funcMap.contains(question.commandName)) funcMap(question.commandName).apply(v) else Log.e("Unknown Graph Question "+question.commandName)
      case o=> Log.e("Unknown Focus Container "+o)
    }

  }

  def lineNextPointQuestion(actionText: String, strict: Boolean = true): DialogQuestion = //     singlePointQuestion(actionText,"bis Punkt",true)
    DialogQuestion(actionText, Seq(new AnswerDefinition("bis Punkt", DataType.VectorTyp, None, if (strict) "" else PointAnswerPanel.NOSTRICT_HIT),
      new AnswerDefinition("dx", DataType.DoubleTyp, None), new AnswerDefinition("dy", DataType.DoubleTyp, None),
      new AnswerDefinition("Winkel", DataType.DoubleTyp, None)), repeat = true)

  def lineLengthQuestion(actionText: String) = DialogQuestion(actionText, Seq(new AnswerDefinition("Länge", DataType.DoubleTyp, None), new AnswerDefinition("durch Punkt", DataType.VectorTyp, None)))


  def lineTo(lineText: String, create: Boolean, separateElements: Boolean, strict: Boolean = true)(gc: Viewer2DController): Unit = {
    println(" line To "+lineText+" ")
    var lastPoint: VectorConstant = null

    def lineDragger(pos: VectorConstant, g: CanvasRenderingContext2D): Unit = {
      val sm = gc.scaleModel
      val lastScreenPos: Vector3 =gc.toScreenPosition(lastPoint.x,lastPoint.y)
      gc.canvasHandler.drawLine(g,lastScreenPos.x,lastScreenPos.y,pos.x,pos.y)
    }

    def lineQuestion(actionText: String, create: Boolean, strict: Boolean = true) = DialogQuestion(actionText, Seq(
      new AnswerDefinition("von Punkt", DataType.VectorTyp, None, if (create) "Create" else if (strict) "" else PointAnswerPanel.NOSTRICT_HIT)))


    DialogManager.startIntermediateQuestion(lineQuestion(lineText, create, strict), answerList => {
      lastPoint = answerList.head.result.toVector
      gc.setCustomDragger(lineDragger)

      def askSecondPoint(): Unit = {
        DialogManager.startIntermediateQuestion(lineNextPointQuestion(lineText, strict), answerList => {
          //println("Answer:"+answerList.mkString(" | "))
          def handlePointAnswer(p: VectorConstant): Unit = {
            println("Handle answer "+p)
            if (separateElements) DialogManager.increaseNumCreatedElements()
            gc.addTempElement(LineElement(EMPTY_REFERENCE, 0, 10, 0, lastPoint, p))
            lastPoint = p
            gc.setCustomDragger(lineDragger)
          }

          answerList.last.result match {
            case v: VectorConstant => handlePointAnswer(v)
            case d: DoubleConstant => answerList.last.paramName match {
              case "dx" => handlePointAnswer(lastPoint + new VectorConstant(d.toDouble, 0d, 0d))
              case "dy" => handlePointAnswer(lastPoint + new VectorConstant(0d, d.toDouble, 0d))
              case "Winkel" =>
                def angleDragger(pos: VectorConstant, g: CanvasRenderingContext2D): Unit = {
                  val sm = gc.scaleModel
                  val delta = pos - lastPoint
                  val orth = lastPoint + delta.orthoProjection(VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d))
                  val lastScreenPos: Vector3 =gc.toScreenPosition(lastPoint.x,lastPoint.y)
                  val orthPos=gc.toScreenPosition(orth.x,orth.y)
                  gc.canvasHandler.drawLine(g, lastScreenPos.x,lastScreenPos.y, orthPos.x,orthPos.y)
                }

                gc.setCustomDragger(angleDragger)
                DialogManager.startIntermediateQuestion(lineLengthQuestion(lineText), listener = answerList => {
                  answerList.last.result match {
                    case length: DoubleConstant =>
                      val newPoint = lastPoint + VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d) * length.toDouble
                      handlePointAnswer(newPoint)
                      //println("answer given length:" + length.toDouble)
                      askSecondPoint()
                    case dirPoint: VectorConstant =>
                      val delta = dirPoint - lastPoint
                      val orth = lastPoint + delta.orthoProjection(VectorConstant.fromAngle2D(d.toDouble * Math.PI / 180d))
                      handlePointAnswer(orth)
                      askSecondPoint()
                    case o => Log.e("unknown answer " + o)

                  }
                })
            }
            case o => throw new IllegalArgumentException("Wrong answer:" + o)
          }

        })
      }

      askSecondPoint()
    })
  }

}
