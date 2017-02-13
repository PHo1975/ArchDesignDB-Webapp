package clientbase.viewer2d

import definition.data.{Referencable, Reference}
import definition.expression._
import org.scalajs.dom.ext.Color
import org.scalajs.dom.raw.CanvasRenderingContext2D
import util.Log

/**
  * Created by Peter Holzer on 11.02.2017.
  */

trait Bounds{
  def minX:Double
  def minY:Double
  def maxX:Double
  def maxY:Double
}

case class BRect(minX:Double,minY:Double,maxX:Double,maxY:Double) extends Bounds

trait Formatable extends Referencable {
  def getFormatFieldValue(fieldNr: Int): Constant
}

trait ElemContainer {
  def scaleRatio:Double
}

abstract class GraphElem(override val ref:Reference,val color:Int) extends Formatable {
   def draw(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color]):Unit
   def drawWithOffset(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],offSet:VectorConstant):Unit
   def drawRotated(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],dangle:Double,rotator:VectorConstant=>VectorConstant): Unit
   def getBounds(container: ElemContainer):Bounds
}

class GraphElemStub(override val ref:Reference) extends GraphElem(ref,0)  {
  def getFormatFieldValue(fieldNr:Int):Constant=EMPTY_EX
  def draw(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color]):Unit={}
  def drawWithOffset(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],offSet:VectorConstant):Unit={}
  def drawRotated(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],dangle:Double,rotator:VectorConstant=>VectorConstant): Unit={}
  def getBounds(container: ElemContainer): Bounds =GraphElem.NULLRECT
}

abstract class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor) {
  override def getFormatFieldValue(fieldNr:Int):Constant= {
    fieldNr match {
      case 0 => IntConstant(color)
      case 1 => IntConstant(lineWidth)
      case 2 => IntConstant(lineStyle)
      case _ => null
    }
  }
  protected def prepareStroke(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color])={
    ctx.lineWidth=lineWidth/100d/sm.dotPitch
    ctx.strokeStyle="rgb("+(color/256/256)+","+(color/256)%256+","+(color%256)+")"
  }
}

abstract class AbstractLineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,val startPoint:VectorConstant,val endPoint:VectorConstant) extends
  LinearElement(nref,ncolor,nlineWidth,nlineStyle) with Bounds {

  def minX: Double = scala.math.min(startPoint.x, endPoint.x)
  def minY: Double = scala.math.min(startPoint.y, endPoint.y)
  def maxX: Double = scala.math.max(startPoint.x, endPoint.x)
  def maxY: Double = scala.math.max(startPoint.y, endPoint.y)

  override def getBounds(container: ElemContainer): Bounds = this
}



// ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
case class LineElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,nstartPoint:VectorConstant,nendPoint:VectorConstant) extends
  AbstractLineElement(nref,ncolor,nlineWidth,nlineStyle,nstartPoint,nendPoint) {

  def draw(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color]):Unit=intDraw(ctx,sm,selectColor,nstartPoint,nendPoint)

  override def drawWithOffset(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],offSet:VectorConstant)=
    intDraw(ctx,sm,selectColor,startPoint+offSet,endPoint+offSet)

  override def drawRotated(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],angle:Double,rotator:VectorConstant=>VectorConstant)=
    intDraw(ctx,sm,selectColor,rotator(startPoint),rotator(endPoint))

  def intDraw(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],p1:VectorConstant,p2:VectorConstant):Unit={
    prepareStroke(ctx,sm,selectColor)
    ctx.strokeStyle="black"
    ctx.beginPath()
    ctx.moveTo(sm.xToScreen(p1.x) ,sm.yToScreen(p1.y))
    ctx.lineTo(sm.xToScreen(p2.x),sm.yToScreen(p2.y))
    ctx.stroke()
    //println("Line "+p1+" - "+p2+" x1:"+sm.xToScreen(p1.x)+" y1:"+sm.yToScreen(p1.y)+" x2:"+sm.xToScreen(p2.x)+" y2:"+sm.yToScreen(p2.y))
  }
}

// ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
case class ArcElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
                      diameter:Double,startAngle:Double,endAngle:Double) extends
  LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
  //println("create arc "+ref+" "+centerPoint)
  lazy val points:Seq[VectorConstant]=List(pointFromAngle(startAngle),pointFromAngle(endAngle),centerPoint)
  protected lazy val bounds: BRect = calcArcBounds

  def getBounds(container: ElemContainer): Bounds =bounds

  override def draw(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color]):Unit= drawWithOffset(ctx,sm,selectColor,NULLVECTOR)

  override def drawWithOffset(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],offSet:VectorConstant):Unit=
    internDraw(ctx,sm,selectColor,centerPoint+offSet,0d)

  override def drawRotated(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],dangle:Double,rotator:VectorConstant=>VectorConstant): Unit =
    internDraw(ctx,sm,selectColor,rotator(centerPoint),dangle)

  private def internDraw(ctx:CanvasRenderingContext2D,sm:Scaler,selectColor:Option[Color],cPoint:VectorConstant,angle:Double)={
    prepareStroke(ctx,sm,selectColor)
    val sAngle=(startAngle+angle)*Math.PI/180d
    val eAngle=(endAngle+angle)*Math.PI/180d
    val tx=sm.xToScreen(cPoint.x)
    val ty=sm.yToScreen(cPoint.y)
    ctx.beginPath()
    ctx.arc(tx,ty,diameter*sm.scale,-eAngle,-sAngle,anticlockwise = false)
    ctx.stroke()
    val mx=sm.xToScreen(cPoint.x)
    val my=sm.yToScreen(cPoint.y)
    ctx.moveTo(mx,my)
    ctx.lineTo(mx,my)
    ctx.stroke()
  }

  def pointFromAngle(angle:Double) =
    new VectorConstant(centerPoint.x+scala.math.cos(angle*scala.math.Pi/180d)*diameter,
      centerPoint.y+scala.math.sin(angle*scala.math.Pi/180d)*diameter,0)

  def calcArcBounds: BRect = {
    val pointBuffer = collection.mutable.ArrayBuffer[VectorConstant]() += points.head += points.tail.head
    val sa = startAngle % 360d
    var ea = (if (endAngle < sa) endAngle + 360d else endAngle) % 360d
    if (ea == sa) ea += 360d
    var nextSegmentAngle = ((scala.math.floor(sa / 90d) + 1d) * 90d) % 360d
    //System.out.println("startAngle "+sa+" "+nextSegmentAngle+" ea:"+ea)
    while (nextSegmentAngle < ea) {
      val np = pointFromAngle(nextSegmentAngle)
      //System.out.println("nxa:"+nextSegmentAngle+"Np "+np)
      pointBuffer += np
      nextSegmentAngle += 90d
    }
    GraphElem.getPointsBounds(pointBuffer)
  }
}


object GraphElem {
  val LAYERTYPE=39
  val LINETYPE=40
  val ARCTYPE=41
  val NULLRECT: BRect = BRect(0,0,0,0)
  def getPointsBounds(points:Seq[VectorConstant]):BRect =
    if(points==null && points.isEmpty) {Log.e("getPointBounds "+points);null}
    else 	{
      var x1=points.head.x
      var x2=points.head.x
      var y1=points.head.y
      var y2=points.head.y
      if(points.size>1) for(ix <-1 until points.size) {
        val p=points(ix)
        if(p.x<x1){ x1=p.x }
        if(p.y<y1){ y1=p.y }
        if(p.x>x2) x2=p.x
        if(p.y>y2) y2=p.y
      }
      BRect(x1,y1,x2,y2)
    }
}