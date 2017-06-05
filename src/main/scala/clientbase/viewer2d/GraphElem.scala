package clientbase.viewer2d

import definition.data.{ Referencable, Reference }
import definition.expression._
import org.denigma.threejs._
import util.Log
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Float32Array

/**
  * Created by Peter Holzer on 11.02.2017.
  */


@js.native
@JSGlobal("THREE.BufferGeometry")
class MyBufferGeometry extends Geometry {
  var attributes: js.Array[BufferAttribute] = js.native
  var drawcalls: js.Any = js.native
  var offsets: js.Any = js.native

  def addAttribute(name: String, attribute: BufferAttribute): js.Dynamic = js.native

  def addAttribute(name: String, array: js.Any, itemSize: Double): js.Dynamic = js.native

  def getAttribute(name: String): js.Dynamic = js.native

  def setIndex(index: js.Array[Int]): Unit = js.native

  def addDrawCall(start: Double, count: Double, index: Double): Unit = js.native

  def fromGeometry(geometry: Geometry, settings: js.Any = js.native): BufferGeometry = js.native

  def computeVertexNormals(): Unit = js.native

  def computeOffsets(indexBufferSize: Double): Unit = js.native

  def merge(): Unit = js.native

  def normalizeNormals(): Unit = js.native

  def reorderBuffers(indexBuffer: Double, indexMap: js.Array[Double], vertexCount: Double): Unit = js.native

  override def clone(): BufferGeometry = js.native
}


@js.native
@JSGlobal("THREE.PlaneBufferGeometry")
class PlaneBufferGeometry extends Geometry {
  def this(width: Double, height: Double, widthSegments: Double = js.native, heightSegments: Double = js.native) = this()

  var parameters: js.Any = js.native
}

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


trait SelectionDecorable {
  def showSelection(): Unit

  def hideSelection(): Unit
}


abstract class GraphElem(override val ref: Reference, val color: Int) extends Formatable with SelectionDecorable {
  def getBounds(container: ElemContainer): Bounds
  def geometry: Object3D
}

class GraphElemStub(override val ref:Reference) extends GraphElem(ref,0)  {
  def getFormatFieldValue(fieldNr:Int):Constant=EMPTY_EX
  def getBounds(container: ElemContainer): Bounds =GraphElem.NULLRECT
  def geometry: Object3D = null

  def showSelection(): Unit = {}

  def hideSelection(): Unit = {}
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

  def showSelection(): Unit = geometry match {
    case m: Mesh ⇒ m.material = GraphElem.selectMaterial;
    case _ ⇒
  }

  def hideSelection(): Unit = geometry match {
    case m: Mesh ⇒ m.material = GraphElem.getMaterial(ncolor);
    case _ ⇒
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

  lazy val geometry: Object3D = {
    val delta = endPoint - startPoint
    val center: VectorConstant = startPoint + (delta * 0.5d)
    val lw = lineWidth.toDouble / 1000d
    //val planeBuffer=new PlaneBufferGeometry(delta.toDouble+lw,lw)
    val mesh = new Mesh(GraphElem.lineGeometry, GraphElem.getMaterial(color))
    mesh.scale.x = delta.toDouble + lw
    mesh.scale.y = lw
    val rot = Math.atan2(delta.y, delta.x)
    mesh.position.x = center.x //-Math.cos(rot)*lw/2d
    mesh.position.y = center.y //-math.sin(rot)*lw/2d
    mesh.rotation.z = rot
    mesh.name = "L" + ref.instance
    mesh
  }
}

// ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
case class ArcElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,centerPoint:VectorConstant,
                      diameter:Double,startAngle:Double,endAngle:Double) extends
  LinearElement(nref,ncolor,nlineWidth,nlineStyle) {
  //println("create arc "+ref+" "+centerPoint)
  lazy val points:Seq[VectorConstant]=List(pointFromAngle(startAngle),pointFromAngle(endAngle),centerPoint)
  protected lazy val bounds: BRect = calcArcBounds

  def deltaW: Double = {
    val value = endAngle - startAngle + (if (endAngle < startAngle) 360d else 0)
    if (value == 360d) 360d else value % 360d
  }

  def getBounds(container: ElemContainer): Bounds =bounds


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

  lazy val geometry: Object3D = {
    try {
      val delta = deltaW
      val steps = Math.floor(delta / GraphElem.schrittWeiteKreis).toInt
      //println("startA:" + startAngle + " endA:" + endAngle+" delta:"+delta+" steps:"+steps)
      val apoints = new Float32Array((steps + 2) * 3 * 2)
      val thick = lineWidth.toDouble / 1000d / 2
      val d1 = diameter - thick
      val d2 = diameter + thick

      def setPoints(index: Int, angle: Double) = {
        val radAngle = angle * Math.PI / 180d
        apoints(index) = (Math.cos(radAngle) * d1).toFloat
        apoints(index + 1) = (Math.sin(radAngle) * d1).toFloat
        apoints(index + 3) = (Math.cos(radAngle) * d2).toFloat
        apoints(index + 4) = (Math.sin(radAngle) * d2).toFloat
      }

      //setPoints(0, 0)
      for (st <- 0 to steps)
        setPoints(st * 6, st.toDouble * GraphElem.schrittWeiteKreis)
      setPoints((steps + 1) * 6, delta)
      val geom = new MyBufferGeometry()
      val indices = new js.Array[Int]((steps + 1) * 6)
      var current = 0
      for (i <- 0 until (steps + 1) * 2; l <- 0 to 2) {
        indices(current) = if (i % 2 == 0) i + l else i + 2 - l
        current += 1
      }
      geom.setIndex(indices)
      geom.addAttribute("position", new BufferAttribute(apoints, 3))
      geom.computeFaceNormals()
      geom.computeBoundingSphere()
      val mesh = new Mesh(geom, GraphElem.getMaterial(color))
      mesh.position.x = centerPoint.x
      mesh.position.y = centerPoint.y
      mesh.rotation.z = startAngle * Math.PI / 180d
      mesh.name = "C" + ref.instance
      mesh
    } catch {
      case e: Throwable => util.Log.e("Arc Geo:", e); null
    }
  }
}


case class EllipseElement(nref: Reference, ncolor: Int, nlineWidth: Int, nlineStyle: Int, centerPoint: VectorConstant,
                          r1: Double, r2: Double, mainAngle: Double, startAngle: Double, endAngle: Double) extends LinearElement(nref, ncolor, nlineWidth, nlineStyle) {

  private def getInnerAngleFirstQuadrant(outerAngle: Double, delta: Double) =
    math.atan(math.tan(outerAngle) * (r1 + delta) / (r2 + delta))

  lazy val points: Seq[VectorConstant] = List(pointFromAngle(startAngle * math.Pi / 180d), pointFromAngle(endAngle * math.Pi / 180d), centerPoint)
  protected lazy val bounds: Bounds = calcBounds

  //println("ellipse  r1:"+r1+" r2:"+r2+" ma:"+mainAngle+" sa:"+startAngle+" ea:"+endAngle)
  //println("bounds "+bounds+" points:"+points.mkString(","))

  def getInnerAngle(outerAngle: Double, delta: Double): Double =
    if (outerAngle > math.Pi / 2) {
      if (outerAngle > GraphElem.PI_32) getInnerAngleFirstQuadrant(outerAngle - math.Pi * 2, delta) + math.Pi * 2 else getInnerAngleFirstQuadrant(outerAngle - math.Pi, delta) + math.Pi
    }
    else if (outerAngle < -math.Pi / 2) {
      if (outerAngle < -GraphElem.PI_32) getInnerAngleFirstQuadrant(outerAngle + math.Pi * 2, delta) - math.Pi * 2 else getInnerAngleFirstQuadrant(outerAngle + math.Pi, delta) - math.Pi
    }
    else getInnerAngleFirstQuadrant(outerAngle, delta)

  def pointFromAngle(angle: Double, delta: Double = 0d): VectorConstant = {
    val ia = getInnerAngle(angle % GraphElem.PI2, delta)
    val dx = math.cos(ia) * (r1 + delta)
    val dy = math.sin(ia) * (r2 + delta)
    val ma = mainAngle * math.Pi / 180d
    val cosMa = math.cos(ma)
    val sinMa = math.sin(ma)
    new VectorConstant(dx * cosMa - dy * sinMa + centerPoint.x, dx * sinMa + dy * cosMa + centerPoint.y, 0)
  }

  def calcBounds: Bounds = {
    val pointBuffer = collection.mutable.ArrayBuffer[VectorConstant]() += points.head += points.tail.head
    val ea = if (endAngle < startAngle) endAngle + 360 else endAngle
    var nextSegmentAngle = (scala.math.floor(startAngle / 90) + 1) * 90
    while (nextSegmentAngle < ea) {
      val np = pointFromAngle(nextSegmentAngle * math.Pi / 180d)
      pointBuffer += np
      nextSegmentAngle += 90
    }
    GraphElem.getPointsBounds(pointBuffer)
  }

  override def getBounds(container: ElemContainer): Bounds = bounds

  def deltaW: Double = {
    val value = endAngle - startAngle + (if (endAngle < startAngle) 360d else 0)
    if (value == 360d) 360d else value % 360d
  }

  override lazy val geometry: Object3D = {
    try {
      val delta = deltaW
      val steps = Math.floor(delta / GraphElem.schrittWeiteEllipse).toInt

      val apoints = new Float32Array((steps + 2) * 3 * 2)
      val thick = lineWidth.toDouble / 1000d / 2

      def setPoints(index: Int, angle: Double) = {
        val radAngle = (angle + startAngle) * Math.PI / 180d
        val p1 = pointFromAngle(radAngle, -thick)
        val p2 = pointFromAngle(radAngle, thick)
        apoints(index) = p1.x.toFloat
        apoints(index + 1) = p1.y.toFloat
        apoints(index + 3) = p2.x.toFloat
        apoints(index + 4) = p2.y.toFloat
      }

      // setPoints(0, 0)
      for (st <- 0 to steps)
        setPoints(st * 6, st.toDouble * GraphElem.schrittWeiteEllipse)
      setPoints((steps + 1) * 6, delta)

      val geom = new MyBufferGeometry()
      val indices = new js.Array[Int]((steps + 1) * 6)
      var current = 0
      for (i <- 0 until (steps + 1) * 2; l <- 0 to 2) {
        indices(current) = if (i % 2 == 0) i + l else i + 2 - l
        current += 1
      }
      geom.setIndex(indices)
      geom.addAttribute("position", new BufferAttribute(apoints, 3))
      geom.computeFaceNormals()
      geom.computeBoundingSphere()
      val mesh = new Mesh(geom, GraphElem.getMaterial(color))
      //mesh.position.x = centerPoint.x
      //mesh.position.y = centerPoint.y
      //mesh.rotation.z = startAngle * Math.PI / 180d
      mesh.name = "E" + ref.instance
      mesh
    } catch {
      case e: Throwable => util.Log.e("Ellipse Geo:", e); null
    }
  }
}



object GraphElem {
  final val LAYERTYPE = 39
  final val LINETYPE = 40
  final val ARCTYPE = 41
  final val ELLIPSETYP = 43
  final val NULLRECT: BRect = BRect(0, 0, 0, 0)
  final val schrittWeiteKreis = 11.25d
  final val schrittWeiteEllipse: Double = schrittWeiteKreis / 2d
  final val PI_32: Double = math.Pi * 3d / 2d
  final val PI2: Double = math.Pi * 2d
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

  val materialCache: mutable.HashMap[Int, MeshBasicMaterial] = collection.mutable.HashMap[Int, MeshBasicMaterial]()

  def getMaterial(color: Int): MeshBasicMaterial = materialCache.getOrElseUpdate(color, {
    val c = new Color((color / 256 / 256).toDouble / 256d, ((color / 256) % 256).toDouble / 256d, (color % 256).toDouble / 256d)
    val mat = new MeshBasicMaterial
    mat.color = c
    mat.side = THREE.DoubleSide
    mat
  })

  val selectMaterial: MeshBasicMaterial = getMaterial(255)

  final val lineGeometry = new PlaneBufferGeometry(1, 1)

  val lineCache: mutable.HashMap[(Double, Double), PlaneBufferGeometry] = collection.mutable.HashMap[(Double, Double), PlaneBufferGeometry]()

  def getLine(length: Double, width: Double): PlaneBufferGeometry = lineCache.getOrElseUpdate((length, width), {
    new PlaneBufferGeometry(length, width)
  })
}