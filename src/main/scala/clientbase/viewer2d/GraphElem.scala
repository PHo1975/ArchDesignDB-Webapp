package clientbase.viewer2d

import definition.data.{Referencable, Reference}
import definition.expression._
import org.denigma.threejs._
import util.Log

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Float32Array

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
  def addGeometry(geom:Object3D):Unit
  def dataUpdated():Unit
}


trait SelectionDecorable {
  def showSelection(): Unit

  def hideSelection(): Unit
}


abstract class GraphElem(override val ref: Reference, val color: Int) extends Formatable with SelectionDecorable {
  def getBounds(container: ElemContainer): Bounds
  def createGeometry(container:ElemContainer):Unit
  def geometry:js.Array[Object3D]

  def showSelection(): Unit = for(obj<-geometry) obj match {
    case m: Mesh ⇒ m.material = GraphElem.selectMaterial
    case _ ⇒
  }

  def hideSelection(): Unit = for(obj<-geometry) obj match {
    case m: Mesh ⇒ m.material = GraphElem.getMaterial(color)
    case _ ⇒
  }
}

class GraphElemStub(override val ref:Reference) extends GraphElem(ref,0)  {
  def getFormatFieldValue(fieldNr:Int):Constant=EMPTY_EX
  def getBounds(container: ElemContainer): Bounds =GraphElem.NULLRECT
  def createGeometry(container:ElemContainer): Unit = {}
  //def hideSelection(): Unit = {}
  def geometry:js.Array[Object3D]=null
}

abstract class LinearElement(nref:Reference,ncolor:Int,val lineWidth:Int,val lineStyle:Int) extends GraphElem(nref,ncolor) {
  protected val _geometry= new js.Array[Object3D]
  def geometry: js.Array[Object3D] =_geometry

  override def getFormatFieldValue(fieldNr:Int):Constant= {
    fieldNr match {
      case 0 => IntConstant(color)
      case 1 => IntConstant(lineWidth)
      case 2 => IntConstant(lineStyle)
      case _ => null
    }
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

  def createGeometry(container:ElemContainer): Unit = {
    val mesh=GraphElem.createLineGeometry(container,startPoint,endPoint,lineWidth,color)
    mesh.name = "L" + ref.instance
    _geometry.push(mesh)
    container.addGeometry(mesh)
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

  override def createGeometry(container:ElemContainer): Unit = {
    try {
      val geom=GraphElem.createCircleGeometry(container,diameter,lineWidth,deltaW,GraphElem.schrittWeiteKreis)
      val mesh = new Mesh(geom, GraphElem.getMaterial(color))
      mesh.position.x = centerPoint.x
      mesh.position.y = centerPoint.y
      mesh.rotation.z = startAngle * Math.PI / 180d
      mesh.name = "C" + ref.instance
      _geometry.push(mesh)
      container.addGeometry(mesh)
    } catch {
      case e: Throwable => util.Log.e("Arc Geo:", e)
    }
  }
}


case class EllipseElement(nref: Reference, ncolor: Int, nlineWidth: Int, nlineStyle: Int, centerPoint: VectorConstant,
                          r1: Double, r2: Double, mainAngle: Double, startAngle: Double, endAngle: Double) extends LinearElement(nref, ncolor, nlineWidth, nlineStyle) {

  private def getInnerAngleFirstQuadrant(outerAngle: Double, delta: Double) =
    math.atan(math.tan(outerAngle) * (r1 + delta) / (r2 + delta))

  lazy val points: Seq[VectorConstant] = List(pointFromAngle(startAngle * math.Pi / 180d), pointFromAngle(endAngle * math.Pi / 180d), centerPoint)
  protected lazy val bounds: Bounds = calcBounds

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

  override def createGeometry(container:ElemContainer): Unit = {
    try {
      val delta = deltaW
      val steps = Math.floor(delta / GraphElem.schrittWeiteEllipse).toInt

      val apoints = new Float32Array((steps + 2) * 3 * 2)
      val thick = (if(lineWidth==0)1 else lineWidth.toDouble)*container.scaleRatio / 100000d

      def setPoints(index: Int, angle: Double): Unit = {
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
      _geometry.push(mesh)
      container.addGeometry(mesh)
    } catch {
      case e: Throwable => util.Log.e("Ellipse Geo:", e)
    }
  }
}

case class TextElement(nref:Reference,ncolor:Int,text:String,position:VectorConstant,fontName:String,fheight:Double,
                       widthRatio:Double,style:Int,
                       textAngle:Double,obligeAngle:Double,lineSpace:Double) extends GraphElem(nref,ncolor) {
  protected val _geometry=new js.Array[Object3D]
  val meshName: String ="T"+ref.instance

  def isStyle(pattern: Int): Boolean = (style & pattern) > 0

  override def geometry:js.Array[Object3D]=_geometry

  protected def calcBounds(container:ElemContainer):Bounds= {
    val height=fheight/1000d*container.scaleRatio
    val width=height*text.length/2
    BRect(position.x-width,position.y-height/2,position.x+width,position.y+height/2)
  }

  override def getBounds(container: ElemContainer): Bounds = calcBounds(container)


  override def createGeometry(container:ElemContainer): Unit = try{

    GraphElem.loadTextGeometry(container,text,fheight,fontName, tg=>{
      tg.head.computeBoundingBox()
      tg.last.computeBoundingBox()
      val textWidth=tg.last.boundingBox.max.x-tg.head.boundingBox.min.x
      val textHeight=fheight/1000*container.scaleRatio
      //println("textheight:"+fheight+" width:"+textWidth)
      for(g<-tg) {
        val mesh = new Mesh(g, GraphElem.getMaterial(color))
        mesh.position.set(position.x+ (if(isStyle(GraphElem.rightStyle))-textWidth
                                      else if(isStyle(GraphElem.hCenterStyle)) -textWidth/2 else 0) ,
                          position.y+ (if(isStyle(GraphElem.bottomStyle))-textHeight
                          else if (isStyle(GraphElem.vCenterStyle)) -textHeight/2 else 0  ), 0f)
        mesh.rotation.z = textAngle * Math.PI / 180d
        mesh.name = meshName
        container.addGeometry(mesh)
        _geometry.push(mesh)
      }
    })

  }catch {
    case e: Throwable => util.Log.e("Font Geo:", e)
  }

//  override def showSelection(): Unit = {}
//
//  override def hideSelection(): Unit = {}

  override def getFormatFieldValue(fieldNr: Int): Constant = fieldNr match {
    case 0 => IntConstant(color)
    case _ => null
  }
}





  object GraphElem {
  final val LAYERTYPE = 39
  final val LINETYPE = 40
  final val ARCTYPE = 41
  final val ELLIPSETYP = 43
  final val TEXTTYP = 44
  final val DIMLINETYP= 46
  final val NULLRECT: BRect = BRect(0, 0, 0, 0)
  final val schrittWeiteKreis = 11.25d
  final val schrittWeiteEllipse: Double = schrittWeiteKreis / 2d
  final val PI_32: Double = math.Pi * 3d / 2d
  final val PI2: Double = math.Pi * 2d
  final val vCenterStyle = 1
  final val bottomStyle = 2
  final val hCenterStyle = 8
  final val rightStyle = 16

  var font:ThreeFont=_
  //val fontMap=mutable.HashMap[String,js.Any]()
  val fontLoader: FontLoader =new FontLoader()
  var isFontLoading:Boolean=false
  val fontLoadCallbacks= new ArrayBuffer[(ThreeFont,Boolean)=>Unit]()

  def createLineGeometry(container:ElemContainer,startPoint:VectorConstant,endPoint:VectorConstant,lineWidth:Double,color:Int): Mesh = {
      val delta = endPoint - startPoint
      val center: VectorConstant = startPoint + (delta * 0.5d)
      val lw = (if(lineWidth==0)1 else lineWidth.toDouble)*container.scaleRatio / 100000d
      //val planeBuffer=new PlaneBufferGeometry(delta.toDouble+lw,lw)
      val mesh = new Mesh(GraphElem.lineGeometry, GraphElem.getMaterial(color))
      mesh.scale.x = delta.toDouble + lw
      mesh.scale.y = lw
      val rot = Math.atan2(delta.y, delta.x)
      mesh.position.x = center.x //-Math.cos(rot)*lw/2d
      mesh.position.y = center.y //-math.sin(rot)*lw/2d
      mesh.rotation.z = rot
      mesh
    }


    def createCircleGeometry(container:ElemContainer,diameter:Double,lineWidth:Double,delta:Double,schrittWeite:Double): MyBufferGeometry = {
      try {
        val steps = Math.floor(delta / schrittWeite).toInt
        //println(" delta:"+delta+" schrittWeite:"+schrittWeite+" steps:"+steps+" diameter:"+diameter+" linewidth:"+lineWidth)
        val apoints = new Float32Array((steps + 2) * 3 * 2)
        val thick = (if(lineWidth==0)1 else lineWidth.toDouble)*container.scaleRatio / 100000d
        val d1 = diameter - thick
        val d2 = diameter + thick

        def setPoints(index: Int, angle: Double): Unit = {
          val radAngle = angle * Math.PI / 180d
          apoints(index) = (Math.cos(radAngle) * d1).toFloat
          apoints(index + 1) = (Math.sin(radAngle) * d1).toFloat
          apoints(index + 3) = (Math.cos(radAngle) * d2).toFloat
          apoints(index + 4) = (Math.sin(radAngle) * d2).toFloat
        }

        //setPoints(0, 0)
        for (st <- 0 to steps)
          setPoints(st * 6, st.toDouble * schrittWeite)
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
        geom
      } catch {
        case e: Throwable => util.Log.e("Arc Geo:", e);null
      }
    }

  def createTextGeometry(container:ElemContainer,text:String,height:Double,Font:ThreeFont): js.Array[ShapeGeometry] = {
    val shapes=font.generateShapes(text,height*container.scaleRatio/1350d,4)
    shapes.map(el=>new ShapeGeometry(el.asInstanceOf[Shape]))
  }

  def loadTextGeometry(container:ElemContainer, text:String, height:Double, fontName:String, callBack: js.Array[ShapeGeometry] =>Unit):Unit = {
    GraphElem.loadFont(fontName,(font,repaint)=>if(repaint)container.dataUpdated() else internCreateGeometry(container,font))

    def internCreateGeometry(container:ElemContainer,font:ThreeFont):Unit= {
      val shapes=font.generateShapes(text,height*container.scaleRatio/1350d,4)

      val geometries: js.Array[ShapeGeometry] =shapes.map(el=>new ShapeGeometry(el.asInstanceOf[Shape]))
      callBack(geometries)
    }
  }

  def loadFont(fontName:String,callback:(ThreeFont,Boolean)=>Unit):Unit= {
    //helvetiker_bold.typeface.json
    val fname="opensans.json"
    if (font!=null) callback(font,false)
    else if(isFontLoading) {
      fontLoadCallbacks+=callback
      //Log.w("add font callback:"+fontLoadCallbacks.size)
    } else {
      isFontLoading=true
      fontLoader.load(fname, (nfont: ThreeFont) => {
        font=nfont
        println("load font "+font)
        callback(font,false)
        for(cb<-fontLoadCallbacks)cb(font,false)
        fontLoadCallbacks.last(null,true)
        fontLoadCallbacks.clear()
        isFontLoading=false
      })
    }
  }

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