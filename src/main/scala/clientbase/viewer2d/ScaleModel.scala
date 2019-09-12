package clientbase.viewer2d

import clientbase.connection.WebObjectClass
import definition.expression.VectorConstant
import definition.typ.AllClasses
import util.{ColonSplit, Log, StrToDouble}

import scala.collection.immutable.SortedMap

/**
  * Created by Peter Holzer on 11.02.2017.
  */
object ScaleModel {
  val _dotPitch: Double = 0.25d
  // display resolution in mm/pix
  var scales: Map[Int, Double] = Map.empty


  private val cl =AllClasses.get.getClassByID(GraphElem.LAYERTYPE).asInstanceOf[WebObjectClass]
  //println("Scale " +cl.enumFields.values.mkString("|")+" size:"+cl.enumFields.size)
  if(cl.enumFields.values.nonEmpty)
  scales= SortedMap(cl.enumFields.values.head.enumValues.map(v=>(v._2,ScaleModel.stringToScale(v._1))).toSeq :_*)
  else println("could not init scales !")

  def stringToScale(st: String): Double = {
    st match {
      case ColonSplit(StrToDouble(f1), StrToDouble(f2)) => f1 / f2
      case _ => 1d
    }
  }
}

trait Scaler {
  def xToScreen(wx:Double):Double
  def yToScreen(wy:Double):Double

  def xToWorld(x:Int) :Double
  def yToWorld(y:Int) :Double

  def scale:Double
  def relScaleFactor:Double
  def thicknessScale:Double
  def thicknessToScreen: Double =if(thicknessScale<0) 1d/10d else thicknessScale/10d
  //def getStroke(thick:Float,style:Int):java.awt.BasicStroke
  def dotPitch:Double
  def isPrintScaler:Boolean
  def colorsFixed:Boolean
  def textScale:Double=1d
}


class ScaleModel extends Scaler {

  def dotPitch: Double =ScaleModel._dotPitch

  val vpBorder = 10 // border of the Canvas

  private var _viewSizeX = 1
  // size of the ViewPort component
  private var _viewSizeY = 1

  private var zoomStack = collection.immutable.List[BRect]()

  var _world_X: Double = _
  // pos of the ViewPort in real world dimensions
  var _world_Y: Double = _
  protected var _world_Width: Double = 1
  // size of the ViewPort in real world dimensions
  protected var _world_Height: Double = 1
  protected var _heightSet: Boolean = _
  // is the world height or width relevant for scaling
  protected var xOffset = 0d
  // screen offset to center the drawing
  protected var yOffset = 0d
  // world border
  protected var wbx1: Double = 0
  protected var wby1: Double = 0
  protected var wbx2: Double = 0
  protected var wby2: Double = 0

  protected var _relativeScale: (Double, Double) = (1d, 100d)
  protected var _thicknessScale = 1d
  var colorsFixed = true // Color==Pen coupling

  def isPrintScaler = false

  def thicknessScale: Double = _thicknessScale

  def world_Width: Double = _world_Width

  def world_Height: Double = _world_Height

  private val scaleListeners = collection.mutable.HashSet[() => Unit]()

  //val strokeMap=collection.mutable.HashMap[(Int),BasicStroke]()

  def relScaleFactor: Double = _relativeScale._2 / _relativeScale._1

  /*def getStroke(thick:Float,style:Int)={
    //if(thick<0) System.err.println("Stroke thick :"+thick)
    val key=thick.hashCode+style.toShort*Short.MaxValue
    strokeMap.getOrElseUpdate(key,LineStyleHandler.createStroke(thicknessToScreen,thick,style))
  }*/

  def viewSize: (Int, Int) = (_viewSizeX, _viewSizeY)

  def setViewSize(x: Int, y: Int): Unit = {
    if (x <= 0 || y <= 0) {
      Log.e("wrong viewsize " + x + " " + y)
    } else if (x - vpBorder * 2 != _viewSizeX ||
      y - vpBorder * 2 != _viewSizeY) {
      _viewSizeX = x - vpBorder * 2
      _viewSizeY = y - vpBorder * 2
      calcOffsets()
      notifyScaleChanged()
    }

  }


  def setWorldBounds(x: Double, y: Double, w: Double, h: Double): Unit = {
    //System.out.println("Set world bounds :x="+x+" y="+y+" w="+w+" h="+h+" "+Thread.currentThread().getStackTrace()(2))
    _world_X = x
    _world_Y = y
    _world_Width = if (w == 0) 0.1 else w
    _world_Height = if (h == 0) 0.1 else h
    zoomStack = collection.immutable.List(BRect(_world_X, _world_Y, _world_Width, _world_Height))
    calcOffsets()
    notifyScaleChanged()
  }

  def zoomIn(startx: Int, starty: Int, endx: Int, endy: Int): Unit = {
    val x1 = xToWorld(startx)
    val x2 = xToWorld(endx)
    val y1 = yToWorld(starty)
    val y2 = yToWorld(endy)
    _world_X = math.min(x1, x2)
    _world_Width = math.abs(math.max(x1, x2) - world_X)
    _world_Y = math.min(y1, y2)
    _world_Height = math.abs(math.max(y1, y2) - world_Y)
    zoomStack = BRect(_world_X, _world_Y, _world_Width, _world_Height) :: zoomStack
    calcOffsets()
    notifyScaleChanged()
  }

  def zoomOut(): Unit = {
    //System.out.println("zoomout" + zoomStack)
    if (zoomStack.tail != Nil) {
      zoomStack = zoomStack.tail
      _world_X = zoomStack.head.minX
      _world_Y = zoomStack.head.minY
      _world_Width = zoomStack.head.maxX
      _world_Height = zoomStack.head.maxY
    } else {
      // zoom farther out
      _world_X = wbx1 - (wbx2 - wbx1) / 4
      _world_Width = (wbx2 - wbx1) * 1.5
      _world_Y = wby1 - (wby2 - wby1) / 4
      _world_Height = (wby2 - wby1) * 1.5
      zoomStack = collection.immutable.List(BRect(_world_X, _world_Y, _world_Width, _world_Height))
    }
    calcOffsets()
    notifyScaleChanged()
  }

  def zoomPlus(): Unit = {
    _world_X = wbx1 + (wbx2 - wbx1) / 8d
    _world_Width = (wbx2 - wbx1) / 4d*3
    _world_Y = wby1 + (wby2 - wby1) / 8d
    _world_Height = (wby2 - wby1) / 4d*3
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def zoomMinus(): Unit = {
    _world_X = wbx1 - (wbx2 - wbx1) / 6d
    _world_Width = (wbx2 - wbx1) / 3d*4
    _world_Y = wby1 - (wby2 - wby1) / 6d
    _world_Height = (wby2 - wby1) / 3d*4
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def zoomOutBy(factor: Double): Unit = {
    val w = wbx2 - wbx1
    val h = wby2 - wby1
    _world_X = wbx1 - w * factor
    _world_Width = w + w * factor * 2
    _world_Y = wby1 - h * factor
    _world_Height = h + h * factor * 2
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def zoomInBy(factor: Double): Unit = {
    val w = wbx2 - wbx1
    val h = wby2 - wby1
    if (w > 2 * factor * 2 && h > h * factor * 2) {
      _world_X = wbx1 + w * factor
      _world_Width = w - w * factor * 2
      _world_Y = wby1 + h * factor
      _world_Height = h - h * factor * 2
      updateTopStackElement()
      calcOffsets()
      notifyScaleChanged()
    }
  }

  def updateTopStackElement(): Unit = zoomStack = BRect(_world_X, _world_Y, _world_Width, _world_Height) :: zoomStack.tail

  def moveLeft(): Unit = {
    _world_X -= _world_Width / 4
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def moveRight(): Unit = {
    _world_X += _world_Width / 4
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def moveUp(): Unit = {
    _world_Y += _world_Height / 4
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def moveDown(): Unit = {
    _world_Y -= _world_Height / 4
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }

  def move(dx: Double, dy: Double): Unit = {
    _world_X +=dx/scale
    _world_Y -=dy/scale
    updateTopStackElement()
    calcOffsets()
    notifyScaleChanged()
  }


  private def calcOffsets(): Unit = {
    if (_viewSizeX == 0 || _viewSizeY == 0) Log.e("Calc Offsets viewsize:" + _viewSizeX + " " + _viewSizeY)
    else {
      val worldRatio = _world_Width / _world_Height
      val viewRatio = _viewSizeX.toDouble / _viewSizeY.toDouble
      _heightSet = worldRatio < viewRatio
      //println("wr:"+worldRatio+" vr:"+viewRatio+" hs:"+_heightSet)
      //println("CalcOffsets scale:"+scale+" worldWidth:"+_world_Width+" worldHeight:"+_world_Height+" viewSize:"+_viewSizeX+"|"+_viewSizeY+" ")
      if (_heightSet) {
        yOffset = vpBorder
        xOffset = ((_viewSizeX - _world_Width * scale) / 2).toInt + vpBorder
        val worldOffset = (_viewSizeX / scale - _world_Width) / 2
        wbx1 = _world_X - worldOffset
        wby1 = _world_Y
        wbx2 = _world_X + _world_Width + worldOffset
        wby2 = _world_Y + _world_Height
      } else {
        xOffset = vpBorder
        yOffset = ((_viewSizeY - _world_Height * scale) / 2).toInt + vpBorder
        val worldOffset = (_viewSizeY / scale - _world_Height) / 2
        wbx1 = _world_X
        wby1 = _world_Y - worldOffset
        wbx2 = _world_X + _world_Width
        wby2 = _world_Y + _world_Height + worldOffset
      }
      _thicknessScale = if (relativeScale._1 < 0 || relativeScale._2 < 0) {
        //System.err.println("RelativeScale <0":+relativeScale+"\n"+Thread.currentThread().getStackTrace().mkString("\n"))
          1d
        }
        else (scale / 100d) / (_relativeScale._1 / _relativeScale._2)
    }
  }

  def world_X: Double = _world_X

  def world_Y: Double = _world_Y

  def viewWidthInWorld: Double = _world_Width

  def viewHeightInWorld: Double = _world_Height

  def scale: Double = {
    if (_heightSet && (_world_Height != 0)) {
      _viewSizeY.toDouble / _world_Height
    } else {
      if (_world_Width == 0) 1
      else _viewSizeX.toDouble / _world_Width
    }
    //System.out.println("scale heightSet:"+_heightSet+" wh:"+_world_Height+" ww:"+_world_Width+ " sc:"+scal)
    //scal
  }

  def worldScale: Double = scale * dotPitch

  def getScaleRatio: (Double, Double) = {
    //if(scale<0) System.err.println("Scale <0:"+scale)
    val ret = scale * dotPitch / 1000d
    if (ret > 1) (ret, 1) else (1, 1 / ret)
  }

  def setScaleRatio(a: Double, b: Double): Unit = {
    val wishScale = (a / b) * 1000d / dotPitch
    if (_viewSizeX == 0 || _viewSizeY == 0) {
      util.Log.e("setScaleRatio viewsize is 0:" + _viewSizeX + " " + _viewSizeY)
    }
    var newWorldHeight: Double = 0
    var newWorldWidth: Double = 0
    if (_heightSet) {
      newWorldHeight = _viewSizeY / wishScale
      newWorldWidth = _viewSizeX * newWorldHeight / _viewSizeY
    } else {
      newWorldWidth = _viewSizeX / wishScale
      newWorldHeight = _viewSizeY * newWorldWidth / _viewSizeX
    }
    val newX = _world_X - (newWorldHeight - _world_Height) / 2
    val newY = _world_Y - (newWorldWidth - _world_Width) / 2
    setWorldBounds(newX, newY, newWorldWidth, newWorldHeight)
  }

  def xToScreen(wx: Double): Double = ((wx - _world_X) * scale) + xOffset

  def yToScreen(wy: Double): Double = ((_world_Y + _world_Height - wy) * scale) + yOffset

  def xToWorld(x: Int): Double = (x - xOffset) / scale + _world_X

  def yToWorld(y: Int): Double = (_world_Y + _world_Height) - ((y - yOffset) / scale)

  //def getScreenPos(px:Double,py:Double):Point.Float= new Point(xToScreen(px),yToScreen(py))

  def registerScaleListener(listener: () => Unit): Unit = {
    scaleListeners += listener
  }

  def notifyScaleChanged(): Unit = {
    for (l <- scaleListeners) l()
  }

  def relativeScale: (Double, Double) = _relativeScale

  def relativeScale_=(newScale: (Double, Double)): Unit = {
    _relativeScale = newScale
    calcOffsets()
    notifyScaleChanged()
  }

  def setRelativeScaleID(scID: Int): Unit = {
    if (!ScaleModel.scales.contains(scID)) util.Log.e("Unknown ScaleID:" + scID + "\n" + ScaleModel.scales.mkString("|"))
    else {
      val sc = ScaleModel.scales(scID)
      relativeScale = if (sc > 1) (sc, 1) else (1, 1 / sc)
    }
  }

  def relativeScaleValue: Double = if (relativeScale._1 == 0) 1d else relativeScale._2 / relativeScale._1

  /** tests if the given Point is inside of the world bounds of the screen
    *
    * @param tp the test point
    * @return true if it is inside of the world bounds
    */
  def isInWorldBounds(tp: VectorConstant): Boolean = tp.x >= wbx1 && tp.x <= wbx2 && tp.y >= wby1 && tp.y <= wby2

  def get2DCameraPos: (Double, Double, Double) = {
    val width = wbx2 - wbx1
    val height = wby2 - wby1
    (wbx1 + width / 2d,
      wby1 + height / 2d,
      height / 2 * 1.05)
  }

}