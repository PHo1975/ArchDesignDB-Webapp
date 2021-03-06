package clientbase.viewer2d

import org.denigma.threejs._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

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

@js.native
@JSGlobal("THREE.FontLoader")
class FontLoader extends js.Object {
  def load(url: String, onLoad: js.Function1[ThreeFont, Unit]):Unit = js.native
}

//@ScalaJSDefined
@js.native
@JSGlobal("THREE.TextGeometryParams")
class TextGeometryParams extends js.Object{
  var size: js.Any = js.native
  var height: js.Any = js.native
  var curveSegments: js.Any = js.native
  var font: js.Any = js.native
  var bevelEnabled: js.Any = js.native
  var bevelThickness: js.Any = js.native
  var bevelSize: js.Any = js.native
}

@js.native
@JSGlobal("THREE.TextGeometry")
class TextGeometry(text: String, textGeometryParameters: TextGeometryParams = js.native) extends Geometry

@js.native
@JSGlobal("THREE.Font")
class ThreeFont extends js.Object {
  var data: js.Any = js.native
  def generateShapes ( text:String, size:Double, divisions:Int ):js.Array[js.Object]= js.native
}

@js.native
@JSGlobal("THREE.ShapeBufferGeometry")
class ShapeBufferGeometry extends BufferGeometry{
  def this(shapes:js.Array[Shape], curveSegments:Int) = this()
  var parameters: js.Any = js.native
}