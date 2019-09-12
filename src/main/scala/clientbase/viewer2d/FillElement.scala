package clientbase.viewer2d

import definition.data.{Named, Reference}
import definition.expression.{Polygon, VectorConstant}
import org.denigma.threejs._

import scala.scalajs.js

class FillElement(nref:Reference,ncolor:Int,nlineWidth:Int,nlineStyle:Int,val fillColor:Int,val hatchStyle:Int,val paperScale:Boolean,
                  val poly:Polygon,val  startPoint:VectorConstant,val hatchAngle:Double, var name:String="")
  extends LinearElement(nref,ncolor,nlineWidth,nlineStyle) with Named {

  lazy val bounds= BRect(poly.minX, poly.minY, poly.maxX, poly.maxY)

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit =
    GraphElem.calcScreenBoundsfromPointList(poly.iterateAllPoints,camera,res)


  override def getBounds(container: ElemContainer): Bounds = bounds

  override def createGeometry(container: ElemContainer): Unit = {
    try {
      val holesArray= js.Array( (for (pl<-poly.pathList;if ! pl.isClockWise) yield {
        val path=new Path()
        val firstPoint=pl.points.head
        path.moveTo(firstPoint.x,firstPoint.y)
        for(i<-1 until pl.points.length;np=pl.points(i))
          path.lineTo(np.x,np.y)
        path.lineTo(firstPoint.x,firstPoint.y)
        path
      } ) :_*)
      val shape=new Shape()
      for(pl<-poly.pathList;if  pl.isClockWise){
        val firstPoint=pl.points.head
        shape.moveTo(firstPoint.x,firstPoint.y)
        for(i <-1 until pl.points.length;np=pl.points(i))
          shape.lineTo(np.x,np.y)
        shape.lineTo(firstPoint.x,firstPoint.y)
      }
      shape.holes=holesArray

      val geometry=new ShapeBufferGeometry(js.Array(shape),12)
      geometry.computeFaceNormals()
      geometry.computeBoundingSphere()
      val mesh = new Mesh(geometry, GraphElem.getMaterial(ncolor))
      mesh.name = "F" + ref.instance
      mesh.position.z= -.001f
      _geometry.push(mesh)
      container.addGeometry(mesh)

  } catch {
    case e: Throwable => util.Log.e("Fill Geo:", e)
  }
  }
}
