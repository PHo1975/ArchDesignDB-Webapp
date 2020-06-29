package clientbase.building

import building.PartArea
import clientbase.viewer2d.SelectionDecorable
import definition.data.{Composition, Referencable, Reference}
import definition.expression.{Line3D, VectorConstant}
import org.denigma.threejs.{Line, Mesh}

import scala.collection.mutable.ArrayBuffer

class DecoratedLine(module:BuildingModule,start:VectorConstant,end:VectorConstant,val pa:PartArea,/*val mesh:Seq[Line],*/colors:Seq[Int],val norm:VectorConstant,
                    val composition:Composition)
  extends SelectionDecorable with Referencable {
  import Building2dViewConstants.toZ2
  lazy val dir: VectorConstant =end-start
  val firstEnd=new EndInfo(start,createDefaultEndpointArray(start+norm*pa.align,norm))
  val secondEnd=new EndInfo(end,createDefaultEndpointArray(end+norm*pa.align,norm))
  protected var lineMeshes: ArrayBuffer[Line] = ArrayBuffer[Line]()

  def numLayers: Int =if(composition==null) 0 else composition.shellLayers.size

  def createDefaultEndpointArray(startPoint:VectorConstant,norm:VectorConstant): Array[VectorConstant] =
    if(composition==null) Array.empty
    else{
      new CloneInnerIterator(composition.shellLayers.reverseIterator.map(_.thickness).
        scanLeft(startPoint)((np: VectorConstant, thick: Double)=>np+norm*thick)).toArray.reverse
    }


  def getPointInfo(firstPoint:Boolean): EndInfo =if (firstPoint) firstEnd else secondEnd
  def getOtherPoint(firstPoint:Boolean): EndInfo =if (firstPoint) secondEnd else firstEnd
  def getAxis(ix:Int): Line3D =Line3D(firstEnd.fillPos(ix),dir)

  def isExteriorPA: Boolean =pa.secondCellID!=0

  override def showSelection(): Unit = {
    for(m<-lineMeshes)
       m.material=BuildingDataModel.lineSelectMaterial
    module.canvas.repaint()
  }

  override def hideSelection(): Unit = {
    for(i<-lineMeshes.indices; m=lineMeshes(i); color=colors(i))
      m.material=BuildingDataModel.getLineMaterial(0)
    module.canvas.repaint()
  }

  def deleteLineMeshes():Unit= {
    for( m<-lineMeshes) {
      module.canvas2D.scene.remove(m)
      m.geometry.dispose()
    }
    lineMeshes.clear()
  }

  override def ref: Reference = pa.ref
  override def toString: String ="Decorated line for PA "+ref.instance+", start:"+start+", end:"+end

  def createFillQuads(): Unit = if(composition!=null){
    deleteLineMeshes()
    //val maxShell=composition.shellLayers.size-1
    var first=true
    for(layerIx<-composition.shellLayers.indices;layer=composition.shellLayers(layerIx)) {
      val startPoint1=firstEnd.fillPos(layerIx*2)
      val startPoint2=firstEnd.fillPos(layerIx*2+1)
      val endPoint1=secondEnd.fillPos(layerIx*2)
      val endPoint2=secondEnd.fillPos(layerIx*2+1)
      if(first){
        internDrawLine(startPoint1+toZ2,endPoint1+toZ2,0)
        first=false
      }
      internDrawLine(startPoint2+toZ2,endPoint2+toZ2,0)
      val points=Seq(startPoint1,startPoint2,endPoint2,endPoint1)
      val geom=BuildingDataModel.createQuadGeometry(points)
      val mesh=new Mesh(geom,BuildingDataModel.getMeshMaterial(layer.color))
      module.canvas2D.scene.add(mesh)
      module.viewModel2D.fillList+=mesh
    }
  } else internDrawLine(start,end,0)

  protected def internDrawLine(start:VectorConstant,end:VectorConstant,color:Int):Unit= {
    val mesh=BuildingDataModel.createLine(start, end, BuildingDataModel.getLineMaterial(0))
    mesh.name=pa.ref.instance.toString
    module.canvas2D.scene.add(mesh)
    lineMeshes+=mesh
  }
}



// Iterator that returns the inner elements of the given Iterator twice
class CloneInnerIterator[B ](mainIterator:Iterator[B]) extends Iterator[B] {
  private var first=true
  private var mbuffer:B= _
  private var useBuffer=false
  override def hasNext: Boolean = {
    mainIterator.hasNext
  }
  override def next(): B = {
    if (first) {
      first=false
      mainIterator.next
    }  else {
      if(useBuffer){
        useBuffer=false
        mbuffer
      } else {
        mbuffer=mainIterator.next()
        useBuffer=true
        mbuffer
      }
    }
  }
}


