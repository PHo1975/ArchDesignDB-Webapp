package clientbase.building
import building.{NoCutPlane, PartArea}
import clientbase.viewer2d.Handlers.{CompositionHandler, UndefinedComposition}
import clientbase.viewer2d.SelectionDecorable
import definition.data.{Composition, Referencable, Reference}
import definition.expression.{NULLVECTOR, Plane3D, VectorConstant}
import org.denigma.threejs.Line

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Double
import scala.scalajs.js


class DecoratedLine(module:BuildingModule,val pa:PartArea,val mesh:Seq[Line],colors:Seq[Int],norm:VectorConstant,composition:Composition) extends SelectionDecorable with Referencable{
  override def showSelection(): Unit = {
    for(m<-mesh)
       m.material=BuildingDataModel.lineSelectMaterial
    module.canvas.repaint()
  }

  override def hideSelection(): Unit = {
    for(i<-mesh.indices;m=mesh(i);color=colors(i))
    m.material=BuildingDataModel.getLineMaterial(color)
    module.canvas.repaint()
  }

  override def ref: Reference = pa.ref
  override def toString="Pa "+ref
}



class Building2DViewModel(module:BuildingModule) extends AbstractViewModel {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  var minX: Double =Float.MaxValue.toDouble
  var minY: Double =Float.MaxValue.toDouble
  var maxX: Double =Float.MinValue.toDouble
  var maxY: Double =Float.MinValue.toDouble

  protected var numPAs: Int =0
  protected val cropFactor=10000000000000d

  protected val cellCenters: mutable.HashMap[Int, VectorConstant] = mutable.HashMap[Int,VectorConstant]()

  protected val nodeMap: mutable.HashMap[(Double, Double), List[DecoratedLine]] =mutable.HashMap[(Double,Double),List[DecoratedLine]]()



  val toZ: VectorConstant = VectorConstant(0,0,-3)

  implicit val ordering: Double.TotalOrdering.type =Ordering.Double.TotalOrdering

  protected val geometryList: mutable.ArrayBuffer[DecoratedLine] =collection.mutable.ArrayBuffer[DecoratedLine]()

  def getLine(ID:Int): Option[DecoratedLine] =geometryList.find(_.ref.instance==ID)

  override def cutPlaneChanged(): Unit = {
    updateData()
  }

  override def updateData(): Unit = {
    clearGeometry()
    createGeometry()
    module.canvas2D.updateResize()
    module.canvas2D.repaint()
  }

  def clearGeometry():Unit= {
    for(g<-geometryList.iterator;m<-g.mesh) {
      module.canvas2D.scene.remove(m)
      m.geometry.dispose()
    }
    geometryList.clear()
    for(c<-module.canvas2D.scene.children;if ! js.isUndefined(c))
      module.canvas2D.scene.remove(c)

    cellCenters.clear()
    nodeMap.clear()
    numPAs=0
    minX =Float.MaxValue.toDouble
    minY =Float.MaxValue.toDouble
    maxX =Float.MinValue.toDouble
    maxY =Float.MinValue.toDouble
  }


  def createGeometry(): Unit = {
    var loopHasFinished=false
    var numLines:Int=0
    var linesFinished=0

    def addToNodeMap(x:Double,y:Double,value:DecoratedLine): Unit = {
      val key=(Math.floor(x*cropFactor),Math.floor(y*cropFactor))
      nodeMap(key)=nodeMap.get(key) match {
        case Some(list)=> value::list
        case None=>List(value)
      }
    }

    clearGeometry()
    if(module.currentCutPlane!=NoCutPlane){
      val defPlane: Plane3D =module.currentCutPlane.plane
      println("defplane:"+defPlane)
      BuildingDataModel.loopPartAreas(NoCutPlane, module.dataModel.partAreaSubscriber.map.valuesIterator.filter(
        ! _.defPlane.plane.isLinearyDependentFrom(defPlane)),(pointList,partArea)=>if(pointList.points.nonEmpty&&partArea.aufbau!= -10){
        val paDefPlane=partArea.defPlane.plane
        val cutLine=defPlane.intersectionWith(paDefPlane)
        val l1= paDefPlane.getAreaCoords(cutLine.pos)
        val l2= paDefPlane.getAreaCoords(cutLine.pos+cutLine.dir)
        val intersections: Seq[VectorConstant] =pointList.edges.flatMap(_.getIntersectionWith(l1,l2)).sortBy(_._1).
          map(el=>defPlane.getAreaCoords(paDefPlane.toWorldVector(el._2)))
        numLines+=intersections.size/2
        if(CompositionHandler.compositionExists(partArea.aufbau))
          for (comp <- CompositionHandler.getComposition(partArea.aufbau)) {
            for (li <- intersections.grouped(2); if li.size == 2)
              createDecoratedLines(li(0), li(1), partArea, comp)
            module.canvas2D.repaint()
          }
        else for (li <- intersections.grouped(2); if li.size == 2)
          createDecoratedLines(li(0), li(1),partArea,UndefinedComposition)
      })
      loopHasFinished=true
      if(linesFinished==numLines) checkNodes(linesFinished,true)
    }

    def createDecoratedLines(start:VectorConstant,end:VectorConstant,pa:PartArea,comp:Composition): Unit = {
      def putDL(dl:DecoratedLine):Unit= {
        addToNodeMap(start.x,start.y,dl)
        addToNodeMap(end.x,end.y,dl)
        geometryList+=dl
        linesFinished+=1
        if(loopHasFinished&&linesFinished==numLines) checkNodes(linesFinished,false)
      }
      checkPoint(start)
      checkPoint(end)
      if(comp.shellLayers.isEmpty) putDL(new DecoratedLine(module,pa,Seq(internDrawLine(pa,start,end,pa.aufbau)),Seq(pa.aufbau),NULLVECTOR,null))
      else {
        val delta=end-start
        val cellCenter=getCellCenter(pa.firstCellID)
        val norm=delta.norm2d*(-1*java.lang.Math.signum(VectorConstant.pointLocation2D(start,end,cellCenter)))
        var deltaVect=norm*pa.align
        var lineBuffer=new ArrayBuffer[Line]()
        var colorBuffer=new ArrayBuffer[Int]()
        lineBuffer+=internDrawLine(pa,start+deltaVect,end+deltaVect,0)
        colorBuffer+=0
        for(layer<-comp.shellLayers.reverseIterator){
          deltaVect=deltaVect+norm*layer.thickness
          lineBuffer+=internDrawLine(pa,start+deltaVect,end+deltaVect,layer.color)
          colorBuffer+=layer.color
        }
        putDL(new DecoratedLine(module,pa,lineBuffer.toSeq,colorBuffer.toSeq,norm,comp))
      }
    }
  }

  def getCellCenter(cellID:Int): VectorConstant =cellCenters.getOrElseUpdate(cellID,
    module.currentCutPlane.plane.getAreaCoords(module.dataModel.cellSubscriber.map(cellID).centerPoint))

  protected def checkPoint(v:VectorConstant): Unit = {
    minX=Math.min(minX,v.x)
    maxX=Math.max(maxX,v.x)
    minY=Math.min(minY,v.y)
    maxY=Math.max(maxY,v.y)
  }

  protected def internDrawLine(pa:PartArea,start:VectorConstant,end:VectorConstant,color:Int):Line= {
    val mesh=BuildingDataModel.createLine(start+toZ, end+toZ, BuildingDataModel.getLineMaterial(color))
    mesh.name=pa.ref.instance.toString
    module.canvas2D.scene.add(mesh)
    mesh
  }


  def checkNodes(numLines:Int,first:Boolean): Unit ={
    println("CheckNodes "+numLines+" first:"+first)
    println(nodeMap.mkString("\n "))
    println("Num node lines:"+nodeMap.valuesIterator.foldLeft(0)((v,list)=>v+list.size))
    println("SingleNodes"+nodeMap.valuesIterator.filter(_.size==1).mkString("|"))
  }






}
