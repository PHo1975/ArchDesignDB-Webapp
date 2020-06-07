package clientbase.building
import building.{NoCutPlane, PartArea}
import clientbase.viewer2d.Handlers.{CompositionHandler, UndefinedComposition}
import clientbase.viewer2d.SelectionDecorable
import definition.data.{Composition, Referencable, Reference, ShellLayer}
import definition.expression.{Line3D, NULLVECTOR, Plane3D, VectorConstant}
import org.denigma.threejs.{Line, Mesh}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Double
import scala.scalajs.js


class EndInfo(val basePoint:VectorConstant,val fillPos:Array[VectorConstant])

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

class DecoratedLine(module:BuildingModule,start:VectorConstant,end:VectorConstant,val pa:PartArea,val mesh:Seq[Line],colors:Seq[Int],val norm:VectorConstant,val composition:Composition)
  extends SelectionDecorable with Referencable {
  def numLayers: Int =if(composition==null) 0 else composition.shellLayers.size
  lazy val dir: VectorConstant =end-start

  def createDefaultEndpointArray(startPoint:VectorConstant,norm:VectorConstant): Array[VectorConstant] =
    if(composition==null) Array.empty
    else{
      new CloneInnerIterator(composition.shellLayers.reverseIterator.map(_.thickness).
        scanLeft(startPoint)((np: VectorConstant, thick: Double)=>np+norm*thick)).toArray.reverse
    }

  val firstEnd=new EndInfo(start,createDefaultEndpointArray(start+norm*pa.align,norm))
  val secondEnd=new EndInfo(end,createDefaultEndpointArray(end+norm*pa.align,norm))

  def getPoint(firstPoint:Boolean): EndInfo =if (firstPoint) firstEnd else secondEnd
  def getOtherPoint(firstPoint:Boolean): EndInfo =if (firstPoint) secondEnd else firstEnd
  def getAxis(ix:Int): Line3D =Line3D(firstEnd.fillPos(ix),dir)

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
  override def toString: String ="Pa "+ref
  
  def createFillQuads(): Unit = if(composition!=null){
    //val maxShell=composition.shellLayers.size-1
    for(layerIx<-composition.shellLayers.indices;layer=composition.shellLayers(layerIx)) {
      val startPoint1=firstEnd.fillPos(layerIx*2)
      val startPoint2=firstEnd.fillPos(layerIx*2+1)
      val endPoint1=secondEnd.fillPos(layerIx*2)
      val endPoint2=secondEnd.fillPos(layerIx*2+1)
      val points=Seq(startPoint1,startPoint2,endPoint2,endPoint1)
      val geom=BuildingDataModel.createQuadGeometry(points)
      val mesh=new Mesh(geom,BuildingDataModel.getMeshMaterial(layer.color))
      module.canvas2D.scene.add(mesh)
      module.viewModel2D.fillList+=mesh
    }
  }
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

  // maps Position (x,y) to => List of (DecoratedLines, FirstPoint)
  protected val nodeMap: mutable.HashMap[(Double, Double), List[(DecoratedLine,Boolean)]] =mutable.HashMap[(Double,Double),List[(DecoratedLine,Boolean)]]()



  val toZ: VectorConstant = VectorConstant(0,0,-3)

  implicit val ordering: Double.TotalOrdering.type =Ordering.Double.TotalOrdering

  protected val geometryList: mutable.ArrayBuffer[DecoratedLine] =collection.mutable.ArrayBuffer[DecoratedLine]()
  val fillList= mutable.ArrayBuffer[Mesh]()

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
    for(f<-fillList){
      module.canvas2D.scene.remove(f)
      f.geometry.dispose()
    }
    fillList.clear()
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

    def addToNodeMap(x:Double,y:Double,value:DecoratedLine,firstPoint:Boolean): Unit = {
      val key=(Math.floor(x*cropFactor),Math.floor(y*cropFactor))
      val tuple=(value,firstPoint)
      nodeMap(key)=nodeMap.get(key) match {
        case Some(list)=> tuple::list
        case None=>List(tuple)
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
        val intersections: Seq[VectorConstant] =pointList.removeDoublePoints().removeStraightEdges().edges.flatMap(_.getIntersectionWith(l1,l2)).sortBy(_._1).
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
        addToNodeMap(start.x,start.y,dl,true)
        addToNodeMap(end.x,end.y,dl,false)
        geometryList+=dl
        linesFinished+=1
        if(loopHasFinished&&linesFinished==numLines) checkNodes(linesFinished,false)
      }
      checkPoint(start)
      checkPoint(end)
      if(comp.shellLayers.isEmpty) putDL(new DecoratedLine(module,start+toZ,end+toZ,pa,Seq(internDrawLine(pa,start,end,pa.aufbau)),Seq(pa.aufbau),NULLVECTOR,null))
      else {
        val delta=end-start
        val cellCenter=getCellCenter(pa.firstCellID)
        val norm=delta.norm2d*(-1*java.lang.Math.signum(VectorConstant.pointLocation2D(start,end,cellCenter)*(if(pa.flip)-1 else 1)))
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
        putDL(new DecoratedLine(module,start+toZ,end+toZ,pa,lineBuffer.toSeq,colorBuffer.toSeq,norm,comp))
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
    val mesh=BuildingDataModel.createLine(start+toZ, end+toZ, BuildingDataModel.getLineMaterial(0))
    mesh.name=pa.ref.instance.toString
    module.canvas2D.scene.add(mesh)
    mesh
  }


  def checkNodes(numLines:Int,first:Boolean): Unit ={
    println("CheckNodes "+numLines+" first:"+first)
    println(nodeMap.mkString("\n "))
    println("Num node lines:"+nodeMap.valuesIterator.foldLeft(0)((v,list)=>v+list.size))
    for(node<-nodeMap.valuesIterator){
      node.size match {
        case 1=> println("Only one Line at node:"+node.head)
        case 2=>
          val lineA=node.head._1
          val lineAFP=node.head._2
          val lineB=node.last._1
          val lineBFP=node.last._2
          if(!lineA.norm.isNearlyLinearyDependentFrom(lineB.norm)&& lineA.norm!=NULLVECTOR&& lineB.norm!=NULLVECTOR) { // lines are not parallel
            val thisLineP2=lineA.getOtherPoint(lineAFP).basePoint
            val aPoint=lineA.getPoint(lineAFP)
            val bPoint=lineB.getPoint(lineBFP)
            val crossPoint=aPoint.basePoint
            val otherLineP2=lineB.getOtherPoint(lineBFP).basePoint
            val bendLeft=VectorConstant.pointLocation2D(thisLineP2,crossPoint,otherLineP2)>0
            val otherNormLeft=VectorConstant.pointLocation2D(crossPoint,otherLineP2,crossPoint+lineB.norm)>0
            val aReverse= !(bendLeft ^ lineA.pa.flip)
            val lineALayerIterator= if(aReverse) lineA.composition.shellLayers.indices.reverseIterator
                                    else lineA.composition.shellLayers.indices.iterator
            val bReverse= !(bendLeft ^lineB.pa.flip)
            val lineBLayerIterator = if(bReverse) lineB.composition.shellLayers.indices.reverseIterator
                                    else lineB.composition.shellLayers.indices.iterator
            println("Node between "+lineA+" and "+lineB +" bendLeft:"+bendLeft+" otherNormLeft:"+otherNormLeft+" areverse:"+aReverse+" bReverse:"+bReverse)
            val lineALayers=lineA.composition.shellLayers
            val lineBLayers=lineB.composition.shellLayers
            var lineBLayerIx= lineBLayerIterator.next()
            var lineALayerIx= -1
            var lastHandledBIx= -1
            while(lineALayerIterator.hasNext) {
              lineALayerIx=lineALayerIterator.next()
              val firstLineIxA=lineALayerIx*2+(if (aReverse) 1 else 0)
              val secondLineIxA=lineALayerIx*2+(if (aReverse) 0 else 1)
              val currentLineALayer=lineALayers(lineALayerIx)
              println("lineALayerIx "+lineALayerIx+" firstLineIxA:"+firstLineIxA+" secondLineIxA:"+secondLineIxA)

              while (currentLineALayer.priority>lineBLayers(lineBLayerIx).priority&&(lineBLayerIterator.hasNext||lastHandledBIx!=lineBLayerIx)) {
                val firstLineIxB=lineBLayerIx*2+(if (bReverse) 1 else 0)
                val secondLineIxB=lineBLayerIx*2+(if (bReverse) 0 else 1)
                val laAxis=lineA.getAxis(firstLineIxA)
                bPoint.fillPos(firstLineIxB)=laAxis.intersectionWith(lineB.getAxis(firstLineIxB))
                bPoint.fillPos(secondLineIxB)=laAxis.intersectionWith(lineB.getAxis(secondLineIxB))
                lastHandledBIx=lineBLayerIx
                if(lineBLayerIterator.hasNext)
                  lineBLayerIx=lineBLayerIterator.next()
              }
              val lineIxB=lineBLayerIx*2+(if (bReverse ^ (!lineBLayerIterator.hasNext) ) 1 else 0)
              val lbAxis=lineB.getAxis(lineIxB)
              aPoint.fillPos(firstLineIxA)=lbAxis.intersectionWith(lineA.getAxis(firstLineIxA))
              aPoint.fillPos(secondLineIxA)=lbAxis.intersectionWith(lineA.getAxis(secondLineIxA))

              if(currentLineALayer.priority==lineBLayers(lineBLayerIx).priority&& lineBLayerIterator.hasNext) {
                val firstLineIxB=lineBLayerIx*2+(if (bReverse) 1 else 0)
                val secondLineIxB=lineBLayerIx*2+(if (bReverse) 0 else 1)
                val laAxis=lineA.getAxis(secondLineIxA)
                bPoint.fillPos(firstLineIxB)=laAxis.intersectionWith(lineB.getAxis(firstLineIxB))
                bPoint.fillPos(secondLineIxB)=laAxis.intersectionWith(lineB.getAxis(secondLineIxB))
                lastHandledBIx=lineBLayerIx
                lineBLayerIx = lineBLayerIterator.next()
              }
            }
            val secondLineIxA=lineALayerIx*2+(if (aReverse) 0 else 1)
            println("LineA Loop done, lineB IX:"+lineBLayerIx)

            while(lineBLayerIterator.hasNext|| (lineBLayerIx!=lastHandledBIx)){
              val firstLineIxB=lineBLayerIx*2+(if (bReverse) 1 else 0)
              val secondLineIxB=lineBLayerIx*2+(if (bReverse) 0 else 1)
              val laAxis=lineA.getAxis(secondLineIxA)
              bPoint.fillPos(firstLineIxB)=laAxis.intersectionWith(lineB.getAxis(firstLineIxB))
              bPoint.fillPos(secondLineIxB)=laAxis.intersectionWith(lineB.getAxis(secondLineIxB))
              lastHandledBIx=lineBLayerIx
              if(lineBLayerIterator.hasNext)
                lineBLayerIx = lineBLayerIterator.next()
            }

          }
        case 3=>
        case 4=>
        case more=> println("To large number of nodes: "+more+" "+node.mkString("|"))
      }
    }
    for(dl<-geometryList)dl.createFillQuads()
    module.canvas2D.repaint()
  }






}
