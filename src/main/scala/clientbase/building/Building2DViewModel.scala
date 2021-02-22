package clientbase.building
import building.{NoCutPlane, PartArea}
import clientbase.viewer2d.Handlers.{CompositionHandler, UndefinedComposition}
import definition.data.{Composition, Reference, ShellLayer}
import definition.expression.{NULLVECTOR, Plane3D, VectorConstant}
import org.denigma.threejs.Mesh

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Double
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}


class EndInfo(val basePoint:VectorConstant,val fillPos:Array[VectorConstant])


/*@ScalaJSDefined*/ class Building2dViewConstants extends js.Object

@JSExportTopLevel("Building2dViewConstants") object Building2dViewConstants  {
  @JSExportStatic var firstAufbau=8
  @JSExportStatic var firstAlign=0d
  @JSExportStatic var firstFlip=false
  @JSExportStatic def setFirstFlip(v:Boolean)=firstFlip=v
  @JSExportStatic var secondAufbau=9
  @JSExportStatic var secondAlign=0d
  @JSExportStatic var secondFlip=true
  @JSExportStatic var p1=VectorConstant(0,0,0)
  @JSExportStatic var p2=VectorConstant(0,5,0)
  @JSExportStatic var p3=VectorConstant(-4,6,0)
  @JSExportStatic def setP3(x:Double,y:Double,z:Double)=p3=VectorConstant(x,y,z)
  @JSExportStatic def setsecondFlip(flip:Boolean)=secondFlip=flip
  @JSExportStatic var p4=VectorConstant(4,8,0)
  @JSExportStatic def setP4(x:Double,y:Double,z:Double)=p4=VectorConstant(x,y,z)
  @JSExportStatic var cellCenter=VectorConstant(1,2,0)
  val toZ: VectorConstant = VectorConstant(0,0,-2)
  val toZ2: VectorConstant = VectorConstant(0,0,1)
}



class Building2DViewModel(module:BuildingModule) extends AbstractViewModel  {
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


  implicit val ordering: Double.TotalOrdering.type =Ordering.Double.TotalOrdering

  protected val decoratedLinesList: mutable.ArrayBuffer[DecoratedLine] =collection.mutable.ArrayBuffer[DecoratedLine]()
  val fillList= mutable.ArrayBuffer[Mesh]()

  def getLine(ID:Int): Option[DecoratedLine] =decoratedLinesList.find(_.ref.instance==ID)

  override def cutPlaneChanged(): Unit = updateData()

  override def updateData(): Unit = {
    clearGeometry()
    createGeometry()
    module.canvas2D.updateResize()
    module.canvas2D.repaint()
  }

  def clearGeometry():Unit= {
    for(dl<-decoratedLinesList.iterator)
      dl.deleteLineMeshes()
    decoratedLinesList.clear()
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


  def createGeometry(test:Boolean=false): Unit = {
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
    if(test) {
      import Building2dViewConstants._
      val pa1=new PartArea(module.dataModel,Reference(0,1),0,1,0,firstAufbau,firstAlign,firstFlip)
      val pa2=new PartArea(module.dataModel,Reference(0,2),0,1,0,secondAufbau,secondAlign,secondFlip)
      val pa3=new PartArea(module.dataModel,Reference(0,3),0,1,0,0,secondAlign,secondFlip)
      //val pa4=new PartArea(module.dataModel,Reference(0,4),0,1,0,firstAufbau,firstAlign,firstFlip)
      cellCenters(1)=cellCenter
      for(comp1<-CompositionHandler.getComposition(firstAufbau);
          comp2<-CompositionHandler.getComposition(secondAufbau)){
        createDecoratedLine(p1,p2,pa1,comp1)
        createDecoratedLine(p2,p3,pa2,comp2)
        createDecoratedLine(p1,p3,pa3,UndefinedComposition)
        //createDecoratedLine(p2,p4,pa4,comp1)
        checkNodes(first = true)

      }
    }
    else if(module.currentCutPlane!=NoCutPlane){
      val defPlane: Plane3D =module.currentCutPlane.plane
      BuildingDataModel.loopPartAreas(NoCutPlane, module.dataModel.partAreaSubscriber.map.valuesIterator.filter(
        ! _.defPlane.plane.isLinearyDependentFrom(defPlane)),(pointList,partArea)=>if(pointList.points.nonEmpty&&partArea.aufbau!= -10){
        val paDefPlane=partArea.defPlane.plane
        val cutLine=defPlane.intersectionWith(paDefPlane)
        val l1= paDefPlane.getAreaCoords(cutLine.pos)
        val l2= paDefPlane.getAreaCoords(cutLine.pos+cutLine.dir)
        val intersections: Seq[VectorConstant] =pointList.removeDoublePoints().removeStraightEdges().edges.
          flatMap(_.getIntersectionWith(l1,l2)).sortBy(_._1).
          map(el=>defPlane.getAreaCoords(paDefPlane.toWorldVector(el._2)))
        numLines+=intersections.size/2
        if(CompositionHandler.compositionExists(partArea.aufbau))
          for (comp <- CompositionHandler.getComposition(partArea.aufbau)) {
            for (li <- intersections.grouped(2); if li.size == 2)
              createDecoratedLine(li(0), li(1), partArea, comp)
          }
        else for (li <- intersections.grouped(2); if li.size == 2)
          createDecoratedLine(li(0), li(1),partArea,UndefinedComposition)
      })
      loopHasFinished=true
      if(linesFinished==numLines) checkNodes(first = true)
    }

    def createDecoratedLine(start:VectorConstant,end:VectorConstant,pa:PartArea,comp:Composition): Unit = {
      import Building2dViewConstants.toZ
      checkMaxExtension(start)
      checkMaxExtension(end)
      if(comp.shellLayers.isEmpty) putDL(new DecoratedLine(module,start+toZ,end+toZ,pa,Seq(pa.aufbau),NULLVECTOR,null))
      else {
        val delta=end-start
        val cellCenter=getCellCenter(pa.firstCellID)
        val norm=delta.norm2d*(-1*java.lang.Math.signum(VectorConstant.pointLocation2D(start,end,cellCenter)*(if(pa.flip)-1 else 1)))
        var deltaVect=norm*pa.align
        var colorBuffer=new ArrayBuffer[Int]()
        colorBuffer+=0
        for(layer<-comp.shellLayers.reverseIterator){
          deltaVect=deltaVect+norm*layer.thickness
          colorBuffer+=layer.color
        }
        putDL(new DecoratedLine(module,start+toZ,end+toZ,pa,colorBuffer.toSeq,norm,comp))
      }

      def putDL(dl:DecoratedLine):Unit= {
        addToNodeMap(start.x,start.y,dl,firstPoint = true)
        addToNodeMap(end.x,end.y,dl,firstPoint = false)
        decoratedLinesList+=dl
        linesFinished+=1
        if(loopHasFinished&&linesFinished==numLines) checkNodes(first = false)
      }
    }
  }


  def getCellCenter(cellID:Int): VectorConstant =cellCenters.getOrElseUpdate(cellID,
    module.currentCutPlane.plane.getAreaCoords(module.dataModel.cellSubscriber.map(cellID).centerPoint))

  protected def checkMaxExtension(v:VectorConstant): Unit = {
    minX=Math.min(minX,v.x)
    maxX=Math.max(maxX,v.x)
    minY=Math.min(minY,v.y)
    maxY=Math.max(maxY,v.y)
  }


  def checkNodes(first:Boolean): Unit ={
    println("CheckNodes  first:"+first)
    println(" "+nodeMap.mkString("\n "))
    for(node: scala.List[(DecoratedLine, Boolean)] <-nodeMap.valuesIterator){
      node.size match {
        case 1=> println("Only one Line at node:"+node.head)
        case 2=>
          val lineA: DecoratedLine =node.head._1
          val lineAisFirstPoint=node.head._2
          val lineB: DecoratedLine =node.last._1
          val lineBisFirstPoint=node.last._2
          if(!lineA.norm.isNearlyLinearyDependentFrom(lineB.norm)&& lineA.norm!=NULLVECTOR&& lineB.norm!=NULLVECTOR) { // lines are not parallel
            val thisLineP2=lineA.getOtherPoint(lineAisFirstPoint).basePoint
            val aPointInfo: EndInfo =lineA.getPointInfo(lineAisFirstPoint)
            val bPointInfo: EndInfo =lineB.getPointInfo(lineBisFirstPoint)
            val crossPoint=aPointInfo.basePoint
            val otherLineP2=lineB.getOtherPoint(lineBisFirstPoint).basePoint
            println("thisP2:"+thisLineP2+" cross:"+crossPoint+" otherP2:"+otherLineP2)
            val bendLeft=VectorConstant.pointLocation2D(thisLineP2,crossPoint,otherLineP2)>0
            val thisNormLeft=VectorConstant.pointLocation2D(thisLineP2,crossPoint,crossPoint+lineA.norm)>0
            val otherNormLeft=VectorConstant.pointLocation2D(crossPoint,otherLineP2,crossPoint+lineB.norm)>0
            val aReverse=true// !((bendLeft ^ lineA.pa.flip)^(!thisNormLeft))
            val lineALayers=lineA.composition.shellLayers
            println("LineA Shell layers "+lineALayers.mkString("| "))
            val lineBLayers: Seq[ShellLayer] =lineB.composition.shellLayers
            println("LineB Shell layers "+lineBLayers.mkString("| "))
            val lineALayerIterator= if(aReverse) lineALayers.indices.reverseIterator
                                    else lineALayers.indices.iterator
            val bReverse=true// !((bendLeft ^lineB.pa.flip)^(! otherNormLeft))
            val lineBLayerIterator = if(bReverse) lineBLayers.indices.reverseIterator
                                    else lineBLayers.indices.iterator
            println("Node between "+lineA+" and "+lineB +" > bendLeft:"+bendLeft+" thisNormLeft:"+thisNormLeft+" otherNormLeft:"+otherNormLeft+" areverse:"+aReverse+" bReverse:"+bReverse)
            var lineBLayerIx= lineBLayerIterator.next()
            var lineALayerIx= -1
            var lastHandledBIx= -1
            while(lineALayerIterator.hasNext) {
              lineALayerIx=lineALayerIterator.next()
              val firstLineIxA=lineALayerIx*2+(if (aReverse) 1 else 0)
              val secondLineIxA=lineALayerIx*2+(if (aReverse) 0 else 1)
              val currentLineALayer=lineALayers(lineALayerIx)
              println("lineALayerIx "+lineALayerIx+" firstLineIxA:"+firstLineIxA+" secondLineIxA:"+secondLineIxA+" lineBLayerIX:"+lineBLayerIx+
                " APrio:"+currentLineALayer.priority+ " BPrio:"+lineBLayers(lineBLayerIx).priority+" lastBX:"+lastHandledBIx)

              while (currentLineALayer.priority>lineBLayers(lineBLayerIx).priority&&(lineBLayerIterator.hasNext||lastHandledBIx!=lineBLayerIx)) {
                val firstLineIxB=lineBLayerIx*2+(if (bReverse) 1 else 0)
                val secondLineIxB=lineBLayerIx*2+(if (bReverse) 0 else 1)
                val laAxis=lineA.getAxis(firstLineIxA)
                bPointInfo.fillPos(firstLineIxB)=laAxis.intersectionWith(lineB.getAxis(firstLineIxB))
                bPointInfo.fillPos(secondLineIxB)=laAxis.intersectionWith(lineB.getAxis(secondLineIxB))
                lastHandledBIx=lineBLayerIx
                if(lineBLayerIterator.hasNext) {
                  lineBLayerIx=lineBLayerIterator.next()
                  println("step Line B to:"+lineBLayerIx+" BPrio:"+lineBLayers(lineBLayerIx).priority)
                }
              }
              val lineIxB=lineBLayerIx*2+(if (bReverse ^ (lineBLayers(lineBLayerIx).priority<currentLineALayer.priority) ) 1 else 0)
              val lbAxis=lineB.getAxis(lineIxB)
              println("setLine A to BIX:"+lineIxB+" b hasnext:"+lineBLayerIterator.hasNext)
              aPointInfo.fillPos(firstLineIxA)=lbAxis.intersectionWith(lineA.getAxis(firstLineIxA))
              aPointInfo.fillPos(secondLineIxA)=lbAxis.intersectionWith(lineA.getAxis(secondLineIxA))

              if(currentLineALayer.priority==lineBLayers(lineBLayerIx).priority&& lineBLayerIterator.hasNext) {
                val firstLineIxB=lineBLayerIx*2+(if (bReverse) 1 else 0)
                val secondLineIxB=lineBLayerIx*2+(if (bReverse) 0 else 1)
                val laAxis=lineA.getAxis(secondLineIxA)
                bPointInfo.fillPos(firstLineIxB)=laAxis.intersectionWith(lineB.getAxis(firstLineIxB))
                bPointInfo.fillPos(secondLineIxB)=laAxis.intersectionWith(lineB.getAxis(secondLineIxB))
                lastHandledBIx=lineBLayerIx
                lineBLayerIx = lineBLayerIterator.next()
              }
            }

            println("LineA Loop done, lineBLayerIX:"+lineBLayerIx+" lastHandledBIx:"+lastHandledBIx+" lineB hasNext:"+lineBLayerIterator.hasNext)
            println("lineAlayerIx:"+lineALayerIx)
            while(lineBLayerIterator.hasNext|| (lineBLayerIx!=lastHandledBIx)){
              val firstLineIxB=lineBLayerIx*2+(if (bReverse) 1 else 0)
              val secondLineIxB=lineBLayerIx*2+(if (bReverse) 0 else 1)
              println("Priority b:"+lineBLayers(lineBLayerIx).priority+" a:"+lineALayers(lineALayerIx).priority)
              val lineAX=if(lineBLayers(lineBLayerIx).priority>=lineALayers(lineALayerIx).priority) lineALayerIx*2+(if (aReverse) 0 else 1)
              else lineBLayerIx*2+(if (bReverse) 1 else 0)
              println("FirstLineIXB:"+firstLineIxB+" secondLineIXB:"+secondLineIxB+" lineAx:"+lineAX)
              val laAxis=lineA.getAxis(lineAX)
              bPointInfo.fillPos(firstLineIxB)=laAxis.intersectionWith(lineB.getAxis(firstLineIxB))
              bPointInfo.fillPos(secondLineIxB)=laAxis.intersectionWith(lineB.getAxis(secondLineIxB))
              lastHandledBIx=lineBLayerIx
              if(lineBLayerIterator.hasNext)
                lineBLayerIx = lineBLayerIterator.next()
              println("LineBLayerIX:"+lineBLayerIx)
            }

          }

        case 3=>
          val firstLine=node.head._1
          val secondLine=node(1)._1
          val thirdLine=node.last._1
          if(firstLine.dir.isNearlyLinearyDependentFrom(secondLine.dir))check3NodesStraight(node.head,node(1),node.last) else
          if(firstLine.dir.isNearlyLinearyDependentFrom(thirdLine.dir))check3NodesStraight(node.head,node.last,node(1)) else
          if(thirdLine.dir.isNearlyLinearyDependentFrom(secondLine.dir))check3NodesStraight(node.last,node(1),node.head)
          else {
            println("3 Lines ")
          }
        case 4=>
        case more=> println("To large number of nodes: "+more+" "+node.mkString("|"))
      }
    }
    for(dl<-decoratedLinesList)dl.createFillQuads()
    module.canvas2D.updateResize()
    module.canvas2D.repaint()
  }

  def check3NodesStraight(sline1:(DecoratedLine,Boolean),sline2:(DecoratedLine,Boolean),otherLine:(DecoratedLine,Boolean)): Unit = {
    println("3 Nodes Straight s1"+sline1+" s2:"+sline2+" other:"+otherLine)
  }

}
