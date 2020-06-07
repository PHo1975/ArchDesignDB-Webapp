package clientbase.building

import building.{NoCutPlane, PartArea}
import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.Handlers.CompositionHandler
import clientbase.viewer2d.{ElemContainer, GraphElem, MyBufferGeometry, ShapeBufferGeometry}
import definition.data.{OwnerReference, Reference}
import definition.expression.{IntConstant, Plane3D, PointList, VectorConstant}
import org.denigma.threejs._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Double.TotalOrdering
import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array
import org.scalajs.dom.window


class Building3DViewModel(module:BuildingModule) extends ElemContainer with AbstractViewModel {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  import BuildingDataModel._

  val dm: BuildingDataModel = module.dataModel


  var minX: Double = Float.MaxValue.toDouble
  var minY: Double = Float.MaxValue.toDouble
  var minZ: Double = Float.MaxValue.toDouble
  var maxX: Double = Float.MinValue.toDouble
  var maxY: Double = Float.MinValue.toDouble
  var maxZ: Double = Float.MinValue.toDouble

  protected val geomList: ArrayBuffer[Mesh] = ArrayBuffer[Mesh]()
  val edgeList=ArrayBuffer[Edge]()

  val decoratedPartAreas: mutable.Map[Int, DecoratedPartArea] = collection.mutable.HashMap[Int, DecoratedPartArea]()
  val decoratedPlanes: mutable.HashMap[Int, DecoratedPlane] = collection.mutable.HashMap[Int, DecoratedPlane]()

  def updateData(): Unit = {
    clearGeometry()
    createGeometry()
    createDecoratedPlanes()
  }

  def createPartArea(owner: Array[OwnerReference], plane: Int, firstCell: Int, next: () => Unit): Unit = {
    println("createPart plane " + plane + " firstCell:" + firstCell)
    for (IntConstant(pi) <- WebSocketConnector.createInstance(304, owner)) {
      val ref = Reference(304, pi)
      WebSocketConnector.writeInstanceField(ref, 0, IntConstant(plane))
      WebSocketConnector.writeInstanceField(ref, 1, IntConstant(firstCell))
      next()
    }
  }

  def createDecoratedPlanes(): Unit = {
    val nplaneX = new Plane3D(VectorConstant(maxX + planeDistance, 0, 0), VectorConstant(1, 0, 0))
    val nplaneY = new Plane3D(VectorConstant(0, maxY + planeDistance, 0), VectorConstant(0, 1, 0))
    val nplaneZ = new Plane3D(VectorConstant(0, 0, minZ - planeDistance), VectorConstant(0, 0, 1))
    val npx1 = VectorConstant(maxX + planeDistance, minY, minZ)
    val nplaneXPoints = Seq(npx1, VectorConstant(maxX + planeDistance, maxY, minZ), VectorConstant(maxX + planeDistance, maxY, maxZ),
      VectorConstant(maxX + planeDistance, minY, maxZ), npx1)
    val npy1 = VectorConstant(minX, maxY + planeDistance, minZ)
    val nplaneYPoints = Seq(npy1, VectorConstant(maxX, maxY + planeDistance, minZ), VectorConstant(maxX, maxY + planeDistance, maxZ),
      VectorConstant(minX, maxY + planeDistance, maxZ), npy1)
    val npz1 = VectorConstant(minX, minY, minZ - planeDistance)
    val nplaneZPoints = Seq(npz1, VectorConstant(maxX, minY, minZ - planeDistance), VectorConstant(maxX, maxY, minZ - planeDistance),
      VectorConstant(minX, maxY, minZ - planeDistance), npz1)
    val planeUtils: PlaneCalcUtil = new PlaneCalcUtil {
      val planeX: Plane3D = nplaneX;
      val planeY: Plane3D = nplaneY;
      val planeZ: Plane3D = nplaneZ
      val planeXPoints: Seq[VectorConstant] = nplaneXPoints;
      val planeYPoints: Seq[VectorConstant] = nplaneYPoints
      val planeZPoints: Seq[VectorConstant] = nplaneZPoints
    }
    clearDecoratedPlanes()
    for (p <- dm.planeSubscriber.map.valuesIterator) {
      val np = new DecoratedPlane(module, p)
      np.createGeometry(planeUtils)
      for (m <- np.meshList)
        module.canvas3D.scene.add(m)
      decoratedPlanes(p.ref.instance) = np
    }
  }


  def clearDecoratedPlanes(): Unit = {
    for (p <- decoratedPlanes.valuesIterator; m <- p.meshList) {
      module.canvas3D.scene.remove(m)
      m.geometry.dispose()
    }
    decoratedPlanes.clear()
  }

  def clearCellGeometry(): Unit = {
    minX = Float.MaxValue.toDouble
    minY = Float.MaxValue.toDouble
    minZ = Float.MaxValue.toDouble
    maxX = Float.MinValue.toDouble
    maxY = Float.MinValue.toDouble
    maxZ = Float.MinValue.toDouble
    println("clear Geometry " + geomList.size)
    for (geom <- geomList) {
      module.canvas3D.scene.remove(geom)
      geom.geometry.dispose()
    }
    geomList.clear()
    materialCache.clear()
    decoratedPartAreas.clear()
  }

  def clearGeometry(): Unit = {
    clearDecoratedPlanes()
    clearCellGeometry()
  }

  def cutPlaneChanged(): Unit = {
    clearCellGeometry()
    createGeometry()
    createDecoratedPlanes()
  }

  def createQuadGeometry(points: Seq[VectorConstant], el: PartArea): Unit =
    if (points.size == 4) addPartAreaMesh(BuildingDataModel.createQuadGeometry(points), el)
     else println("Wrong number of points " + points.size)


  def addPartAreaMesh(geom: Geometry, el: PartArea): Unit = {
    if(CompositionHandler.compositionExists(el.aufbau))
    for (comp <- CompositionHandler.getComposition(el.aufbau)) {
      val color=comp.shellLayers.headOption match {
        case Some(headLayer) => headLayer.color
        case None => BuildingDataModel.greyColor
      }
      internAddPartAreaMesh(geom,el,color)
      module.canvas3D.repaint()
    } else internAddPartAreaMesh(geom,el,el.aufbau)
  }

  private def internAddPartAreaMesh(geom: Geometry, el: PartArea, color: Int): Unit = {
    val mesh = new Mesh(geom, getMeshMaterial(color))
    mesh.name = el.ref.instance.toString
    decoratedPartAreas(el.ref.instance) = new DecoratedPartArea(module, el, mesh)
    geomList += mesh
    module.canvas3D.scene.add(mesh)
}


  protected def createQuatMatrix(defPlane:Plane3D): Matrix4 = {
    val u0=new Vector3(defPlane.areaAxisX.x,defPlane.areaAxisX.y,defPlane.areaAxisX.z)
    val v0=new Vector3(defPlane.areaAxisY.x,defPlane.areaAxisY.y,defPlane.areaAxisY.z)
    val q2=new Quaternion().setFromUnitVectors(u0,u2)
    val v1=v2.clone().applyQuaternion(q2.clone().conjugate())
    val v0_proj=v0.projectOnPlane(u0)
    val v1_proj=v1.projectOnPlane(u0)
    var angleInPlane: Double =v0_proj.angleTo(v1_proj)
    if (v1_proj.dot(new Vector3().crossVectors(u0, v0)) < 0d) angleInPlane *= -1d
    val q1=new Quaternion().setFromAxisAngle(u0,angleInPlane)
    val q=new Quaternion().multiplyQuaternions(q2,q1)
    val m=new Matrix4()
    m.makeRotationFromQuaternion(q)
    m.getInverse(m)
  }


  def createGeometry(): Unit = {
    val now=window.performance.now()
    println("Create Geometry "+module.currentCutPlane.name)
    val planeQuats=collection.mutable.HashMap[Plane3D,Matrix4]()

    def buildUpGeometryPlane(pL:PointList, el:PartArea): Unit = {
      val pointList=pL.removeDoublePoints().removeStraightEdges()
      val defPlane=el.defPlane.plane
      for(point<-pointList.points; wp=defPlane.toWorldVector(point)){ // find minimal points
        if(wp.x<minX) minX=wp.x
        if(wp.y<minY) minY=wp.y
        if(wp.z<minZ) minZ=wp.z
        if(wp.x>maxX) maxX=wp.x
        if(wp.y>maxY) maxY=wp.y
        if(wp.z>maxZ) maxZ=wp.z
      }
      //println("BG el:"+el.ref+" points:"+r.points.map(p=>p.toString+"=>"+defPlane.toWorldVector(p)).mkString(" | "))
      if(pointList.numVecs==4)
        createQuadGeometry(pointList.points.map(defPlane.toWorldVector),el)
      else if(pointList.numVecs>2){
        val shape = new Shape()
        val tpoints = pointList.points
        shape.moveTo(tpoints.head.x, tpoints.head.y)
        for (i <- 1 until tpoints.size)
          shape.lineTo(tpoints(i).x, tpoints(i).y)
        shape.lineTo(tpoints.head.x, tpoints.head.y)
        val array = js.Array[Shape]()
        array.push(shape)
        val geometry = new ShapeBufferGeometry(array, 12)
        val m2: Matrix4 = planeQuats.getOrElseUpdate(defPlane, createQuatMatrix(defPlane))
        m2.setPosition(new Vector3(defPlane.pos.x, defPlane.pos.y, defPlane.pos.z))
        geometry.applyMatrix(m2)

        addPartAreaMesh(geometry, el)
      }
    }
    // connection planes
    BuildingDataModel.loopPartAreas(module.currentCutPlane,dm.partAreaSubscriber.map.valuesIterator,buildUpGeometryPlane)
    val now2=window.performance.now
    println("Create Geometry "+(now2-now))
    createEdgeList()
    val now3=window.performance.now
    println("Create Edgelist "+(now3-now2))
    showRooms()
    module.canvas3D.repaint()
    println("GesamtDauer: "+(window.performance.now-now))
  }

  def createEdgeList():Unit= {
    edgeList.clear()
    BuildingDataModel.loopPartAreas(NoCutPlane,dm.partAreaSubscriber.map.valuesIterator,(pointList,partArea)=>if(pointList.points.size>3){
      val mappedPointList=pointList.removeDoublePoints().removeStraightEdges().points.map(partArea.defPlane.plane.toWorldVector)
      for(points<-(mappedPointList:+mappedPointList.head).sliding(2)){
        val (start,end)= if(points(0)<points(1)) (points(0),points(1)) else (points(1),points(0))
        //println("PA "+partArea.ref.instance+" Start:"+start+" end:"+end)
        if(!VectorConstant.similar(start,end)) {
          //def checkRayEdges(start:VectorConstant,end:VectorConstant,level:Int):Unit = if(level<3){
            var rayEdges=edgeList.filter(_.sameRay(start,end))
            //println("RayEdges:"+rayEdges.mkString(" | "))
            if(rayEdges.isEmpty ){
              val newEdge=Edge(start,end)
              newEdge.partAreas+=partArea.ref.instance
              edgeList+=newEdge
            }
            else rayEdges.find(_.isSimilar(start,end)) match {
              case Some(edge)=>edge.partAreas+=partArea.ref.instance
              case None =>
                val restSegments=ArrayBuffer[Edge]()
                val newEdge=Edge(start,end)
                newEdge.partAreas+=partArea.ref.instance
                restSegments+=newEdge
                for(existentRayEdge<-rayEdges){
                  var ix=0
                  while(ix<restSegments.size) {
                    val restSegment=restSegments(ix)
                    if(restSegment.isSimilar(existentRayEdge.start,existentRayEdge.end))existentRayEdge.partAreas+=partArea.ref.instance
                    else if(restSegment.intersects(existentRayEdge)){
                      restSegments-=restSegment
                      restSegments++=restSegment.intersectionRest(existentRayEdge)
                      val existentRest=existentRayEdge.intersectionRest(restSegment)
                      //println("intesects with "+existentRayEdge+" rest:"+restSegment+" existentRest:"+existentRest.mkString("|")+" restSegments:"+restSegments.mkString("|"))
                      if(existentRest.isEmpty) existentRayEdge.partAreas+=partArea.ref.instance
                      else {
                        edgeList-=existentRayEdge
                        val intersection=existentRayEdge.getIntersection(restSegment)
                        intersection.partAreas+=partArea.ref.instance
                        edgeList+=intersection
                        edgeList++=existentRayEdge.intersectionRest(restSegment)
                      }
                    }
                    ix+=1
                  }
                }
                for (r<-restSegments) {
                  rayEdges = edgeList.filter(_.sameRay(start, end))
                  rayEdges.find(_.isSimilar(r.start, r.end)) match {
                    case Some(edge) => edge.partAreas += partArea.ref.instance
                    case None => edgeList += r
                  }
                }
            }
          //} else println("!! level >3  pa:"+partArea.ref.instance+" start:"+start+" end:"+end+" rayEdges:")
          //checkRayEdges(astart,aend,1)
        }
      }
    })
    println("Num Edges:"+edgeList.size)
    val singles=edgeList.filter(_.partAreas.size==1)
    println("\nSingle edges:"+singles.size+"\n"+singles.map(s=> s.toString+" index:"+edgeList.indexOf(s)+
      "\nIntersects:" +edgeList.filter(p=>s.intersects(p)).mkString(" |  ")+"  similar:"+edgeList.filter(p=>s.isSimilar(p.start,p.end)).mkString(" | ")+"\n \n").mkString(""))
  }


  def showRooms():Unit= if(module.currentCutPlane!=NoCutPlane) {
    GraphElem.loadFont("Arial",(font,repaint)=>{
      println("Load repaint "+repaint)
      if(repaint) module.canvas3D.repaint()
      else {
        for(room<-dm.roomSubscriber.map.valuesIterator;ri=room.ref.instance){
          def printText(text:String,x:Double,y:Double,z:Double,height:Double): Unit ={
            val geometries: js.Array[ShapeGeometry] =GraphElem.createTextGeometry(this,text,height,font)
            geometries.head.computeBoundingBox()
            geometries.last.computeBoundingBox()
            val textWidth=geometries.last.boundingBox.max.x-geometries.head.boundingBox.min.x
            for(g<-geometries){
              val nm=new Mesh(g,textMaterial)
              nm.name="R"+room.ref.instance
              nm.position.x=x-textWidth/2
              nm.position.y=y
              nm.position.z=z
              addGeometry(nm)
            }
          }

          val cells =dm.cellSubscriber.map.valuesIterator.filter(_.roomID==ri).toSeq
          if(cells.nonEmpty) {
            val areaValue = cells.foldLeft(0d)(_ + _.floorAreaValue)
            val biggestCell = cells.maxBy(_.floorAreaValue)(TotalOrdering)
            val center: VectorConstant = biggestCell.floorCenter
            //println("Raum " + room.name + " " + "%.2f".format(areaValue) + " m2 " + biggestCell.ref.instance + " " + center)
            printText(room.name, center.x, center.y, biggestCell.bottomPlane.plane.pos.z + 0.01d, 3.0)
            printText("%.2f".format(areaValue) + " m2 ", center.x, center.y - 0.3, biggestCell.bottomPlane.plane.pos.z + 0.01d, 2.5)
          }
        }
        module.canvas3D.repaint()
      }
    })
  }

  // ElemContainer Interface
  override def scaleRatio: Double = 100d
  override def addGeometry(geom: Mesh): Unit = {
    geomList+=geom
    module.canvas3D.scene.add(geom)
  }
  override def dataUpdated(): Unit = module.canvas3D.repaint()
}
