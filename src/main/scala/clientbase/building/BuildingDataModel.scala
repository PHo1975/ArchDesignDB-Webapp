package clientbase.building

import building.{AbstractBuildingModel, Cell, PartArea, Plane, Room,CutPlane,NoCutPlane}
import util.clipping.{Area, Path2D, PathIterator}
import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.{MyBufferGeometry, SelectionDecorable, ShapeBufferGeometry}
import definition.data.{OwnerReference, Reference}
import definition.expression.{IntConstant, IntList, Plane3D, PointList, Polygon, VectorConstant}
import org.denigma.threejs.{Color, EdgesGeometry, Float32BufferAttribute, LineBasicMaterial, LineSegments, Matrix4, Mesh, MeshBasicMaterial, Object3D, Quaternion, Shape, ShapeGeometry, THREE, Vector3}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array

object BuildingDataModel {
  var oqacity:Double=0.8
  val u2=new Vector3(1d,0d,0d)
  val v2=new Vector3(0d,1d,0d)

  val materialCache: mutable.HashMap[Int, MeshBasicMaterial] = collection.mutable.HashMap[Int, MeshBasicMaterial]()
  def getMaterial(color: Int): MeshBasicMaterial = materialCache.getOrElseUpdate(color, {
    val c = new Color(((color >> 16) & 0xff).toDouble / 256d, ((color >> 8) & 0xff).toDouble / 256d, (color & 0xff).toDouble / 256d)
    val mat = new MeshBasicMaterial
    mat.color = c
    mat.transparent=oqacity!=1d
    mat.opacity=oqacity
    mat.side = THREE.DoubleSide
    mat
  })

  lazy val dashedMaterial= {
    val dm = new LineBasicMaterial()
    dm.color = new Color(1d, 0, 0)
    dm.linewidth = 3
    dm
  }
  def toPath(pathList:Seq[PointList]): Path2D.Double = {
    val pa = new Path2D.Double
    if (pathList.nonEmpty) {
      for (pList <- pathList; if pList.points.size > 2) {
        pa.moveTo(pList.points.head.x, pList.points.head.y)
        for (ix <- 1 until pList.points.size)
          pa.lineTo(pList.points(ix).x, pList.points(ix).y)
        pa.closePath()
      }
    }
    pa
  }

  def areaToPoints(area: Area): Seq[PointList] = {
    val iter = area.getPathIterator()
    val retArray = new Array[Double](6)
    val pathList = new collection.mutable.ArrayBuffer[PointList]()
    var pList: collection.mutable.ArrayBuffer[VectorConstant] = null
    while (!iter.isDone) {
      iter.currentSegment(retArray) match {
        case PathIterator.SEG_MOVETO => pList = collection.mutable.ArrayBuffer[VectorConstant](new VectorConstant(retArray(0), retArray(1), 0))
        case PathIterator.SEG_LINETO => pList += new VectorConstant(retArray(0), retArray(1), 0)
        case PathIterator.SEG_CLOSE => pathList += PointList(pList.toSeq)
      }
      iter.next()
    }
    pathList.toSeq
  }
}



class BuildingDataModel(module:BuildingModule) extends AbstractBuildingModel {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  import BuildingDataModel._
  var buildingRef:Reference=_

  var cellFirstCreated:Boolean=false

  protected val geomList=ArrayBuffer[Mesh]()
  protected val partAreaGeomMap=collection.mutable.HashMap[Int,Mesh]()
  protected val connAreaMap=collection.mutable.HashMap[(Int,Int),List[Area]]()

  val partAreaSubscriber: ElemSubscriber[PartArea with SelectionDecorable] =new ElemSubscriber((ref, const)=>new PartArea(ref,const,this) with SelectionDecorable{
    var helper:LineSegments=_
    override def showSelection(): Unit = {
      val elem=partAreaGeomMap(ref.instance)
      helper=new LineSegments(new EdgesGeometry(elem.geometry,1d),BuildingDataModel.dashedMaterial)
      helper.applyMatrix(elem.matrix)

      module.canvas.scene.add(helper)
      module.canvas.repaint()
    }

    override def hideSelection(): Unit = {
      if(helper!=null) module.canvas.scene.remove(helper)
      module.canvas.repaint()
    }
  },readyLoaded,updateData)

  val cellSubscriber: ElemSubscriber[DecoratedCell] =new ElemSubscriber((ref, const)=>new DecoratedCell(module,ref,const), cellsLoaded,updateData)

  val roomSubscriber: ElemSubscriber[Room] =new ElemSubscriber((ref, const)=>new Room(ref,const), ()=>
  {WebSocketConnector.createSubscription(buildingRef,2,partAreaSubscriber)},updateData)

  val planeSubscriber: ElemSubscriber[Plane] =new ElemSubscriber((ref, const)=>new Plane(ref,const), ()=>
  {WebSocketConnector.createSubscription(buildingRef,1,cellSubscriber)},updateData)

  val cutPlaneSubscriber:ElemSubscriber[CutPlane]=new ElemSubscriber((ref,const)=>new CutPlane(ref,const),()=>{
    WebSocketConnector.createSubscription(buildingRef,0,planeSubscriber)
  },updateData)

  def load(ref:Reference): Unit = {
    buildingRef=ref
    WebSocketConnector.createSubscription(buildingRef,5,cutPlaneSubscriber)
  }

  def createPartArea(owner:Array[OwnerReference],plane:Int,firstCell:Int,next:()=>Unit):Unit={
    println("createPart plane "+plane+" firstCell:"+firstCell)
    for(IntConstant(pi)<-WebSocketConnector.createInstance(304,owner)){
      val ref=Reference(304,pi)
      WebSocketConnector.writeInstanceField(ref,0,IntConstant(plane))
      WebSocketConnector.writeInstanceField(ref,1,IntConstant(firstCell))
      next()
    }
  }

  def cellsLoaded():Unit= {
    println("cellsLoaded empty:"+cellSubscriber.map.values.isEmpty +" " +planeSubscriber.map.isEmpty)
    if(cellSubscriber.map.isEmpty && ! cellFirstCreated){
      cellFirstCreated=true
      for(IntConstant(inst)<-WebSocketConnector.createInstance(303,Array(OwnerReference(1,buildingRef)))){
        println("newInst="+inst)
        val planes: Seq[Plane] =planeSubscriber.map.values.toSeq.sortBy(_.ref.instance)
        val cellRef=Reference(303,inst)
        WebSocketConnector.writeInstanceField(cellRef,0,IntConstant(planes.head.ref.instance))
        WebSocketConnector.writeInstanceField(cellRef,1,IntConstant(planes(1).ref.instance))
        WebSocketConnector.writeInstanceField(cellRef,2,IntList(planes.drop(2).map(_.ref.instance).toArray))
        val owner=Array(OwnerReference(2,buildingRef))
        var plid= -1
        def loop():Unit={
          plid+= 1
          if(plid<planes.size)
          createPartArea(owner,planes(plid).ref.instance,inst,loop)
        }
        loop()
      }

    }
    else WebSocketConnector.createSubscription(buildingRef,3,roomSubscriber)
  }

  def storePolygon(planeID:Int, cellID:Int, area:Area): Unit ={
    val key=(planeID,cellID)
    if(connAreaMap.contains(key)) {
      connAreaMap(key)=area::connAreaMap(key)
    } else connAreaMap(key)=List(area)
  }

  def updateData(): Unit ={
    clearGeometry()
    module.canvas.repaint()
    createGeometry()
  }

  def clearGeometry(): Unit ={
    println("clear Geometry "+geomList.size)
    connAreaMap.clear()
    for(geom<-geomList)
      module.canvas.scene.remove(geom)
    geomList.clear()
    materialCache.clear()
    partAreaGeomMap.clear
  }

  def findPartAreas(defPlane:Int,cell:Int): Iterator[PartArea] =
    partAreaSubscriber.map.valuesIterator.filter(ar=>ar.defPlaneID==defPlane&&
      (ar.firstCell.ref.instance==cell||(ar.secondCell.isDefined&&ar.secondCell.get.ref.instance==cell)))


  def createQuadGeometry(points:Seq[VectorConstant],el:PartArea): Unit =
    if(points.size==4) {
      val geom = new MyBufferGeometry()
      val apoints = new Float32Array(6 * 3)
      for (i <- 0 to 2) {
        apoints(i * 3) = points(i).x.toFloat
        apoints(i * 3 + 1) = points(i).y.toFloat
        apoints(i * 3 + 2) = points(i).z.toFloat
      }
      for (i <- 0 to 2) {
        apoints(9 + i) = apoints(6 + i)
        apoints(15 + i) = apoints(i)
      }
      apoints(12) = points(3).x.toFloat
      apoints(13) = points(3).y.toFloat
      apoints(14) = points(3).z.toFloat
      geom.addAttribute("position", new Float32BufferAttribute(apoints, 3))
      geom.computeFaceNormals()
      addMesh(new Mesh(geom, BuildingDataModel.getMaterial(el.aufbau)),el)
    } else println("Wrong number of points " + points.size)

  def addMesh(mesh:Mesh,el:PartArea): Unit ={
    mesh.name = el.ref.instance.toString
    partAreaGeomMap(el.ref.instance) = mesh
    geomList += mesh
    module.canvas.scene.add(mesh)
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




  def createGeometry(cutPlane:CutPlane=NoCutPlane): Unit = {
    println("Create Geometry "+cutPlane.name)
    val planeQuats=collection.mutable.HashMap[Plane3D,Matrix4]()

    def buildUpGeometryPlane(r:PointList,defPlane:Plane3D,el:PartArea): Unit = {
      println("BG el:"+el.ref+" points:"+r.points.map(p=>p.toString+"=>"+defPlane.toWorldVector(p)).mkString(" | "))
      if(r.numVecs==4)
        createQuadGeometry(r.points.map(defPlane.toWorldVector),el)
      else if(r.numVecs>2){
        val shape = new Shape()
        val tpoints = r.points
        shape.moveTo(tpoints.head.x, tpoints.head.y)
        for (i <- 1 until tpoints.size)
          shape.lineTo(tpoints(i).x, tpoints(i).y)
        shape.lineTo(tpoints.head.x, tpoints.head.y)
        val array = js.Array[Shape]()
        array.push(shape)
        val geometry = new ShapeBufferGeometry(array, 12)
        val mesh = new Mesh(geometry, getMaterial(el.aufbau))
        val m2 = planeQuats.getOrElseUpdate(defPlane, createQuatMatrix(defPlane))
        m2.setPosition(new Vector3(defPlane.pos.x, defPlane.pos.y, defPlane.pos.z))
        mesh.applyMatrix(m2)
        addMesh(mesh, el)
      }
    }

    for (el <- partAreaSubscriber.map.valuesIterator; if el.secondCell.isDefined) {
      val points = el.createCornerPoints(cutPlane).toSeq
      val defPlane = el.defPlane.plane
      val area = new Area( Polygon.toPath2d( points))
      storePolygon(el.defPlaneID, el.firstCell.ref.instance, area)
      storePolygon(el.defPlaneID, el.secondCell.get.ref.instance, area)
      buildUpGeometryPlane(PointList(points),defPlane,el)
    }
    for (el <- partAreaSubscriber.map.valuesIterator; if el.secondCell.isEmpty) {
      val key=(el.defPlaneID,el.firstCell.ref.instance)
      val defPlane = el.defPlane.plane
      val points=el.createCornerPoints(cutPlane).toSeq
      if(connAreaMap.contains(key)){
        val mainArea=new Area(Polygon.toPath2d(points))
        val otherPolys: Seq[Area] =connAreaMap(key)

        for(o<-otherPolys){
          mainArea.subtract(o)}
        val result=Polygon.newAreaToPoints(mainArea)

        for(r<-result; if Math.abs(r.getArea)>VectorConstant.tolerance)
          buildUpGeometryPlane(r,defPlane,el)

      } else buildUpGeometryPlane(PointList(points),defPlane,el)
    }
    module.canvas.repaint()
  }


  protected def readyLoaded(): Unit ={
    createGeometry()
    module.updateResize()
    module.showCutPlanes()
  }

  def shutDown(): Unit ={
    cellSubscriber.unsubscribe()
    roomSubscriber.unsubscribe()
    planeSubscriber.unsubscribe()
  }

  override def getPlane(id: Int): Plane = planeSubscriber.map(id)

  override def getRoom(id: Int): Option[Room] = roomSubscriber.map.get(id)

  override def getCell(id: Int): Cell = cellSubscriber.map(id)

  def getpartArea(id:Int):PartArea= partAreaSubscriber.map(id)
}
