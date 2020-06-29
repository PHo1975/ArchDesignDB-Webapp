package clientbase.building

import building._
import clientbase.connection.WebSocketConnector
import clientbase.viewer2d.MyBufferGeometry
import definition.data.{OwnerReference, Reference}
import definition.expression.{PartArea => _, _}
import org.denigma.threejs.{LineBasicMaterial, Plane => _, _}
import util.clipping.{Area, Path2D, PathIterator}

import scala.collection.mutable
import scala.scalajs.js.typedarray.Float32Array
import scala.util.matching.Regex

protected[building] trait PlaneCalcUtil{
  def planeX:Plane3D
  def planeY:Plane3D
  def planeZ:Plane3D
  def planeXPoints:Seq[VectorConstant]
  def planeYPoints:Seq[VectorConstant]
  def planeZPoints:Seq[VectorConstant]
}


object BuildingDataModel {
  val oqacity:Double=0.8
  val u2=new Vector3(1d,0d,0d)
  val v2=new Vector3(0d,1d,0d)
  val greyColor: Int = -6710887

  val roomInternMeshMaterial: MeshBasicMaterial = {
    val ri= new MeshBasicMaterial()
    ri.color=new Color(0.9,0.9,0.9)
    ri.transparent=true
    ri.opacity=0.1d
    ri.side=THREE.DoubleSide
    ri
  }

  val textMaterial: MeshBasicMaterial = {
    val ri= new MeshBasicMaterial()
    ri.color=new Color(0.0,0.0,0.0)
    ri.side=THREE.DoubleSide
    ri.transparent=false
    ri.opacity=1d
    ri
  }

  def threeColor(color:Int): Color =new Color(((color >> 16) & 0xff).toDouble / 256d, ((color >> 8) & 0xff).toDouble / 256d, (color & 0xff).toDouble / 256d)

  val materialCache: mutable.HashMap[Int, MeshBasicMaterial] = collection.mutable.HashMap[Int, MeshBasicMaterial]()
  def getMeshMaterial(color: Int): MeshBasicMaterial = if(color== -10) roomInternMeshMaterial else
    materialCache.getOrElseUpdate(color, {
    val c = threeColor(color)
    val mat = new MeshBasicMaterial
    mat.color = c
    mat.transparent=oqacity!=1d
    mat.opacity=oqacity
    mat.side = THREE.DoubleSide
    mat
  })

  lazy val dashedMaterial: LineBasicMaterial = {
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

  val planeMatcher:Regex ="""P(\d+)""".r
  val planeDistance=5d



  val planeMaterial: LineDashedMaterial = {
    val dm = new LineDashedMaterial()
    dm.color = new Color(0, 0d, .4d)
    dm.dashSize=.4
    dm.gapSize=0.2
    dm
  }
  lazy val planeSelectMaterial: LineBasicMaterial = {
    val dm = new LineBasicMaterial()
    dm.color = new Color(1d, 0d, 0d)
    dm.linewidth = 2
    dm
  }

  def createLineMaterial(color:Int,width:Double):LineBasicMaterial = {
    val dm = new LineBasicMaterial()
    dm.color = threeColor(color)
    dm.linewidth =width
    dm
  }

  protected val lineMaterialBuffer: mutable.HashMap[Int, LineBasicMaterial] =collection.mutable.HashMap[Int,LineBasicMaterial]()

  def getLineMaterial(color:Int): LineBasicMaterial =lineMaterialBuffer.getOrElseUpdate(color,createLineMaterial(color,2))

  val lineSelectMaterial: LineBasicMaterial =getLineMaterial(255*256*256)

  def loopPartAreas(cutPlane:CutPlane,paIterator: => Iterator[PartArea],func:(PointList,PartArea)=>Unit): Unit = {
    // connection planes
    val connAreaMap: mutable.HashMap[(Int, Int), List[Area]] =collection.mutable.HashMap[(Int,Int),List[Area]]()

    def storePolygon(planeID:Int, cellID:Int, area:Area): Unit ={
      val key=(planeID,cellID)
      if(connAreaMap.contains(key)) {
        connAreaMap(key)=area::connAreaMap(key)
      } else connAreaMap(key)=List(area)
    }

    for (pa: PartArea <- paIterator; if pa.secondCellID!=0) { // inside planes
      val points = pa.createCornerPoints(cutPlane).toSeq
      val area = new Area( Polygon.toPath2d( points))
      storePolygon(pa.defPlaneID, pa.firstCellID, area)
      storePolygon(pa.defPlaneID, pa.secondCellID, area)
      func(PointList(points),pa)
    }
    // outside planes
    for (pa <- paIterator; if pa.secondCellID==0) {
      val points = pa.createCornerPoints(cutPlane).toSeq
      val key = (pa.defPlaneID, pa.firstCellID)
      if (connAreaMap.contains(key)) {
        val mainArea = new Area(Polygon.toPath2d(points))
        for (o <- connAreaMap(key))
          mainArea.subtract(o)
        val resultPointLists: Seq[PointList] = Polygon.newAreaToPoints(mainArea)
        for (r <- resultPointLists; if Math.abs(r.getArea) > VectorConstant.tolerance)
          func(r, pa)
      } else func(PointList(points), pa)
    }
  }

  def createLine(p1:VectorConstant,p2:VectorConstant,material:LineBasicMaterial):Line= {
    val points = new Float32Array(6)
    points(0)=p1.x.toFloat
    points(1)=p1.y.toFloat
    points(2)=p1.z.toFloat
    points(3)=p2.x.toFloat
    points(4)=p2.y.toFloat
    points(5)=p2.z.toFloat
    val geometry = new BufferGeometry()
    geometry.addAttribute("position", new Float32BufferAttribute(points, 3))
    val mesh=new Line(geometry, material)
    mesh.computeLineDistances()
    mesh
  }

  def createQuadGeometry(points: Seq[VectorConstant]): MyBufferGeometry =
    if (points.size == 4) {
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
      geom
    } else throw new IllegalArgumentException("Wrong number of points " + points.size)

}




class BuildingDataModel(module:BuildingModule) extends AbstractBuildingModel {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  var buildingRef:Reference=_
  var updateInibitorCount=0

  var cellFirstCreated:Boolean=false

  //val partAreaGeomMap: mutable.HashMap[Int, Mesh] =collection.mutable.HashMap[Int,Mesh]()

  val partAreaSubscriber: ElemSubscriber[PartArea] =
    new ElemSubscriber((ref, const)=>new PartArea(ref,const,this) ,module.readyLoaded,updateData)

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
          module.viewModel3D.createPartArea(owner,planes(plid).ref.instance,inst,loop)
        }
        loop()
      }

    }
    else WebSocketConnector.createSubscription(buildingRef,3,roomSubscriber)
  }

  def updateData(): Unit =
    if(updateInibitorCount>0) updateInibitorCount -= 1
    else      module.updateData()


  def findPartAreas(defPlane:Int,cell:Int): Iterator[PartArea] =
    partAreaSubscriber.map.valuesIterator.filter(ar=>ar.defPlaneID==defPlane&&
      (ar.firstCellID==cell||(ar.secondCellID!=0 && ar.secondCellID==cell)))



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



