package clientbase.viewer2d

import definition.data.{DimensionPoint, Reference}
import definition.expression._
import org.denigma.threejs.{Camera, Mesh, Object3D, ShapeGeometry}

import scala.scalajs.js

class DimLineElement(nref:Reference,ncolor:Int,position:VectorConstant,style:Int,val angle:Double,val mainRefPoint:VectorConstant,
                     val relDist:Double,val textScale:Double,val points:IndexedSeq[DimensionPoint]) extends
  GraphElem(nref,ncolor) {
  val radAngle: Double = angle * math.Pi / 180d
  val mdirVector: VectorConstant = VectorConstant.fromAngle2D(radAngle)
  val hdirVector = new VectorConstant(-mdirVector.y, mdirVector.x, 0)
  val mline = Line3D(position, mdirVector)
  lazy val intersectionLines: IndexedSeq[(DimensionPoint, VectorConstant)] = points.map(p => (p, mline.intersectionWith(Line3D(p.refPoint, hdirVector)))).sortBy(_._2)(VectorConstant.pointOrdering)
  lazy val mainRefIntersection: VectorConstant = mline.intersectionWith(Line3D(mainRefPoint, hdirVector))
  lazy val firstInterPoint: VectorConstant = if (intersectionLines.isEmpty) NULLVECTOR else intersectionLines.head._2
  lazy val lastInterPoint: VectorConstant = if (intersectionLines.isEmpty) mainRefPoint else intersectionLines.last._2
  lazy val styleInfo: DimLineStyle = DimLineStyleHandler.getStyle(style)
  lazy val hitPoints: IndexedSeq[VectorConstant] = intersectionLines.map(_._2)
  protected val _geometry= new js.Array[Object3D]

  lazy val bounds: BRect = {
    var minx = Double.MaxValue
    var miny = Double.MaxValue
    var maxx = Double.MinValue
    var maxy = Double.MinValue
    for (li <- intersectionLines; p = li._1.refPoint; ip = li._2) {
      if (p.x < minx) minx = p.x
      if (p.y < miny) miny = p.y
      if (p.x > maxx) maxx = p.x
      if (p.y > maxy) maxy = p.y
      if (ip.x < minx) minx = ip.x
      if (ip.y < miny) miny = ip.y
      if (ip.x > maxx) maxx = ip.x
      if (ip.y > maxy) maxy = ip.y
      li._1.textPos match {
        case Some(tp) =>
          if (tp.x < minx) minx = tp.x
          if (tp.y < miny) miny = tp.y
          if (tp.x > maxx) maxx = tp.x
          if (tp.y > maxy) maxy = tp.y
        case _ =>
      }
    }
    val delta = (styleInfo.textPosition + styleInfo.textHeight) / 10d
    minx -= delta
    miny -= delta
    maxx += delta
    maxy += delta
    BRect(minx, miny, maxx, maxy)
  }

  def getBounds(container: ElemContainer): BRect =bounds

  override def calcScreenBounds(container: ElemContainer, camera:Camera, res:BoundsContainer): Unit =
    GraphElem.calcScreenBoundsfrom2Points(firstInterPoint,lastInterPoint,camera,res)

  override def createGeometry(container: ElemContainer): Unit = {
    val meshName="D"+nref.instance.toString
    GraphElem.loadFont("Arial",(font,repaint)=>
      if(repaint)container.dataUpdated()
      else {
        def drawLine(a:VectorConstant,b:VectorConstant): Unit = {
          //println("drawline "+a+" "+b)
          val mesh=GraphElem.createLineGeometry(container,a,b,10,0)
          mesh.name=meshName
          _geometry.push(mesh)
          container.addGeometry(mesh)
        }

        def drawText(meshes: js.Array[Mesh],withHText:Boolean,fontHeight:Double,posx:Double,posy:Double,angle:Double): Unit = {
          val lastMesh=meshes.size-1
          for(mi<-meshes.indices;m=meshes(mi)){
            m.position.x=posx
            m.position.y=posy
            m.rotation.z= angle
            if(withHText&& mi==lastMesh) {
              m.scale.x=0.8
              m.scale.y=0.8
              m.position.x=posx+Math.cos(angle)*0.03*fontHeight-Math.sin(angle)*0.03*fontHeight
              m.position.y=posy+Math.sin(angle)*0.03*fontHeight+Math.cos(angle)*0.03*fontHeight
            }
            container.addGeometry(m)
            _geometry.push(m)
          }
        }
        val rscale=container.scaleRatio
        val hoff=(lastInterPoint-firstInterPoint).unit*(rscale*styleInfo.helpLineOffset/1000)
        if(!styleInfo.hideDimensionLine)drawLine(firstInterPoint-hoff,lastInterPoint+hoff)
        for((mp,ip)<-intersectionLines;p1=mp.refPoint){
          val dirUnit=(ip-p1).unit
          val decorOff=dirUnit*(rscale*styleInfo.helpLineOffset/1000d)
          if(mp.helpLineLength==0|| styleInfo.hideHelpLine) drawLine(ip-decorOff,ip+decorOff)
          else if(styleInfo.hasFixedHelpLine) drawLine(ip+decorOff,ip-dirUnit*(styleInfo.fixedHelpLineLength/1000d))
          else drawLine(p1+dirUnit* mp.helpLineLength,ip+decorOff)
          // draw decore
          if(styleInfo.isStationDimLine) {

          }
          else {
            val geom=GraphElem.createCircleGeometry(container,.3d*container.scaleRatio/1000d,30d,360d,30d)
            val mesh = new Mesh(geom, GraphElem.getMaterial(color))
            mesh.position.x=ip.x
            mesh.position.y=ip.y
            mesh.name=meshName
            container.addGeometry(mesh)
            _geometry.push(mesh)
          }
        }
        val textDistance=hdirVector*(styleInfo.textPosition*rscale/1000d)
        if(styleInfo.isStationDimLine){
          val mUnit=mdirVector.unit
          for((_,il)<-intersectionLines){
            val deltaM=il-mainRefIntersection
            val measure=deltaM.toDouble*deltaM.unit.getScaleTo(mUnit) +relDist
            val text: Seq[String] =styleInfo.formatMeasure(measure)
            val fontHeight=styleInfo.textHeight/1000d*container.scaleRatio
            val moveitY = 0.3f
          }
        }
        else for(Seq(ixa,ixb)<-intersectionLines.indices.sliding(2);a=intersectionLines(ixa);b=intersectionLines(ixb)) {
          val measure= (a._2-b._2).toDouble
          val text: Seq[String] =styleInfo.formatMeasure(measure)
          //println("\ntext:"+text.mkString(";"))
          val fontHeight=styleInfo.textHeight/1000d*container.scaleRatio
          val midPoint=VectorConstant.midPoint(a._2,b._2)
          val geometries: js.Array[ShapeGeometry] =GraphElem.createTextGeometry(container,if(text.size==2 && text(1)=="0")text.head else text.mkString,
            styleInfo.textHeight,font)
          geometries.head.computeBoundingBox()
          geometries.last.computeBoundingBox()
          val textWidth=geometries.last.boundingBox.max.x-geometries.head.boundingBox.min.x
          var moveitX=1f
          var moveitY=0f
          val withHtext= text.size>1&&text(1)!="0"
          val worldTextWidth=/*(if(withHtext) textWidth * (text.size + 1) / text.size  else*/ textWidth//)
          if(worldTextWidth>measure) {
            if(ixa==0) moveitX=if(withHtext) 4.5f else 3.5f
            else if(ixb==intersectionLines.size-1) moveitX= -2.1f
            else if((a._2-intersectionLines(ixa-1)._2 ).toDouble  /3.9f > worldTextWidth) moveitX=if(withHtext) 3.5f else 3f
            else if ((intersectionLines(ixb+1)._2 - b._2).toDouble/3.9f >worldTextWidth) moveitX= -2.1f
            else moveitY=if(ixa % 2 ==1) 0.8f else -1.5f
          }
          val posVect=midPoint+textDistance-mdirVector*(textWidth/2f*moveitX)+hdirVector*(moveitY*fontHeight)
          val meshes =geometries.map(g=>{
            val nm=new Mesh(g,GraphElem.getMaterial(color))
            nm.name=meshName
            nm
          })
          drawText(meshes,withHtext,styleInfo.textHeight,posVect.x,posVect.y,radAngle)
        }
      })
  }

  override def geometry: js.Array[Object3D] = _geometry

  override def getFormatFieldValue(fieldNr: Int): Constant = EMPTY_EX

  override def getHitPoints(container: ElemContainer): Seq[VectorConstant] = hitPoints
}