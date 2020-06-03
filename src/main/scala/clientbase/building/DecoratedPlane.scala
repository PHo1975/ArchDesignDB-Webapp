package clientbase.building

import building.Plane
import clientbase.viewer2d.SelectionDecorable
import definition.data.{Named, Referencable, Reference}
import definition.expression.{Line3D, Plane3D, VectorConstant}
import org.denigma.threejs.Line


class DecoratedPlane(module: BuildingModule, pl:Plane) extends SelectionDecorable with Referencable with Named{
  var meshList:Seq[Line]=Seq.empty
  def ref:Reference=pl.ref
  def name: String =pl.name

  override def showSelection(): Unit = {
    for(m<-meshList)
      m.material=BuildingDataModel.planeSelectMaterial
    module.canvas.repaint()
  }

  override def hideSelection(): Unit = {
    for(m<-meshList)
      m.material=BuildingDataModel.planeMaterial
    module.canvas.repaint()
  }

  protected def checkPlane(defPlane:Plane3D,points:Seq[VectorConstant]):Option[Line] =
    if( ! pl.plane.dir.isLinearyDependentFrom(defPlane.dir)){
      //println("checkPlane "+name+" "+p.plane+" with "+defPlane+" points:"+points.mkString("|"))
      val cutLine=pl.plane.intersectionWith(defPlane)
      //println("Cutline:"+cutLine)
      val cutPoints = (for(el: Seq[VectorConstant] <- points.sliding(2);
                                                   p1=el.head; p2=el(1);
                                                   edge=Line3D(p1,p2-p1)
                                                   if ! edge.dir.isLinearyDependentFrom(cutLine.dir);
                                                   cutPoint=cutLine.intersectionWith(edge)
                                                   if cutPoint.isInSegment(p1,p2))
          yield cutPoint).toSeq
      if(cutPoints.size==2)
        Some(createLine(cutPoints.head,cutPoints(1)))
      else None
    }else None

  def createGeometry(util:PlaneCalcUtil): Unit = {
    val m = module.dataModel
    meshList=Seq(checkPlane(util.planeX,util.planeXPoints),
    checkPlane(util.planeY,util.planeYPoints),
    checkPlane(util.planeZ,util.planeZPoints)).flatten
  } 

  def createLine(p1:VectorConstant,p2:VectorConstant):Line= {
    val mesh=BuildingDataModel.createLine(p1,p2, BuildingDataModel.planeMaterial)
    mesh.name = "P"+ref.instance.toString
    module.canvas.scene.add(mesh)
    mesh
  }
}