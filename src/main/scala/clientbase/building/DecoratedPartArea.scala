package clientbase.building

import building.PartArea
import clientbase.building.BuildingDataModel.dashedMaterial
import clientbase.viewer2d.SelectionDecorable
import definition.data.{Referencable, Reference}
import org.denigma.threejs.{EdgesGeometry, LineSegments, Mesh}

class DecoratedPartArea(module:BuildingModule,val pa:PartArea,val mesh:Mesh) extends SelectionDecorable with Referencable {
  def ref: Reference =pa.ref
  var helper:LineSegments=_
  override def showSelection(): Unit = {
    helper=new LineSegments(new EdgesGeometry(mesh.geometry,1d),dashedMaterial)
    helper.applyMatrix(mesh.matrix)
    module.canvas.scene.add(helper)
    module.canvas.repaint()
    val edges=module.viewModel3D.edgeList.filter(_.partAreas.contains(pa.ref.instance))
    println("Edges for "+pa.ref.instance+"\n "+edges.mkString("\n "))
  }

  override def hideSelection(): Unit = {
    if(helper!=null) module.canvas.scene.remove(helper)
    module.canvas.repaint()
  }
}
