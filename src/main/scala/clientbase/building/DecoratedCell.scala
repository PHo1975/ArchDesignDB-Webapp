package clientbase.building

import building.{AbstractBuildingModel, Cell, PartArea,NoCutPlane}
import clientbase.viewer2d.SelectionDecorable
import definition.data.Reference
import definition.expression.{Constant, VectorConstant}
import org.denigma.threejs.{BufferGeometry, Float32BufferAttribute, Line, LineSegments, Mesh}

import scala.scalajs.js.typedarray.Float32Array

class DecoratedCell(module: BuildingModule, nref:Reference, ndata: Seq[Constant]) extends Cell(module.dataModel,nref,ndata) with SelectionDecorable {
  var meshes:Seq[Line]=Seq.empty

  def createLineSegment(topPoints:Seq[VectorConstant]): LineSegments = {
    val points = new Float32Array(topPoints.size * 6 )
    var offset = 0
    if(topPoints.nonEmpty)
      for (List(p1, p2) <- (topPoints :+ topPoints.head).sliding(2)) {
        points(offset) = p1.x.toFloat
        points(offset + 1) = p1.y.toFloat
        points(offset + 2) = p1.z.toFloat
        points(offset + 3) = p2.x.toFloat
        points(offset + 4) = p2.y.toFloat
        points(offset + 5) = p2.z.toFloat
        offset += 6
      }
    val geometry = new BufferGeometry()
    geometry.addAttribute("position", new Float32BufferAttribute(points, 3))
    new LineSegments(geometry, BuildingDataModel.dashedMaterial)
  }

  def createMesh: Iterator[LineSegments] = {
    val topAreas: Iterator[PartArea] = module.dataModel.findPartAreas(topPlaneID, ref.instance)
    val bottomAreas = module.dataModel.findPartAreas(bottomPlaneID, ref.instance)
    (for (ta <- topAreas) yield createLineSegment(ta.createCornerPoints(NoCutPlane).map(ta.defPlane.plane.toWorldVector).toSeq))++
      (for (ba <- bottomAreas) yield createLineSegment(ba.createCornerPoints(NoCutPlane).map(ba.defPlane.plane.toWorldVector).toSeq))++(
        for (wa<-wallPlaneIDs;waArea<-module.dataModel.findPartAreas(wa,ref.instance)) yield
          createLineSegment(waArea.createCornerPoints(NoCutPlane).map(waArea.defPlane.plane.toWorldVector).toSeq)
      )


  }

  override def showSelection(): Unit = {
    meshes=createMesh.toSeq
    for(m<-meshes) module.canvas.scene.add(m)
    module.canvas.repaint()
  }

  override def hideSelection(): Unit = {
    for(m<-meshes) module.canvas.scene.remove(m)

    module.canvas.repaint()
  }
}
