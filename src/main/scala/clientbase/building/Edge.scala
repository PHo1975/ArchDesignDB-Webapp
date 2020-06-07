package clientbase.building

import definition.expression.VectorConstant

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Edge{
  def apply(s:VectorConstant,e:VectorConstant)=
    if(s<e)new Edge(s,e)
    else new Edge(e,s)
}

class Edge protected (val start:VectorConstant,val end:VectorConstant) {

  val partAreas=mutable.HashSet[Int]()
  lazy val dir=end-start

  override def toString: String =("Edge "+start.toString()+"->"+end.toString())+ " pas: "+partAreas.mkString(", ")


  def isSimilar(thatStart:VectorConstant,thatEnd:VectorConstant): Boolean =  start.similar(thatStart) && end.similar(thatEnd)

  def sameRay(ostart:VectorConstant,oend:VectorConstant): Boolean =
    dir.isNearlyLinearyDependentFrom(oend-ostart)&& (start-ostart).isNearlyLinearyDependentFrom(dir)

  def sameRay(other:Edge): Boolean = dir.isNearlyLinearyDependentFrom(other.dir) && (start-other.start).isNearlyLinearyDependentFrom(dir)

  def intersects(other:Edge): Boolean = {
    sameRay(other) &&
    ((start.similar(other.start)&&(end.isInSegmentExclusive(other.start,other.end) || other.end.isInSegmentExclusive(start,end)) ) ||
    (end.similar(other.end)&&(start.isInSegmentExclusive(other.start,other.end) || other.start.isInSegmentExclusive(start,end)) ) ||
    ((start.isInSegmentExclusive(other.start,other.end)||end.isInSegmentExclusive(other.start,other.end)||
      other.start.isInSegmentExclusive(start,end)||other.end.isInSegmentExclusive(start,end))))
  }

  def intersectionRest(other:Edge):Iterable[Edge]= {
    val rest=ArrayBuffer[Edge]()
    if(start<other.start) rest+=Edge(start,other.start)
    if(end>other.end) rest+=Edge(other.end,end)
    for(r<-rest)
      r.partAreas++=partAreas
    rest
  }

  def getIntersection(other:Edge):Edge= {
    val first=if(start<other.start) other.start else start
    val second=if(end>other.end) other.end else end
    val result=Edge(first,second)
    result.partAreas++=partAreas
    result
  }
}
