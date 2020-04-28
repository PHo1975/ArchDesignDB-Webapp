package clientbase.building

import java.io.DataInput

import clientbase.connection.Subscriber
import definition.data.{InstanceData, Referencable, Reference}
import definition.expression.{Constant, Expression}


class ElemSubscriber[A<: Referencable](nfactory: (Reference,Seq[Constant])=>A,loadCallback:()=>Unit,updateCallback:()=>Unit) extends Subscriber[A] {
  val map: collection.mutable.Map[Int, A] = collection.mutable.HashMap[Int,A]()


  override def onLoad(data: Iterator[A]): Unit = {
    for(d<-data) map(d.ref.instance)=d
    loadCallback()
  }

  override def onUpdate(data: Iterator[A]): Unit = {
    map.clear()
    for(d<-data) map(d.ref.instance)=d
    updateCallback()
  }


  override def onChange(data: A): Unit = {
    map(data.ref.instance)=data
    updateCallback()
  }

  override def onDelete(data: Reference): Unit = {
    map.remove(data.instance)
    updateCallback()
  }

  override def onChildAdded(data: A): Unit = {
    map(data.ref.instance)=data
    updateCallback()
  }

  override def factory(in: DataInput): A = {
    val ref=Reference(in)
    val fieldNr=in.readByte()
    val list: Seq[Constant] =for(i<-0 until fieldNr)yield Expression.read(in).getValue
    InstanceData.readOwners(in,ref)
    InstanceData.readSecondUseOwners(in)
    in.readBoolean
    nfactory(ref,list)
  }
}
