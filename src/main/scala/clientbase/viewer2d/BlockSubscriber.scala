package clientbase.viewer2d

import java.io.DataInput

import clientbase.connection.Subscriber
import definition.data.{BlockData, Reference}
import definition.typ.BlockClass

class BlockSubscriber(blockClass:BlockClass,updateListener:()=>Unit) extends Subscriber[BlockData] {
  var blockList:Seq[BlockData]=Seq.empty


  override def factory(in: DataInput): BlockData = {
    val inst=in.readInt()
    val readBuffer=new Array[Byte](blockClass.blocksize)
    in.readFully(readBuffer)
    new BlockData(Reference(blockClass.id,inst),readBuffer)
  }

  protected def load(data: Iterator[BlockData]): Unit ={
    blockList=data.toSeq
    updateListener()
  }

  override def onLoad(data: Iterator[BlockData]): Unit = load(data)

  override def onUpdate(data: Iterator[BlockData]): Unit = load(data)

  override def onChange(data: BlockData): Unit = {
    blockList.indexWhere(_.ref==data.ref) match {
      case -1 => println("On change unknown Data "+data.ref)
      case ix =>
        blockList=blockList.updated(ix,data)
        updateListener()
    }
  }

  override def onDelete(data: Reference): Unit = if(blockList.exists(_.ref==data.ref) ) {
    blockList=blockList.filter(_.ref!=data)
    updateListener()
  } else println("Delete Block Unknown "+data)



  override def onChildAdded(data: BlockData): Unit = {
    blockList=blockList :+data
    updateListener()
  }
}
