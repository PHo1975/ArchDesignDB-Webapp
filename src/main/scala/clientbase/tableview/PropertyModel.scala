package clientbase.tableview

import clientbase.connection.{InstSubscriber, Subscriber}
import clientbase.control.{CellEditor, FocusOwner}
import definition.data.{InstanceData, Reference}
import definition.typ.AllClasses
import org.scalajs.dom.raw.Node
import util.Log



/**
 * Created by Peter Holzer on 05.07.2015.
 */
class PropertyModel(val topRef:Reference,val propField:Int,parentNode:Node,pathSubsID:Int,callBack:()=>Unit,
                    singleField:Boolean,allowedClass:Int) extends InstSubscriber {


  val tableModelList= new collection.mutable.LinkedHashMap[Int,TableModel]()

  override def onLoad(data: Iterator[InstanceData]): Unit = {
    internUpdate(data)
    callBack()
  }


  private def internAddTable(typ:Int):TableModel={
    val tm=new TableModel(tableModelList.size,typ,parentNode,pathSubsID,this,singleField,allowedClass==0||AllClasses.get.hasSubClasses(allowedClass))
    tableModelList(typ)=tm
    tm
  }

  private def internUpdate(data: Iterator[InstanceData]): Unit=
    for((typ,list)<- data.toSeq.groupBy(_.ref.typ))
      internAddTable(typ).load(list)


  override def onUpdate(data: Iterator[InstanceData]): Unit= {
    tableModelList.clear()
    internUpdate(data)
  }

  override def onChange(data: InstanceData): Unit = tableModelList.get(data.ref.typ) match {
    case Some(tableModel)=> tableModel.changeInstance(data)
    case None=> Log.e("change instance, table not found "+data.ref)
  }

  override def onDelete(ref: Reference): Unit = tableModelList.get(ref.typ) match {
    case Some(tableModel)=> tableModel.removeInstance(ref)
    case None=> Log.e("delate instance, table not found "+ref)
  }

  override def onChildAdded(data: InstanceData): Unit =
    tableModelList.getOrElse(data.ref.typ,internAddTable(data.ref.typ)).
    addInstance(data)

  def shutDown():Unit= {
    tableModelList.clear()
    unsubscribe()
  }

  def getPrevTable(toIx:Int):Option[FocusOwner]= if (toIx>0) tableModelList.values.find(_.index== toIx-1) else None

  def getNextTable(toIx:Int) :Option[FocusOwner]= if (toIx< tableModelList.size-1) tableModelList.values.find(_.index== toIx+1) else None
}
