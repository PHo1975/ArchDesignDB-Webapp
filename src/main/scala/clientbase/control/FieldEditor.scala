package clientbase.control

import clientbase.connection.WebSocketConnector
import definition.data.Referencable
import definition.typ.{AllClasses, SelectGroup}
import org.scalajs.dom.html.Div

trait FieldEditor {
  var dataList:Iterable[SelectGroup[_ <:Referencable]]=Seq.empty
  var inited=false
  var allowedClassIds:Map[String,Int]=Map.empty

  def fieldComponents: Seq[SidePanelComponent[_]]

  def allowedClassNames:Iterable[String]

  def getPanel:Div

  def setData(data:Iterable[SelectGroup[_<:Referencable]]): Unit = {
    dataList=data
    if(!inited) init()
    if(data!=null) {
      for(group <-data;el <-group.children;fc<-fieldComponents)
        fc.checkSearchValue(el)
      for(fc<-fieldComponents)
        fc.updateSearchValue()
    }
  }

  def init(): Unit = if (!inited) {
    inited=true
    allowedClassIds=allowedClassNames.map(a => a -> AllClasses.get.getClassIDByName(a) ).toMap
      for(f<-fieldComponents)
        f.createFieldMap(allowedClassIds)
    //println("Fieldeditor settings done")

  }

  def storeValue[A](newValue: A, component: SidePanelComponent[A]): Unit = {
    //println("store Value" +newValue+" comp:"+component.getClass+" datasize:"+dataList.size+" typMap:"+component.fieldMap)
    val instList=dataList.flatMap(_.children.filter(inst=>component.fieldMap.contains(inst.ref.typ )))
    if(instList.nonEmpty){
      val newConstant=component.getConstant(newValue)
      val typedMap: Map[Int, Iterable[Referencable]] =instList.groupBy(_.ref.typ)
      val keySet=typedMap.keySet
      /*TODO */
      if(keySet.map(aType=>component.fieldMap(aType)).size==1) // all types have the same field Number to change
        WebSocketConnector.writeInstancesField(instList,component.fieldMap(instList.head.ref.typ),newConstant) //write together
      else for(typ <-keySet) // write for distinct types separately
        WebSocketConnector.writeInstancesField(typedMap(typ),component.fieldMap(typ),newConstant)
    }
  }

  def storeValueMapped[A](component: SidePanelComponent[A], func: A => A): Unit = {
    val instList=dataList.flatMap(_.children.filter(inst=>component.fieldMap.contains(inst.ref.typ )))
    if(instList.nonEmpty)
      for(elem<-instList){
        val oldValue=component.elemToValueLookup(elem)
        val newValue=func(oldValue)
        val newConstant=component.getConstant(newValue)
        WebSocketConnector.writeInstanceField(elem.ref,component.fieldMap(elem.ref.typ),newConstant)
      }
  }
}


