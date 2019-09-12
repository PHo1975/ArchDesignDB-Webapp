package clientbase.viewer2d

import clientbase.connection.WebSocketConnector
import definition.data.{EMPTY_REFERENCE, InstanceData, Reference}
import definition.expression._
import definition.typ.DataType
import org.denigma.threejs.Object3D

import scala.collection.immutable.Map
import scala.collection.mutable



class SymbolStamp(val ref:Reference,val templates: Seq[InstanceData])


object StampPool {
  val maxSize=50

  val measureElemContainer: ElemContainer = new ElemContainer{
    def scaleRatio: Double =1d/100d
    override def addGeometry(geom: Object3D): Unit = {}
    override def dataUpdated(): Unit = {}
  }

  lazy val generatorMap:Map[Int,(InstanceData,Double,VectorConstant,Expression=>Constant,VectorConstant=>VectorConstant)=>GraphElem]=Map(
    GraphElem.LINETYPE->createLine,
    GraphElem.ARCTYPE->createArc,
    GraphElem.ELLIPSETYP->createEllipse)

  val poolList: mutable.LinkedHashMap[Reference, Option[SymbolStamp]] = mutable.LinkedHashMap[Reference,Option[SymbolStamp]]()


  def loadSymbol(stampRef:Reference,angle:Double,position:VectorConstant, callback:Seq[GraphElem]=>Unit):Unit = {
    if(poolList.contains(stampRef)) poolList(stampRef) match {
      case Some(stamp)=> callback(stamp.templates.flatMap(generateElement(_,angle,position,_.getValue )))
      case None => callback(Seq(TextElement(EMPTY_REFERENCE,0,"Symbol "+stampRef+" not found",NULLVECTOR,"Arial",3,1,0,0,0,0)))
    } else
      WebSocketConnector.loadChildren(stampRef,0, data=>{
        if(data.nonEmpty) {
          //println("load Symbol "+stampRef+" angle: "+angle+" num:"+data.size+ " call:"+callback)
          poolList.put(stampRef,Some(new SymbolStamp(stampRef,data)))
          callback(data.flatMap(generateElement(_,angle,position, _.getValue )))
        }
        else poolList.put(stampRef,None)
      })
  }


  def createLine(data:InstanceData,angle:Double,pos:VectorConstant,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant): LineElement = {
    LineElement(data.ref, translator(data.fieldData.head).toInt, translator(data.fieldData(1)).toInt,
      translator(data.fieldData(2)).toInt, rotator(translator(data.fieldData(3)).toVector)+pos, rotator(translator(data.fieldData(4)).toVector)+pos)
  }

  def createArc(data:InstanceData,angle:Double,pos:VectorConstant,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant): ArcElement = {
    ArcElement(data.ref, translator(data.fieldData.head).toInt, translator(data.fieldData(1)).toInt,
      translator(data.fieldData(2)).toInt, rotator(translator(data.fieldData(3)).toVector)+pos, translator(data.fieldData(4)).toDouble,
      translator(data.fieldData(5)).toDouble + angle, translator(data.fieldData(6)).toDouble + angle)
  }

  def createEllipse(data:InstanceData,angle:Double,pos:VectorConstant,translator: Expression=>Constant,rotator:VectorConstant=>VectorConstant): EllipseElement = {
    EllipseElement(data.ref, translator(data.fieldData.head).toInt, translator(data.fieldData(1)).toInt,
      translator(data.fieldData(2)).toInt, rotator(translator(data.fieldData(3)).toVector)+pos, translator(data.fieldData(4)).toDouble,
      translator(data.fieldData(5)).toDouble, translator(data.fieldData(6)).toDouble + angle, translator(data.fieldData(7)).toDouble,
      translator(data.fieldData(8)).toDouble)
  }

  //private def getNone(data:InstanceData,translator:Expression=>Constant)= None

  def generateElement(data:InstanceData,angle:Double,pos:VectorConstant,translator: Expression=>Constant): Option[GraphElem]= {
    val radAngle=angle*Math.PI/180
    val cosa=math.cos(radAngle)
    val sina=math.sin(radAngle)
    def rotator(v:VectorConstant):VectorConstant=
      new VectorConstant(v.x*cosa-v.y*sina,v.x*sina+v.y*cosa,0)

    generatorMap.get(data.ref.typ).map(_ (data, angle,pos, translator, rotator))
  }


  def parseParamValues(paramString:String):Map[String,Constant]=paramString.split('|').flatMap(_.split('#') match {
    case Array(a,b)=> StringParser.parse(b,DataType.StringTyp) match {
      case p:ParserError=> None
      case ex:Expression=>Some(a -> ex.getValue)
    }
    case _=> None
  }).toMap
}
