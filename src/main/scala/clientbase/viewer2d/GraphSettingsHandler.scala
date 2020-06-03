package clientbase.viewer2d

import java.awt.BasicStroke

import clientbase.building.BuildingDataModel
import clientbase.connection.{WebSocketConnector, WebSystemSettings}
import definition.data._
import definition.typ.SystemSettings

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}
import scala.util.matching.Regex


object GraphSettingsHandler {
  var mainFolders:Seq[InstanceData]=Seq.empty
  val settingHandlers: ArrayBuffer[AbstractSettingHandler] =collection.mutable.ArrayBuffer[AbstractSettingHandler]()

  def setup():Unit= {
    println("setup "+settingHandlers.mkString("|"))
    val mainFoldersRef=SystemSettings().asInstanceOf[WebSystemSettings].systemSettingsMap("GraphicSettings")
    WebSocketConnector.loadChildren(mainFoldersRef,1,data=>{
      mainFolders=data
      for(handler<-settingHandlers)
        getMainFolder(handler.name) match {
          case Some(folderInst)=> handler.loadSettings(folderInst.ref)
          case _=>println("setup Mainfolder not found "+handler.name)
        }
    })
  }

  def registerHandler(handler:AbstractSettingHandler): Unit = {
    println("registerHandler:"+handler.name)
    settingHandlers+=handler
    if(mainFolders.nonEmpty)
      getMainFolder(handler.name) match {
        case Some(inst)=> handler.loadSettings(inst.ref)
        case _=>println("Mainfolder not found "+handler.name)
      }

  }

  def getMainFolder(name:String): Option[InstanceData] = mainFolders.find(_.fieldValue.head.toString==name)
  def shutDown(): Unit =    settingHandlers foreach (_.shutDown())
}



trait AbstractSettingHandler {
  def init(): Unit =  GraphSettingsHandler.registerHandler(this)
  def name:String
  def loadSettings(ref:Reference): Unit
  def shutDown(): Unit = {}
}



case class DimLineStyle(ref:Reference,id:Int,name:String,options:Int,textFont:String,textHeight:Double,
                        roundMM:Int,textPosition:Double,numberFormat:String,lineWidth:Double,
                        fixedHelpLineLength:Double,helpLineOffset:Double) {
  def this(data: InstanceData) = this(data.ref, data.fieldValue.head.toInt, data.fieldValue(1).toString, data.fieldValue(2).toInt,
    data.fieldValue(3).toString, data.fieldValue(4).toDouble, data.fieldValue(5).toInt, data.fieldValue(6).toDouble,
    data.fieldValue(7).toString, data.fieldValue(8).toDouble, data.fieldValue(9).toDouble, data.fieldValue(10).toDouble)

  import DimLineStyleHandler._

  //lazy val symbolElems: IndexedSeq[GraphElem] = ClientQueryManager.queryInstanceFact(ref, 0, GraphElemFactory)

  def hideDimensionLine: Boolean = (options & HideDimensionLine) > 0

  def hideHelpLine: Boolean = (options & HideHelpLine) > 0

  def isStationDimLine: Boolean = (options & IsStationLine) > 0

  def hasFixedHelpLine: Boolean = (options & FixedHelpLine) > 0

  def hasHighMM: Boolean = (options & HighMM) > 0

  def unitStyle: Int = (options >> 5) & 7

  def roundedMeasure(measure: Double): Double = if (roundMM == 0) measure else (Math.round(measure / roundMM) * roundMM).toDouble

  def formatMeasure(measure: Double): Seq[String] = {
    val ftext = (unitStyle match {
      case UnitMM => numberFormat.format(roundedMeasure(measure * 1000))
      case UnitM => numberFormat.format(roundedMeasure(measure * 1000) / 1000)
      case UnitM_MM =>
        if (math.abs(measure) < 1) {
          if (isStationDimLine && measure != 0d) f"${roundedMeasure(measure * 1000) / 10}%+3.1f"
          else                                   f"${roundedMeasure(measure * 1000) / 10}%3.1f"
        }
        else numberFormat.format(roundedMeasure(measure * 1000) / 1000)
      case _ => "Fehler"
    }).trim
    if (hasHighMM) ftext match {
      case Match1(text, h) => Seq(text, h)
      case Match2(text, h) => Seq(text, h)
      case _ => Seq(ftext)
    }
    else Seq(ftext)
  }
}

object DimLineStyleHandler extends AbstractSettingHandler {
    val name="DimLineStyles"
    final val HideDimensionLine=1 //1
    final val HideHelpLine=2      //2
    final val IsStationLine=4     //3
    final val FixedHelpLine=8     //4
    final val HighMM=16           //5
    final val UnitMM=2
    final val UnitM=0
    final val UnitM_MM=1
    final val DimLineHTextScale=0.75f

    val Match1: Regex ="""(\d+[.,]\d+)(\d)""".r
    val Match2: Regex ="""(\d+)[.,](\d)""".r
    var styleMap:collection.Map[Int,DimLineStyle]=Map.empty
    var defaultStyle:DimLineStyle=_

    def loadSettings(ref:Reference): Unit =  {
      WebSocketConnector.loadChildren(ref,1,data=> {
        styleMap=collection.immutable.SortedMap[Int,DimLineStyle]()++ (for(da<-data) yield {
          val d= new DimLineStyle(da)
          (d.id,d)
        }).toMap
        if(styleMap.size>1)
        defaultStyle=styleMap(1)
      })
    }

    def getStyle(styleID:Int): DimLineStyle =if(styleMap.contains(styleID)) styleMap(styleID) else defaultStyle

    def getStyleSeq: Seq[DimLineStyle] =styleMap.values.toSeq
  }


object Handlers {

  implicit object LineStyleHandler extends AbstractSettingHandler with AbstractLineStyleHandler {
    //println("Start LineStyleHandler")
    val undefinedStyle = new LineStyle(-1, " - ", Array())
    //val hoverStroke=new BasicStroke(3.5f)
    val name = "LineStyles"
    var stylesList: Seq[LineStyle] = Seq.empty
    //r standardStrokes:Seq[BasicStroke]=Seq.empty
    //var folderRef: Option[Reference] = None


    def styles: Seq[LineStyle] = stylesList

    def loadSettings(ref: Reference): Unit = this.synchronized { //Swing.onEDT {
      //folderRef = Some(ref)
      var i: Int = -1
      WebSocketConnector.loadChildren(ref, 1, data => {
        stylesList = data.map(ls => {
          i += 1
          new LineStyle(i, ls)
        })
      })
    }

    override def createStroke(scale: Double, width: Float, styleIx: Int): BasicStroke = null
  }


  implicit object MaterialHandler extends  AbstractSettingHandler with AbstractMaterialHandler {
    val undefinedMaterial=new Material(0," - ",0,0)
    var materialMap=new collection.mutable.LinkedHashMap[Int,Material]()

    override def name: String = "Material"

    override def loadSettings(ref: Reference): Unit = {
      WebSocketConnector.loadChildren(ref,1,data=> for(d<-data)
        materialMap(d.ref.instance)=new Material(d)
      )
    }

    override def getMaterial(inst: Int): Option[Material] = materialMap.get(inst)
  }

  class CompositionStub(data:InstanceData) {
    protected var promise:Promise[Composition]=_
    protected var future:Future[Composition]=_
    def ref: Reference =data.ref
    def name: String =data.fieldValue(1).toString

    def getValue:Future[Composition]=if (future==null){
      promise=Promise[Composition]()
      WebSocketConnector.loadChildren(data.ref,0,layerData=>{
        promise.success(new Composition(data,layerData.map(new ShellLayer(_))))
      })
      future=promise.future
      future
    }else future

    override def toString: String = name
  }

  object undefinedShellLayer extends ShellLayer(MaterialHandler.undefinedMaterial, 0, 0, 0, 0, LineStyleHandler.undefinedStyle, 0,0,BuildingDataModel.greyColor)

  object UndefinedComposition extends Composition(0, " - ", 0, Seq.empty)

  object CompositionHandler extends AbstractSettingHandler {

    val undefinedCompositionStub: CompositionStub =new CompositionStub(null ){
      override def name="nicht definiert"
    }
    protected val compMap: mutable.LinkedHashMap[Int, CompositionStub] =collection.mutable.LinkedHashMap[Int,CompositionStub]()

    override def name: String = "Composition"

    override def loadSettings(ref: Reference): Unit = {
      WebSocketConnector.loadChildren(ref,1,data=> for(d<-data )
        compMap(d.ref.instance)=new CompositionStub(d))
    }

    def compositionExists(inst:Int): Boolean =compMap.contains(inst)

    def getStub(inst:Int): CompositionStub =compMap.getOrElse(inst, undefinedCompositionStub)

    def getComposition(inst:Int):Future[Composition]={
      if(compositionExists(inst)) compMap(inst).getValue
      else Future.failed(new IllegalArgumentException("Wrong composition instance:"+inst))
    }

    def compositionIDs: Iterable[Int] =compMap.keys

    def compositionNames: Iterator[CompositionStub] =compMap.valuesIterator
  }
}