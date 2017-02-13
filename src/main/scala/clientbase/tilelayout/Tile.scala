package clientbase.tilelayout

import clientbase.control.SidepanelController
import definition.data.{Referencable, Reference}
import org.scalajs.dom.html.{Button, Div, Paragraph}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent, TouchEvent}
import org.scalajs.dom.{document, window}

import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 22.01.2017.
  */

object TileKind extends Enumeration {
  val Single,Horizontal,Vertical=Value
}

/*trait TileHolder{
  def replaceTile(oldTile:Tile,newTile:Tile):Unit
  def deleteTile(tile:Tile):Unit
}*/


trait TileContent{
  def init(selection:Iterable[Referencable]):Unit
  def load():Unit
  def save():Unit
  def content:HTMLElement
  //def debug(st:String):Unit
  def close():Unit
  def getSelection:Iterable[Referencable]
  def updateResize():Unit={}
}

class TestContent(color:String) extends TileContent{

  def init(selection:Iterable[Referencable]):Unit=par.appendChild(p("Selection "+selection.mkString("|")).render)
  lazy val par: Paragraph = p()( "Test "+color).render
  override def load():Unit = {}
  override val content: Div = div(style:="background-color:"+color+";width:100%;height:100%;font-size:0.5em;")(par).render
  override def save():Unit = {}
  //def debug(st:String):Unit = content.appendChild(div(st).render)
  def close():Unit = {}
  def getSelection:Iterable[Referencable]=Seq.empty
}


class Tile(nparentTile:Option[Tile]=None,val isTop:Boolean=false) {
  import Tile._
  var parentTile: Option[Tile] =nparentTile
  var kind:TileKind.Value=TileKind.Single
  var outerEdgeRight=false
  var outerEdgeBottom=false
  var maximized=false
  var firstTile:Option[Tile]=None
  var secondTile:Option[Tile]=None
  var dragArea:Option[Div]=None
  var singleContent:Option[TileContent]=None
  var proportion=0.5d
  val myDiv: Div = div(`class`:=StyleSingleContent).render
  val rightButtonGroup:Div = div(`class`:="tile-button-group-right").render
  lazy val slideRight: EdgeSensorRight =new EdgeSensorRight("tile-slide-right",true,
    ()=>{open(createRandomContent,TileKind.Horizontal)},()=>for(p<-getParentTile)
      if(p.kind==TileKind.Horizontal)p.closeChild(this))
  lazy val slideBottom: EdgeSensorRight =new EdgeSensorRight("tile-slide-bottom",false,
    ()=>{open(createRandomContent,TileKind.Vertical)},()=>for(p<-getParentTile)
      if(p.kind==TileKind.Vertical)p.closeChild(this))
  val rightButtons: List[Button] =Tile.factoryList map{
    case ContentFactory(symbol,description,callBack) => button(`title`:=description,`class`:=StyleTRButton,onclick:={
    ()=>open(callBack(),TileKind.Horizontal)})(symbol).render}

  val closeButton:Button = button(`class`:=StyleTRButton,onclick:=closeMe _)("X").render
  val wideButton:Button=button(`class`:=StyleTRButton,onclick:={
    ()=>if(maximized)recoverThis() else maximizeThis()})("<>").render
  val bottomButtonGroup: Div = div(`class`:="tile-button-group-bottom").render
  val bottomButtons: List[Button] =Tile.factoryList map{
    case ContentFactory(symbol,description,callBack) =>
      button(`title`:=description,`class`:=StyleBLButton,onclick:={ ()=>
       {open(callBack(),TileKind.Vertical)}})(symbol).render}
  rightButtonGroup.appendChild(closeButton)
  rightButtonGroup.appendChild(wideButton)
  myDiv.appendChild(rightButtonGroup)
  myDiv.appendChild(bottomButtonGroup)
  //myDiv.appendChild(slideRight)

  def closeMe():Unit={
    for(p<-getParentTile)p.closeChild(this)
    notifyResize()
  }

  def getParentTile=parentTile

  def clearContent():Unit= {
    //println("clear")
    while (myDiv.hasChildNodes())
      myDiv.removeChild(myDiv.firstChild)
  }

  def setEdgeRight(trVisible:Boolean):Unit={
    //println("Set Edge Right "+trVisible+" tile: "+this+" old:"+outerEdgeRight+" cont:"+myDiv.contains(slideRight))
    outerEdgeRight=trVisible
    /*buttonGroup.style.setProperty("display",if(groupVisible) "initial" else "none")*/
    if(trVisible) {
      for(b<-rightButtons)
      rightButtonGroup.appendChild(b)
      myDiv.appendChild(slideRight.myDiv)
    }
    else {
      for(b<-rightButtons)
      if(rightButtonGroup.contains(b)) rightButtonGroup.removeChild(b)
      if(myDiv.contains(slideRight.myDiv)) myDiv.removeChild(slideRight.myDiv)
    }
  }

  def setEdgeBottom(visible:Boolean):Unit={
    outerEdgeBottom=visible
    if(visible) {
      for(b<-bottomButtons) bottomButtonGroup.appendChild(b)
      myDiv.appendChild(slideBottom.myDiv)
    }
    else {
      for(b<-bottomButtons) if(bottomButtonGroup.contains(b)) bottomButtonGroup.removeChild(b)
      if(myDiv.contains(slideBottom.myDiv)) myDiv.removeChild(slideBottom.myDiv)
    }
    //blButton.style.setProperty("display",if(visible)"initial" else "none")
  }

  def setSingleContent(ncontent:TileContent):Unit = {
    singleContent=Some(ncontent)
    myDiv.appendChild(ncontent.content)
  }

  def findRoot():Tile = parentTile match {
    case None => this
    case Some(parent)=> parent.findRoot()
  }

  def maximizeThis():Unit = if(!maximized){
    maximized=true
    maxWidthStyle=myDiv.style.getPropertyValue("width")
    maxHeightStyle=myDiv.style.getPropertyValue("height")
    myDiv.style.setProperty("padding","4px 1px 1px 4px")
    myDiv.style.setProperty("width","calc( 100% - 1px)")
    myDiv.style.setProperty("height","calc( 100% - 5px)")
    myDiv.removeChild(bottomButtonGroup)
    findRoot().prepareRootForMax(this)
    rightButtonGroup.removeChild(closeButton)
    for(b<-rightButtons) if(rightButtonGroup.contains(b)) rightButtonGroup.removeChild(b)
    notifyResize()
  }

  def recoverThis():Unit= if(maximized){
    maximized=false
    findRoot().recoverRootFromMax()
    myDiv.style.setProperty("padding","4px 30px 30px 4px")
    myDiv.style.setProperty("width",maxWidthStyle)
    myDiv.style.setProperty("height",maxHeightStyle)
    myDiv.appendChild(bottomButtonGroup)
    rightButtonGroup.removeChild(wideButton)
    rightButtonGroup.appendChild(closeButton)
    rightButtonGroup.appendChild(wideButton)
    setEdgeRight(outerEdgeRight)
    notifyResize()
  }

  def prepareRootForMax(tile:Tile):Unit = {
    maxRootParentElement=myDiv.parentElement
    maxDivParentElement=tile.myDiv.parentElement
    maxRootParentElement.removeChild(myDiv)
    maxRootParentElement.appendChild(tile.myDiv)
  }

  def recoverRootFromMax():Unit= {
    if(maxRootParentElement!=null) {
      val childDiv=maxRootParentElement.children.item(0)
      maxRootParentElement.appendChild(myDiv)
      maxDivParentElement.appendChild(childDiv)
      maxRootParentElement=null
    }
  }


  def setupDragListeners():Unit= for(drag<-dragArea){
    if(kind==TileKind.Horizontal){
      drag.addEventListener("mousedown",horizontalDrag)
      drag.addEventListener("touchstart",horizontalTouchDrag)
    }
    else {
      drag.addEventListener("mousedown",verticalDrag)
      drag.addEventListener("touchstart",verticalTouchDrag)
    }
  }

  def open(newContent:TileContent,nkind:TileKind.Value): Unit = if(kind==TileKind.Single) {
    clearContent()
    kind=nkind
    val ft=new Tile(Some(this))
    //println("Tile Open "+singleContent)
    for(sc<-singleContent){
      ft.setSingleContent(sc)
      newContent.init(sc.getSelection)
    }

    if(nkind==TileKind.Horizontal)
      ft.setEdgeBottom(outerEdgeBottom)
    else ft.setEdgeRight(trVisible = outerEdgeRight)
    val drag=div(`class`:=(
      if(nkind==TileKind.Horizontal)Tile.StyleHDrag else Tile.StyleVDrag)).render

    myDiv.appendChild(drag)
    dragArea=Some(drag)
    setupDragListeners()
    myDiv.appendChild(ft.myDiv)
    firstTile=Some(ft)
    val st=new Tile(Some(this))
    st.setSingleContent(newContent)
    st.setEdgeRight(trVisible = outerEdgeRight)
    st.setEdgeBottom(outerEdgeBottom)
    myDiv.appendChild(st.myDiv)
    secondTile=Some(st)
    singleContent=None
    //setEdgeRight(groupVisible = false,trVisible = false)
    //setEdgeBottom(false)
    updateLayout()
    notifyResize()
    //println("open done "+this+" ft:"+ft+" st:"+st+" r:"+outerEdgeRight+" b:"+outerEdgeBottom)
  }

  protected def takeOver(tile:Tile,fromFirst:Boolean,otherTile:Tile):Unit= {
      kind=tile.kind
      if(tile.kind==TileKind.Single)  {
        for(sc <- tile.singleContent) setSingleContent(sc)
        firstTile = None
        secondTile = None
        dragArea = None
        //println("take over single r:"+outerEdgeRight+" b:"+outerEdgeBottom)
        myDiv.appendChild(rightButtonGroup)
        myDiv.appendChild(bottomButtonGroup)
        setEdgeRight(outerEdgeRight)
        setEdgeBottom(outerEdgeBottom)
        proportion=0.5
      }
      else {
        tile.detachListeners()
        for(cft<-tile.firstTile){
          myDiv.appendChild(cft.myDiv)
          cft.parentTile=Some(this)
        }
        myDiv.appendChild(tile.dragArea.get)
        for(cst<-tile.secondTile){
          myDiv.appendChild(cst.myDiv)
          cst.parentTile=Some(this)
        }
        firstTile=tile.firstTile
        secondTile=tile.secondTile
        dragArea=tile.dragArea
        setupDragListeners()
        if(!tile.outerEdgeBottom && outerEdgeBottom) Tile.setEdgeBottom(this)
        if(!tile.outerEdgeRight && outerEdgeRight) Tile.setEdgeRight(this)
      }
  }



  def closeChild(childTile:Tile):Unit =
    if (childTile.kind == TileKind.Single) {
      for(s<-childTile.singleContent) s.close()
      clearContent()
      //println("close this:"+this+" child:"+childTile)

      if (firstTile.isDefined && firstTile.get == childTile) {
        //println("Close first st:"+secondTile.get+" kind:"+secondTile.get.kind)
        for (st <- secondTile;ft<-firstTile) takeOver(st,fromFirst = false,ft)
      } else if (secondTile.isDefined && secondTile.get == childTile) {
        //println("close second ft:"+firstTile.get+" kind:"+firstTile.get.kind)
        for (ft <- firstTile;st<-secondTile) takeOver(ft,fromFirst = true,st)
      } else println("close, Unknown child " + childTile+" ft:"+firstTile+" st:"+secondTile)

    } else println("close but not single " + childTile.kind)


  lazy val verticalDrag:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=> for (drag<-dragArea) {
      val parentH=myDiv.scrollHeight
      val parentY=myDiv.getBoundingClientRect().top
      val delta=e.clientY-drag.offsetTop-parentY
      e.stopPropagation()
      e.preventDefault()

      val moveFunc: scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=> {
        val pos = 26+Math.min(parentH - 40, Math.max(20, e.pageY - parentY))
        proportion = (pos - delta + 6) / parentH
        updateLayout()
        e.stopPropagation()
        e.preventDefault()
      }
      document.addEventListener("mousemove",moveFunc,useCapture = false)

      lazy val remove: scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=>{
        document.removeEventListener("mousemove",moveFunc,useCapture = false)
        document.removeEventListener("mouseup",remove,useCapture = false)
        notifyResize()
      }

      document.addEventListener("mouseup",remove,useCapture = false)
    }

  lazy val verticalTouchDrag:scala.scalajs.js.Function1[TouchEvent, Unit]=(e:TouchEvent)=> for (drag<-dragArea) {
    //myDiv.appendChild(p("touch "+e.touches.length).render)
    if (e.touches.length == 1) {
      val parentH = myDiv.scrollHeight
      val parentY = myDiv.getBoundingClientRect().top
      val delta = e.touches.item(0).clientY - drag.offsetTop - parentY

      e.stopPropagation()
      e.preventDefault()

      val moveFunc: scala.scalajs.js.Function1[TouchEvent, Unit] = (e: TouchEvent) => if (e.touches.length == 1) {
        val pos = 26+Math.min(parentH - 40, Math.max(20, e.touches.item(0).pageY - parentY))
        //firstTile.get.singleContent.get.debug("vertical touch " + pos)
        proportion = (pos - delta + 6) / parentH
        updateLayout()
        e.stopPropagation()
        e.preventDefault()
      }
      document.addEventListener("touchmove", moveFunc, useCapture = false)

      lazy val remove: scala.scalajs.js.Function1[MouseEvent, Unit] = (e: MouseEvent) => {
        document.removeEventListener("touchmove", moveFunc, useCapture = false)
        document.removeEventListener("touchend", remove, useCapture = false)
        document.removeEventListener("touchcancel", remove, useCapture = false)
        notifyResize()
      }

      document.addEventListener("touchend", remove, useCapture = false)
      document.addEventListener("touchcancel",remove,useCapture = false)
    }
  }


  lazy val horizontalDrag:scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=> for (drag<-dragArea){
    val parentW=myDiv.scrollWidth
    val parentX=myDiv.getBoundingClientRect().left
    val delta=e.clientX-drag.offsetLeft-parentX
    e.stopPropagation()
    e.preventDefault()

    val moveFunc: scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=> {
      val pos = 26+Math.min(parentW - 40, Math.max(20, e.pageX - parentX))
      proportion = (pos - delta + 6) / parentW
      updateLayout()
      e.stopPropagation()
      e.preventDefault()
    }
    document.addEventListener("mousemove",moveFunc,useCapture = false)

    lazy val remove: scala.scalajs.js.Function1[MouseEvent, Unit]=(e:MouseEvent)=>{
      document.removeEventListener("mousemove",moveFunc,useCapture = false)
      document.removeEventListener("mouseup",remove,useCapture = false)
      notifyResize()
    }

    document.addEventListener("mouseup",remove,useCapture = false)
  }

  lazy val horizontalTouchDrag:scala.scalajs.js.Function1[TouchEvent, Unit]=(e:TouchEvent)=> for (drag<-dragArea)
    if(e.touches.length==1){
    val parentW=myDiv.scrollWidth
    val parentX=myDiv.getBoundingClientRect().left
    val delta=e.touches.item(0).clientX-drag.offsetLeft-parentX
    e.stopPropagation()
    e.preventDefault()

    val moveFunc: scala.scalajs.js.Function1[TouchEvent, Unit]=(e:TouchEvent)=> {
      val pos = 26+Math.min(parentW - 40, Math.max(20, e.touches.item(0).pageX - parentX))
      proportion = (pos - delta + 6) / parentW
      updateLayout()
      e.stopPropagation()
      e.preventDefault()
    }
    document.addEventListener("touchmove",moveFunc,useCapture = false)

    lazy val remove: scala.scalajs.js.Function1[TouchEvent, Unit]=(e:TouchEvent)=>{
      document.removeEventListener("touchcancel",remove,useCapture = false)
      document.removeEventListener("touchmove",moveFunc,useCapture = false)
      document.removeEventListener("touchend",remove,useCapture = false)
      notifyResize()
    }

    document.addEventListener("touchend",remove,useCapture = false)
    document.addEventListener("touchcancel",remove,useCapture = false)
  }

  def detachListeners():Unit = for (drag<-dragArea){
    if(kind==TileKind.Horizontal) {
      drag.removeEventListener("mousedown",horizontalDrag)
      drag.removeEventListener("touchstart",horizontalTouchDrag)
    }
    else {
      drag.removeEventListener("mousedown",verticalDrag)
      drag.removeEventListener("touchStart",verticalTouchDrag)
    }
  }




  def updateLayout(): Unit ={
    //println("update kind: "+kind+" firstTile:"+firstTile+" secondTile:"+secondTile)
    kind match {
      case TileKind.Single=>
      case TileKind.Horizontal=>
        for(ft<-firstTile){
          setStyleClass(ft.myDiv,StyleLeft)
          ft.myDiv.style.setProperty("width","calc("+(proportion*100d)+"% - 64px)")
        }
        for(st<-secondTile) {
          setStyleClass(st.myDiv,StyleRight)
          st.myDiv.style.setProperty("width","calc("+((1-proportion)*100d)+"% - "+(4+dragWidth)+"px)")
        }
        for(da<-dragArea)
          da.style.setProperty("left","calc("+proportion*100d+"% - 30px)")
      case TileKind.Vertical=>
        for(ft<-firstTile){
          setStyleClass(ft.myDiv,StyleTop)
          ft.myDiv.style.setProperty("height","calc("+(proportion*100d)+"% - 64px)")
        }
        for(st<-secondTile) {
          setStyleClass(st.myDiv,StyleBottom)
          st.myDiv.style.setProperty("height","calc("+((1-proportion)*100d)+"% - "+(4+dragWidth)+"px)")
        }
        for(da<-dragArea)
          da.style.setProperty("top","calc("+proportion*100d+"% - 30px)")
    }
  }

  def notifyChildrenResize():Unit= kind match {
    case TileKind.Single => for(s<-singleContent) s.updateResize()
    case TileKind.Horizontal|TileKind.Vertical=>
      for(f<-firstTile)f.notifyChildrenResize()
      for(f<-secondTile)f.notifyChildrenResize()
  }

  def notifyResize():Unit = findRoot().notifyChildrenResize()

}



class EdgeSensorRight(cssClass:String,hor:Boolean,funcIn:()=>Unit,funcOut:()=>Unit) {
  val myDiv: Div =div(`class`:=cssClass).render
  var startx:Double= -1d
  var startx2:Double= -1d


  lazy val touchmove : scala.scalajs.js.Function1[TouchEvent, Unit]=(e:TouchEvent)=>
    if(startx> -1 && e.touches.length==2&&e.touches.item(0).target==myDiv&&e.touches.item(1).target==myDiv) {
      ///myDiv.appendChild(p("1x:"+e.touches.item(0).clientX+" 2x:"+e.touches.item(1).clientX).render)
      if ( (hor&&(e.touches.item(0).clientX< (startx-10)||e.touches.item(1).clientX< (startx2-10) )) ||
        (!hor&&(e.touches.item(0).clientY< (startx-10)||e.touches.item(1).clientY< (startx2-10)) ) )  {
      myDiv.removeEventListener("touchmove",touchmove)
        myDiv.parentElement.appendChild(p("open").render)
      funcIn()
      e.preventDefault()
      e.stopPropagation()
      startx= -1
      startx2= -1
    }
    if ( (hor&&(e.touches.item(0).clientX> (startx+10) ||e.touches.item(1).clientX> (startx2+10) )) ||
        (!hor&&(e.touches.item(0).clientY> (startx+10) ||e.touches.item(1).clientY> (startx2+10) ))  )  {
        myDiv.removeEventListener("touchmove",touchmove)
        funcOut()
        myDiv.parentElement.appendChild(p("close").render)
        e.preventDefault()
        e.stopPropagation()
        startx= -1
        startx2= -1
      }
  }

  myDiv.addEventListener("touchstart",(e:TouchEvent)=>{
    startx= -1
    startx2= -1
    if(e.touches.length==2) {
      val firstItem=e.touches.item(0)
      val secondItem=e.touches.item(1)
      if(firstItem.target==myDiv && secondItem.target==myDiv) {
        startx = if (hor) firstItem.clientX else firstItem.clientY
        startx2 = if (hor) secondItem.clientX else secondItem.clientY
        myDiv.addEventListener("touchmove", touchmove)
        //myDiv.appendChild(p("1:"+startx+" 2:"+startx2).render)
        e.preventDefault()
        e.stopImmediatePropagation()
      }
    }
  })

}

case class ContentFactory(ssymbol:String,description:String,factory:()=>TileContent)


object Tile {
  val StyleSingleContent="tile-single-content"
  val StyleLeft="tile-left"
  val StyleRight="tile-right"
  val StyleTop="tile-top"
  val StyleBottom="tile-bottom"
  val StyleVDrag="tile-vdrag"
  val StyleHDrag="tile-hdrag"
  val StyleTRButton="tile-tr-button"
  val StyleBLButton="tile-bl-button"
  var maxRootParentElement:HTMLElement=null
  var maxDivParentElement:HTMLElement=null
  var maxWidthStyle:String=""
  var maxHeightStyle:String=""

  var factoryList:List[ContentFactory]=Nil

  val dragWidth: Int =if(window.screen.width<1100) 15 else 8

  val colors=Array("blue","white","green","red","yellow","cyan","fuchsia","grey",
    "lightgrey","greenyellow")

  def setStyleClass(el:HTMLElement,newStyleClass:String):Unit={
    val classes=el.classList
    if(!classes.contains(newStyleClass)){
      while(classes.length>0) classes.remove(classes.item(0))
      classes.add(newStyleClass)
    }
  }

  protected def setEdgeBottom(tile:Tile):Unit ={
    //println("Set Edge Bottom "+tile)
    tile.kind match {
      case TileKind.Single => tile.setEdgeBottom(true)
      case TileKind.Horizontal =>
        for(ft<-tile.firstTile) setEdgeBottom(ft)
        for(st<-tile.secondTile) setEdgeBottom(st)
      case TileKind.Vertical=>  for(st<-tile.secondTile) setEdgeBottom(st)
    }
  }

  protected def setEdgeRight(tile:Tile):Unit ={
    //println("set Edge Right "+tile)
    tile.kind match {
      case TileKind.Single => tile.setEdgeRight(true)
      case TileKind.Horizontal =>
        for(st<-tile.secondTile) setEdgeRight(st)
      case TileKind.Vertical=>
        for(ft<-tile.firstTile) setEdgeRight(ft)
        for(st<-tile.secondTile) setEdgeRight(st)
    }
  }
  def createRandomContent=new TestContent(colors(Math.floor(Math.random()*10).toInt))
}