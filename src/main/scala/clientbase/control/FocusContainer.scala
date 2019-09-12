package clientbase.control

import definition.data.Referencable
import definition.expression.Constant

trait FocusContainer {
  protected var onCreatedDataReceived:Option[ ()=>Unit]=None

  def containerName:String

  /** Reference of the current owner object in the container
    *
    */
  def getOwnerRef:Option[Referencable]

  def requestFocus():Unit

  /** notifies the Container that a createAction was started that creates a number of objects in the container
    * so the container can select it
    * @param numCreatedElements how many new elements were created
    */
  def createActionSubmitted(numCreatedElements:Int):Unit = {}

  def hasCreateActionStarted:Boolean=false

  /** stops the CreateActionStarted mode in the container, so that new elements wont get selected
    *
    */
  def resetCreateAction():Unit=
    onCreatedDataReceived=None


  def onCreatedDataReceived(func: () => Unit): Unit = onCreatedDataReceived = Some(func)

  protected def createdDataReceived(): Unit = {
    for (listener<-onCreatedDataReceived) listener()
    resetCreateAction()
  }


  /** Format Field Values to give to a new created Object
   * @param forType class type of the object to create
   * @return list of (formatfieldNr,FieldValue)
   */
  def getCreationFormatValues(forType:Int):Seq[(Int,Constant)]=Nil

  def actionStopped():Unit = {}

  def lostSelection():Unit = {}
}
