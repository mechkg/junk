import scala.sys.process._
import java.io._
import java.awt.geom._
import scala.xml._

val swfmillCommand = """D:\swfmill\swfmill.exe"""
val sourceDir = new File("""C:\Program Files (x86)\SCRAN24\Images""")

val swfs = sourceDir.listFiles().filter(_.getName.endsWith(".swf")).toSeq

println ("Converting swfs to xml...")

val xmls = swfs.map ( f => {
  val path = f.getAbsolutePath
  val xmlName = f.getName + ".xml"
  val xmlFile = new File (xmlName)

  if (xmlFile.exists) {
    println ("\t" + path + " exists, skipping")
    Some(xmlFile)
  }
  else {
    val cmd = "\"" + swfmillCommand + "\"" + " swf2xml " + "\"" + path + "\" " + "\"" + xmlName + "\""
    val success = cmd.! == 0

    println ("\t" + path + (if (success) " OK" else " FAILED"))
  
    if (success) Some(xmlFile) else None
  }
}).flatten


println ("Processing xmls...")

case class Rect (left: Int, right: Int, top: Int, bottom: Int)

case class ShapeDef (outline: Path2D.Double)

case class Transform (tx: Double, ty: Double, sx: Double, sy: Double) {
  val t: AffineTransform = {
    val t = new AffineTransform()
    t.scale(sx, sy)
    t.translate(tx, ty)
    t
  }
}

case class ButtonDef (hitId: Int, resultId: Int, transform : Transform)

def parseTransform (telem: Node) = {
  val tx = telem.attribute("transX").map(_.text.toDouble).getOrElse(0.0)
  val ty = telem.attribute("transY").map(_.text.toDouble).getOrElse(0.0)
  val sx = telem.attribute("scaleX").map(_.text.toDouble).getOrElse(1.0)
  val sy = telem.attribute("scaleY").map(_.text.toDouble).getOrElse(1.0)
  
  Transform (tx, ty, sx, sy)
}

def parseShape (n: NodeSeq) = {
  val path = new Path2D.Double()

  var cx = 0.0
  var cy = 0.0

  n.foreach { 
    case e @ <ShapeSetup/> => {
      val x = e.attribute("x").map(_.text.toDouble)
      val y = e.attribute("y").map(_.text.toDouble)

      (x, y) match {
	case (Some(x), Some(y)) => { 
	  cx = x
	  cy = y
	  path.moveTo (x, y)
	}
	case _ => { path.moveTo (0.0, 0.0) }
      }
    }
    
    case e @ <LineTo/> => {
      val x = (e \ "@x").text.toDouble + cx
      val y = (e \ "@y").text.toDouble + cy

      path.lineTo (x, y )

      cx = x
      cy = y
    }
    
    case e @ <CurveTo/> => {
      val x0 = cx
      val y0 = cy
      
      val x1 = (e \ "@x1").text.toDouble + cx
      val y1 = (e \ "@y1").text.toDouble + cy
      cx = x1
      cy = y1
      val x2 = (e \ "@x2").text.toDouble + cx
      val y2 = (e \ "@y2").text.toDouble + cy

      // Any quadratic spline can be expressed as a cubic (where the cubic term is zero). 
      // The end points of the cubic will be the same as the quadratic's.

      // CP0 = QP0
      // CP3 = QP2

      // The two control points for the cubic are:

      // CP1 = QP0 + 2/3 *(QP1-QP0)
      // CP2 = QP2 + 2/3 *(QP1-QP2)

      val twothirds = 2.0 / 3.0

      val ccp1x = x0 + twothirds * (x1-x0)
      val ccp1y = y0 + twothirds * (y1-y0)

      val ccp2x = x2 + twothirds * (x1-x2)
      val ccp2y = y2 + twothirds * (y1-y2)

      path.curveTo (ccp1x, ccp1y, ccp2x, ccp2y, x2, y2)

      cx = x2
      cy = y2
    }
    
    case _ => { }
  }
  
  ShapeDef (path)
}

xmls.foreach ( f => {
  print ("\t" + f.getAbsolutePath)

  try {
    val xml = XML.load(new InputStreamReader(new FileInputStream(f), "UTF-8"))

    val shapeDefs = (xml \\ "DefineShape").map ( e => {
      val id = e.attribute("objectID").get.toString.toInt
      (id, parseShape ( (e \\ "edges").head.child ))
    }).toMap
    
    val buttonDefs = (xml \\ "DefineButton2").map ( e => {
      val id = e.attribute("objectID").get.text.toInt
      val btn = (e \\ "Button").find(_.attribute("objectID").isDefined).get
      val result = (e \\ "String")
	.flatMap (_.attribute("value"))
      .map (_.toString)
      .find (_.toList.forall(_.isDigit))
      .get.toInt

      val hitId = btn.attribute("objectID").get.toString.toInt
      val transform = parseTransform ((btn \ "transform" \ "Transform")(0))
      
      (id, ButtonDef (hitId, result, transform))
    }).toList

    val placement = (xml \\ "PlaceObject2").map ( e => {
      val id = e.attribute("objectID").get.text.toInt
      val transform = parseTransform ((e \ "transform" \ "Transform")(0))

      (id, transform)
    }).toMap

    val out = new File (f.getName + ".svg")
    val out2 = new File (f.getName + ".pts")
    val writer = new PrintWriter (new FileOutputStream(out))
    val writer2 = new PrintWriter (new FileOutputStream(out2))

    writer.println ("<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">")

    buttonDefs.foreach { case (id, ButtonDef (hitId, result, transform)) => {
      val ht = transform.t
      val pt = placement(id).t

      pt.concatenate(ht)

      val shape = shapeDefs(hitId)

      val center = new Point2D.Double(shape.outline.getBounds2D().getCenterX(),
				      shape.outline.getBounds2D().getCenterY())

      pt.transform(center, center);

      writer2.println ("new Point (%f, %f),".format (center.getX / 20, center.getY / 20))
     

      writer.print ("<path d=\"")

      val i = shape.outline.getPathIterator (pt, 5.0);

      def pp (i: PathIterator, space: Boolean): Unit = {
	if (!i.isDone()) {
	  val arr = new Array[Double](6)
	  val t = i.currentSegment(arr)

          val tx = arr(0)
	  val ty = arr(1)

	  if (space) writer.print (" ")

	  if (t == PathIterator.SEG_MOVETO) writer.print ("M " + tx / 20 + " " + ty / 20)
	  if (t == PathIterator.SEG_LINETO) writer.print ("L " + tx / 20 + " " + ty / 20)
	  if (t == PathIterator.SEG_CLOSE) writer.print ("z")

	  i.next()
	  pp (i, true)
	}
      }

      pp (i, false)

      writer.println ("\"/>")


    }}

    writer.println ("</svg>")
    writer.close()
    writer2.close()
    println (" OK")
  } catch {
    case e => { println (" FAILED:\n"); e.printStackTrace() }
  }
})
