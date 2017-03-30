
import matryoshka.Algebra

/**
  * Created by Nathnael on 3/24/2017.
  */
object behaviors {

  import structures._

  val size: Algebra[ShapeF, Int] = {
    case Circle(c) => 1
    case Rectangle(width, height) => 1
    case Ellipse(majorAxis, minorAxis) => 1
    case Location(x, y, r) => r
    case Group(shapes) => sum(shapes)
  }

  def sum(shape: Seq[Int]): Int = {
    return shape.foldLeft {
      (0)
    } { case ((sum), next) =>
      sum + next
    }
  }

  def maxHeight(shape: Seq[Int]): Int = {
    return shape.foldLeft {
      (0)
    } { case ((maximum), next) =>
      math.max(maximum, next)
    }
  }


  val height: Algebra[ShapeF, Int] = {
    case Circle(c) => 1
    case Rectangle(width, height) => 1
    case Ellipse(majorAxis, minorAxis) => 1
    case Location(x, y, shape) => 1 + shape
    case Group(shapes) => 1 + maxHeight(shapes)


  }

  def rescale(factor: Int): Algebra[ShapeF, Shape] = {
    case Circle(c) => Shape.circle(c)
    case Rectangle(width, height) => Shape.rectangle(width * factor, height * factor)
    case Ellipse(majorAxis, minorAxis) => Shape.ellipse(majorAxis * factor, minorAxis * factor)
    case Location(x, y, shape) => shape.unFix match {
      case Circle(c) => Shape.location(x, y, Shape.circle(c * factor))
      case Rectangle(width,height) => Shape.location(x - (width / 4) * (factor - 1), y - (height / 4) * (factor - 1), Shape.rectangle(width/2 * factor, height/2 * factor))
      case Ellipse(majorAxis, minorAxis) => Shape.location(x, y, Shape.ellipse(majorAxis * factor/2, minorAxis*factor/2))
      case Group(shapes) => {
        val listOfNewShapes = shapes.foldLeft(List[Shape]()){ case (acc,next) =>
          next :: acc
        }
        Shape.location(x,y,Shape.group(listOfNewShapes.reverse:_*))
      }
      case Location(_,_,_) => Shape.location(x,y,shape)
    }
    case Group(shape1) => {
      val listOfNewShapes = shape1.foldLeft(List[Shape]()){ case (acc,next) =>
          next :: acc
      }
      Shape.group(listOfNewShapes.reverse:_*)
    }
  }

  val boundingBox: Algebra[ShapeF, Shape] = {
    case Location(x, y, shape) => Shape.location(x + (shape.unFix).asInstanceOf[Location[_]].x, y + (shape.unFix).asInstanceOf[Location[_]].y, (shape.unFix).asInstanceOf[Location[_]].shape.asInstanceOf[Shape])
    case Rectangle(width, height) => Shape.location(0, 0, Shape.rectangle(width, height))
    case Ellipse(majorAxis, minorAxis) => Shape.location(-majorAxis, -minorAxis, Shape.rectangle(majorAxis * 2, minorAxis * 2))
    case Circle(radius) => Shape.location(-radius, -radius, Shape.rectangle(2 * radius, 2 * radius))
    case Group(shapes) => {
      val (minx, miny) = shapes.foldLeft(10000, 1000) { case ((minimumx, minimumy), next) =>
        (math.min(minimumx, next.unFix.asInstanceOf[Location[_]].x), math.min(minimumy, next.unFix.asInstanceOf[Location[_]].y))
      }
      val (maxx, maxy) = shapes.foldLeft(0, 0) { case ((maximumx, maximumy), next) =>
        val shapeRectangele1 = (next.unFix).asInstanceOf[Location[_]].shape.asInstanceOf[Shape]
        val shapeRectangele = shapeRectangele1.unFix.asInstanceOf[Rectangle[_]]
        (math.max(maximumx, next.unFix.asInstanceOf[Location[_]].x + shapeRectangele.width), math.max(maximumy, next.unFix.asInstanceOf[Location[_]].y + shapeRectangele.height))
      }
      Shape.location(minx, miny, Shape.rectangle(maxx - minx, maxy - miny))
    }
  }
}

