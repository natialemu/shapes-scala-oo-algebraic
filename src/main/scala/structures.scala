import scalaz.{Applicative, Equal, Functor, Show, Traverse}
import matryoshka.Delay
import matryoshka.data.Fix


/**
  * Created by Nathnael on 3/24/2017.
  */
object structures {

  /**
    *
    *
    * @tparam A
    */

  sealed trait ShapeF[+A]

  case class Rectangle[A](width: Int, height: Int) extends ShapeF[A]

  case class Location[A](x: Int, y: Int, shape: A) extends ShapeF[A] {
    require(shape != null, "null shape in location")
  }

  case class Ellipse[A](majorAxis: Int, minorAxis: Int) extends ShapeF[A]{
    require(majorAxis >= 0,"invalid value for major axis")
    require(minorAxis >=0,"invalid value for the minor axis")
  }

  case class Circle[A](radius : Int) extends ShapeF[A]{
    require(radius >=0,"invalid value for the radius")
  }

  case class Group[A](shape1: List[A]) extends ShapeF[A]{
    require(shape1 != null,"a group must be composed of shapes")
    require(shape1.size > 1, "list must contain more than 1 shape")

  }

  implicit object shapeFFunctor extends Functor[ShapeF]{
    def map[A,B](fa: ShapeF[A])(f: A => B): ShapeF[B] = fa match{
      case Rectangle(width, height) => Rectangle[B](width,height)
      case Location(x,y,shape)=> Location(x,y,f(shape))
      case Ellipse(majorAxis, minorAxis) => Ellipse[B](majorAxis,minorAxis)
      case Circle(radius)=> Circle[B](radius)
      case Group(shapes)=> Group(shapes.map(f))
    }
  }

  type Shape = Fix[ShapeF]

  object Shape {
    def circle(radius: Int) = Fix[ShapeF](Circle(radius))
    def rectangle(width: Int, height: Int) = Fix[ShapeF](Rectangle(width, height))
    def ellipse(majorAxis: Int, minorAxis: Int) = Fix[ShapeF](Ellipse(majorAxis,minorAxis))
    def location(x: Int, y: Int, shape: Shape) = Fix[ShapeF](Location(x,y,shape))
    def group(shapes: Shape*) = Fix[ShapeF](Group(shapes.toList))
  }


  implicit object shapeFEqualD extends Delay[Equal, ShapeF]{
    def apply[A](a: Equal[A]) = Equal.equalA[ShapeF[A]]
  }

  implicit object shapeFShowD extends Delay[Show, ShapeF]{
    def apply[A](a:Show[A]) = Show.showFromToString[ShapeF[A]]
  }

}

