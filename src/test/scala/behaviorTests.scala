/**
  * Created by Nathnael on 3/26/2017.
  */

//import behaviors._

import scala.util.Success
import matryoshka._
import matryoshka.implicits._
import fixtures._
import org.scalacheck.{Prop, Properties}
import org.scalatest.{FunSuite, WordSpec}
//import structures.Location
object behaviorTests extends Properties("behaviorTests")  {


  import scalaz.std.anyVal._
  import structures._
  import behaviors._



  property("size1") = Prop { (fixtures.simpleEllipse cata size) == 1}
  property("size2") = Prop { (fixtures.simpleRectangle cata size) == 1 }
  property("size1") = Prop { (fixtures.simpleLocation cata size) == 1}
  property("size2") = Prop { (fixtures.basicGroup cata size) == 2 }
  property("size1") = Prop { (fixtures.simpleGroup cata size) == 2}
  property("size2") = Prop { (fixtures.complexGroup cata size) == 5 }


  property("height1") = Prop { (fixtures.simpleEllipse cata height) == 1 }
  property("height2") = Prop { (fixtures.simpleRectangle cata height) == 1 }
  property("height1") = Prop { (fixtures.simpleLocation cata height) == 2 }
  property("height2") = Prop { (fixtures.basicGroup cata height) == 2 }
  property("height1") = Prop { (fixtures.simpleGroup cata height) == 3 }
  property("height2") = Prop { (fixtures.complexGroup cata height) == 6 }

  property("rescale1") = Prop { (fixtures.simpleEllipse cata rescale(3)) == Shape.ellipse(150,90) }
  property("rescale2") = Prop { (fixtures.simpleRectangle cata rescale(2)) == Shape.rectangle(160,240) }

  property("rescale3") = Prop { (fixtures.simpleLocation1 cata rescale(2)) == Shape.location(70,30,Shape.ellipse(20,40)) }
  property("rescale4") = Prop { (fixtures.basicGroup cata rescale(2)) == Shape.group(Shape.ellipse(100,60),Shape.rectangle(40,80)) }

  property("rescale3") = Prop { (fixtures.simpleGroup cata rescale(2)) == Shape.group(Shape.location(200,100,Shape.ellipse(100,60)), Shape.location(400 - 50,300 -25,Shape.rectangle(200,100))) }
  property("rescale4") = Prop { (fixtures.complexGroup cata rescale(2)) == fixtures.expectedShape }


  property("boundingBox1") = Prop { (fixtures.simpleEllipse cata boundingBox)== Shape.location(-50,-30,Shape.rectangle(100,60))}
  property("boundingBox2") = Prop { (fixtures.simpleRectangle cata boundingBox) == Shape.location(0,0,Shape.rectangle(80,120)) }
  property("boundingBox3") = Prop { (fixtures.simpleLocation cata boundingBox) == Shape.location(70,30,Shape.rectangle(80,120)) }
  property("boundingBox4") = Prop { (fixtures.basicGroup cata boundingBox) == Shape.location(-50,-30,Shape.rectangle(100,70)) }
  property("boundingBox5") = Prop { (fixtures.simpleGroup cata boundingBox) == Shape.location(150,70,Shape.rectangle(350,280)) }
  property("boundingBox6") = Prop { (fixtures.complexGroup cata boundingBox) == Shape.location(30,60,Shape.rectangle(470,320)) }



}
