
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    //val validNeighbours = findNeighbours(x,y,radius).filter(withinArea(src.height,src.width,_ ))
    //(validNeighbours.map(point => src.apply(point.x,point.y)).sum)/validNeighbours.size
    val neighbours = findNeighboursIter(src,x,y,radius)
    neighbours.map(point => src.apply(point.x, point.y)).sum/neighbours.size
  }

  def withinArea(height: Int, width: Int, point: Point) : Boolean = {
          point.x >= 0 && point.x < width &&  point.y >= 0 && point.y < height 
  }

  def findNeighbours(x: Int, y: Int, radius: Int): Set[Point] = {
         //(x-1,y),(x-1,y-1),(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1)
         var result = Set(new Point(x,y))
    var counter =  radius
         while(counter > 0){
           result ++=  Set(new Point(x-counter,y), new Point(x-counter,y-counter)
                ,new Point(x-counter,y+counter), new Point(x, y + counter)
                 , new Point(x+counter,y+counter), new Point(x+counter,y)
                ,new Point(x+counter,y-counter), new Point(x,y-counter))
           counter -= 1
         }
      result
  }

  def findNeighboursIter(src:Img, x: Int, y: Int, radius: Int): Set[Point] = {
    var result = Set(new Point(x,y))
    var col = clamp(x-radius, 0 , src.width -1 )
    while(col <= clamp(x+radius,0,src.width -1)){
      var row = clamp(y - radius,0 , src.height -1)
      while(row <= clamp(y+radius,0,src.height -1)){
        result += new Point(col,row);
        row = row + 1
      }
      col = col + 1
    }
    result
  }

  class Point(val x: Int, val y:Int) {
    
    override def toString = s"Point($x, $y)"

    def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

    override def equals(other: Any): Boolean = other match {
      case that: Point =>
        (that canEqual this) &&
          x == that.x &&
          y == that.y
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(x, y)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }
}
