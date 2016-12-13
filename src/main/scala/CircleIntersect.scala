import Santa.Position

/**
  * Shamelessly ripped off from StackOverflow
  */
object CircleIntersect {


    val EPSILON = 0.00001

    def calculateThreeCircleIntersection(x0: Double,  y0: Double,  r0: Double,
                                                          x1: Double,  y1: Double,  r1: Double,
                                                          x2: Double,  y2: Double,  r2: Double) : Option[Position] = {

    /* dx and dy are the vertical and horizontal distances between
    * the circle centers.
    */
        var dx = x1 - x0
        var dy = y1 - y0

    /* Determine the straight-line distance between the centers. */
        val d = Math.sqrt((dy*dy) + (dx*dx))

    /* Check for solvability. */
        if (d > (r0 + r1))
        {
        /* no solution. circles do not intersect. */
            return None
        }
        if (d < Math.abs(r0 - r1))
        {
        /* no solution. one circle is contained in the other */
            return None
        }

    /* 'point 2' is the point where the line through the circle
    * intersection points crosses the line between the circle
    * centers.
    */

    /* Determine the distance from point 0 to point 2. */
        val a = ((r0*r0) - (r1*r1) + (d*d)) / (2.0 * d) ;

    /* Determine the coordinates of point 2. */
        val point2_x = x0 + (dx * a/d)
        val point2_y = y0 + (dy * a/d)

    /* Determine the distance from point 2 to either of the
    * intersection points.
    */
        val h = Math.sqrt((r0*r0) - (a*a))

    /* Now determine the offsets of the intersection points from
    * point 2.
    */
        val rx = -dy * (h/d)
        val ry = dx * (h/d)

    /* Determine the absolute intersection points. */
        val intersectionPoint1_x = point2_x + rx
        val intersectionPoint2_x = point2_x - rx
        val intersectionPoint1_y = point2_y + ry
        val intersectionPoint2_y = point2_y - ry

//        println(new Seq[Position](Position(intersectionPoint1_x,intersectionPoint1_y), new Position(intersectionPoint2_x , intersectionPoint2_y)))

    /* Lets determine if circle 3 intersects at either of the above intersection points. */
        dx = intersectionPoint1_x - x2
        dy = intersectionPoint1_y - y2
        val  d1 = Math.sqrt((dy*dy) + (dx*dx))

        dx = intersectionPoint2_x - x2
        dy = intersectionPoint2_y - y2
        val  d2 = Math.sqrt((dy*dy) + (dx*dx))

        if(Math.abs(d1 - r2) < EPSILON) {
            Some(Position(intersectionPoint1_x , intersectionPoint1_y))
        }
        else if(Math.abs(d2 - r2) < EPSILON) {
            Some(Position(intersectionPoint2_x,intersectionPoint2_y))
        }
        else {
            None
        }
    }

}
