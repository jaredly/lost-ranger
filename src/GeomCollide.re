
open Geom;

/**
 * Ok folks, I'm gonna really do collision.
 * Turns out support points are where it's at.
 *
 * I've got detection fine, this is all "collision response".
 * You put in:
 * - shapeA
 * - shapeB
 * - velocity from A's perspective
 *
 * You get out
 * - "force" coming back at A
 * - you then would split that up based on masses of the two things
*/

let pointToCircle = (vec, point, {Circle.center, rad}) => {
  let vecToCircle = Geom.vectorBetweenPoints(point, center);
  let vecRelativeToVec = {
    magnitude: vecToCircle.magnitude,
    /* this might be the wrong direction */
    /* theta: vec.theta -. vecToCircle.theta */
    theta: vecToCircle.theta -. vec.theta
  };
  let distToClosestPoint = Geom.vx(vecRelativeToVec);
  let distFromCircleToClosest = Geom.vy(vecRelativeToVec);
  let legToIntersectionPoint = Geom.pythag(rad, distFromCircleToClosest);

  /** This is almost our last deal */
  let amountIntruded = vec.magnitude +. legToIntersectionPoint -. distToClosestPoint;

  let angleBetweenClosestAndIntersectionForCircle = acos(distFromCircleToClosest /. rad);

  let vecIntrudedRelativeToEverything = {
    magnitude: amountIntruded,
    theta: /*Geom.halfPi -. */angleBetweenClosestAndIntersectionForCircle,
  };

  let amountToPushBack = Geom.vy(vecIntrudedRelativeToEverything);
  let angleToPushBack = (Geom.halfPi -. angleBetweenClosestAndIntersectionForCircle) +. vec.theta -. pi;

  {
    magnitude: amountToPushBack,
    theta: angleToPushBack
  }
};

let circleToCircle = (vec, c1, c2) => {
  open Circle;
  pointToCircle(vec, c1.center, {...c2, rad: c2.rad +. c1.rad})
};

/** NOTE this returns magnitude 0 if the vec doesn't intersect the line */
let pointToLine = (vec, point, p1, p2) => {
  let vecOfLine = Geom.vectorBetweenPoints(p1, p2);
  let vecToFirstPoint = Geom.vectorBetweenPoints(point, p1);
  let vecToP1RelativeToVec = {
    magnitude: vecToFirstPoint.magnitude,
    theta: vecToFirstPoint.theta -. vec.theta
  };
  let bd = Geom.vy(vecToP1RelativeToVec);
  /** DANGER ZONE */
  let angleDBC = (vecOfLine.theta -. vec.theta);
  let distFromP1ToIntersection = bd /. cos(angleDBC);
  if (distFromP1ToIntersection > vecOfLine.magnitude) {
    /* Doesn't intersect the line */
    v0
  } else {
    {
      magnitude: Geom.vx(vecToP1RelativeToVec) +. distFromP1ToIntersection *. sin(angleDBC),
      theta: vecOfLine.theta +. halfPi
    }
  }
};

let pointToRect = (vec, point, rect) => {
  Rect.sides(rect)
  |> List.map(((p1, p2)) => pointToLine(vec, point, p1, p2))
  |> List.fold_left((a, b) => (a.magnitude > b.magnitude ? a : b), v0)
};

let circleToRect = (vec, circle, rect) => {
  pointToRect(vec, circle.Circle.center, Rect.addMargin(rect, circle.rad, circle.rad))
};