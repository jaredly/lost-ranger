
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

  let angleDBC = vecOfLine.theta -. vec.theta -. halfPi;

  let distFromP1ToIntersection = -. bd /. cos(angleDBC);

    let distToIntersection = Geom.vx(vecToP1RelativeToVec) -. distFromP1ToIntersection *. sin(angleDBC);

  let didCross = (distFromP1ToIntersection > 0. && distFromP1ToIntersection < vecOfLine.magnitude)
  && (distToIntersection <= vec.magnitude);


      let distFromPenetrationToIntersection = vec.magnitude -. distToIntersection;
      let vecFromIntersectionToPenetrationFromP1Perspective = {
        magnitude: distFromPenetrationToIntersection,
        theta: vec.theta -. vecOfLine.theta,
      };
      ({
        magnitude: Geom.vy(vecFromIntersectionToPenetrationFromP1Perspective),
        theta: vecOfLine.theta -. halfPi
      }, didCross ? distFromPenetrationToIntersection : 0.);
};

let pointToRect = (vec, point, rect) => {
  Rect.sides(rect)
  |> List.map(((p1, p2)) => pointToLine(vec, point, p1, p2))
  |> List.fold_left((a, b) => (snd(a) > snd(b) ? a : b), (v0, 0.))
  |> fst
};

let circleToRect = (vec, circle, rect) => {
  pointToRect(vec, circle.Circle.center,
  /* rect */
  Rect.addMargin(rect, circle.rad, circle.rad)
  )
};

/** Test each point of the first against each line of the other.
 * We're looking for *greatest penetration*, not necessarily
 * greatest impulse response.
 * Not sure if this will play out for any polygon.
*/
let rectToRect = (vec, r1, r2) => {
  Rect.points(r1)
  |> List.map(point => {
    Rect.sides(r2) |> List.map(((p1, p2)) => pointToLine(vec, point, p1, p2))
    /* pointToRect(vec, point, r2) */
  })
  |> List.concat
  |> List.fold_left((a, b) => (snd(a) > snd(b) ? a : b), (v0, 0.))
  |> fst
};

/**
 * New plan :D
 * Longest penetration = "which side to pick"
 * Then longest vector out of there.
 */

let polyToPoly = (vec, p1, p2) => {
  open Polygon;
  p1.vertices
  |> Array.to_list
  |> List.map(point => {
    Polygon.lines(p2) |> List.map(((p1, p2)) => pointToLine(vec, point, p1, p2))
    /* pointToRect(vec, point, r2) */
  })
  |> List.concat
  |> List.fold_left((a, b) => (snd(a) > snd(b) ? a : b), (v0, 0.))
  |> fst
};

let _polyToPoly = (vec, poly1, poly2) => {
  open Polygon;
  Polygon.lines(poly2) |> List.map(((p1, p2)) => {
    poly1.vertices
    |> Array.to_list
    |> List.map(point => {
      /* pointToRect(vec, point, r2) */
      pointToLine(vec, point, p1, p2)
    })
    |> List.fold_left(((a, a1), (b, b1)) => {
      (a.magnitude > b.magnitude ? a : b, max(a1, b1))
    }, (v0, 0.))
  })
  /* |> fail */
  /* |> List.concat */
  |> List.fold_left((a, b) => (snd(a) > snd(b) ? a : b), (v0, 0.))
  |> fst
};