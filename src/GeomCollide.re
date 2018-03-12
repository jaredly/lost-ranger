
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

  /* if (compare(legToIntersectionPoint, nan) == 0) {
    print_endline("LEG NAN")
  };
  if (compare(distFromCircleToClosest, nan) == 0) {
    print_endline("DIST CIRCLE NAN")
  }; */

  let angleBetweenClosestAndIntersectionForCircle = acos(distFromCircleToClosest /. rad);

  /* NOTE the nans look like they're coming from
  distFromCircleToClosest being larger than rad */
  /* if (compare(angleBetweenClosestAndIntersectionForCircle, nan) == 0) {
    print_endline("ANGLE NAN" ++ string_of_float(distFromCircleToClosest /. rad))
  }; */

  let vecIntrudedRelativeToEverything = {
    magnitude: amountIntruded,
    theta: /*Geom.halfPi -. */angleBetweenClosestAndIntersectionForCircle,
  };

  let amountToPushBack = Geom.vy(vecIntrudedRelativeToEverything);
  let angleToPushBack = (Geom.halfPi -. angleBetweenClosestAndIntersectionForCircle) +. vec.theta -. pi;

  if (vec.magnitude < amountToPushBack || compare(amountToPushBack, nan) == 0) {
    /* {magnitude: 0.1, theta: 0.} */
    {
      magnitude: rad -. vecToCircle.magnitude,
      theta: vecToCircle.theta +. pi
    }
    /* v0 */
  } else {
    /* if (amountToPushBack > 10.) { */
      /* print_endline(string_of_float(amountToPushBack)); */
    /* }; */
    {
      magnitude: amountToPushBack,
      theta: angleToPushBack
    }
    /* {
      magnitude: rad -. vecToCircle.magnitude,
      theta: vecToCircle.theta +. pi
    } */
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


  let didCross = (distFromP1ToIntersection > 0. && distFromP1ToIntersection < vecOfLine.magnitude);
  /** Remove these bailouts if you want to try non-convex polygon stuff */
  /* if (!didCross) {
    (v0, 0.)
  } else { */
    let distToIntersection = Geom.vx(vecToP1RelativeToVec) -. distFromP1ToIntersection *. sin(angleDBC);

    let didCross = didCross && (distToIntersection <= vec.magnitude);

    /* if (!didCross) {
      (v0, 0.)
    } else { */
      let distFromPenetrationToIntersection = vec.magnitude -. distToIntersection;
      let vecFromIntersectionToPenetrationFromP1Perspective = {
        magnitude: distFromPenetrationToIntersection,
        theta: vec.theta -. vecOfLine.theta,
      };
      ({
        magnitude: Geom.vy(vecFromIntersectionToPenetrationFromP1Perspective),
        theta: vecOfLine.theta -. halfPi
      }, didCross ? distFromPenetrationToIntersection : 0.);
    /* }
  } */
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
 *
 * TODO this results in buggy bhavior.
 * Because if the rect starts collided just a little from the top,
 * and has e.g. velocity to the left, then we push back to the right.
 * Now, how we got into the situation where we started in collision
 * is another question, and might just be due to floating-point
 * rounding errors. Will need to investigate more if I want to use
 * this algorith.
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

/** This was an attempt at non-convex polygons. Didn't work :/ */
let polyToPoly = (vec, poly1, poly2) => {
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