open Reprocessing;
open Geom;

let range = x => {
  let rec loop = i => {
    i >= x ? [] : [i, ...loop(i + 1)]
  };
  loop(0)
};

let mousePos = env => Geom.fromIntTuple(Env.mouse(env));

let withAlpha = (alpha, color) => {...color, Reprocessing_Common.a: alpha};

let drawResponse = (moving, target, vel, env) => {
  let moved = Geom.Shape.push(moving, vel);
  Draw.stroke(withAlpha(0.1, Constants.blue), env);
  GeomDraw.vec(Geom.Shape.center(moving), vel, env);
  Draw.noStroke(env);
  if (Geom.Shape.testShapes(moved, target)) {
    Draw.fill(withAlpha(0.1, Constants.red), env);
    GeomDraw.shape(moved, env);
    Draw.noFill(env);
    let response =  Geom.Shape.collideToShape(vel, moved, target);
    Draw.stroke(Constants.blue, env);
    GeomDraw.vec(Geom.Shape.center(moved), response, env);
    let final = Geom.Shape.push(moved, response);
    GeomDraw.shape(final, env);
    Draw.noStroke(env);
  };
};

let angleRing = count => range(count)
  |> List.map(i => 3.14159 *. 2. /. float_of_int(count) *. float_of_int(i));

let allShapes = (env) => {
  Draw.noStroke(env);

  let circle = {Geom.Circle.center: mousePos(env), rad: 20.};
  let rect = Geom.Rect.create(mousePos(env), 20., 20.);

  let size = 100.;
  angleRing(3) |> List.iter2((shape, theta) => {
    Draw.fill(withAlpha(0.2, Constants.green), env);
    let shape = Geom.Shape.push(shape, {Geom.magnitude: 200., theta});
    let shape = Geom.Shape.translate(shape, {Geom.x: 400., y: 400.});
    GeomDraw.shape(shape, env);

    angleRing(20) |> List.iter(theta => drawResponse(
      /* Geom.Shape.aabb(rect |> Geom.Rect.aabb), */
      /* Geom.Shape.rect(rect), */
      Geom.Shape.circle(circle),
      shape,
      {Geom.magnitude: 100., theta},
      env
    ));
  }, Geom.Shape.([
    circle({Circle.center: Geom.origin, rad: size /. 2.}),
    rect(Rect.create(Geom.origin, size, size)),
    aabb(Rect.create(Geom.origin, size, size) |> Rect.aabb)
  ]));

};


let regularPoly = (center, sides, size) => {
  Geom.Polygon.fromVertices(
    angleRing(sides) |> Array.of_list |> Array.map(theta => (
      Geom.addVectorToPoint({magnitude: size, theta}, center)
    ))
  )
};

let polyPolyResponse = (env) => {

  let mainPoly = regularPoly(mousePos(env), 5, 30.);

  /* This was to test non-convex polygons... it didn't work :( */
  /* let mainPoly = Geom.Polygon.fromVertices(
    [|
    {x: 0.,  y: 20.},
    {x: 40., y: 0.},
    {x: 10., y: 40.},
    {x: 20., y: 20.}
    |]
    /* angleRing(4) |> Array.of_list |> Array.map(theta => (
      Geom.addVectorToPoint({magnitude: 20. +. cos(theta *. 2.) *. 10., theta}, mousePos(env))
    )) */
  ) |> x => Geom.Polygon.translate(x, mousePos(env)); */


  let poly = regularPoly({x: 400., y: 400.}, 8, 100.);
  /* let poly = Geom.Polygon.fromVertices(
    angleRing(8) |> Array.of_list |> Array.map(theta => (
      Geom.addVectorToPoint({magnitude: 100. +. cos(theta *. 2.) *. 50., theta}, {x: 400., y: 400.})
    ))
  ); */

  Draw.stroke(withAlpha(0.5, Constants.green), env);
  GeomDraw.polygon(poly, env);
  Draw.noFill(env);
  Draw.stroke(withAlpha(0.2, Constants.black), env);

  let vel = {Geom.magnitude: 150., theta: 0.};

  angleRing(7) |> List.iter(theta => {
    let vel = {Geom.magnitude: 150., theta};

    let moved = Polygon.push(mainPoly, vel);
    Draw.stroke(withAlpha(0.3, Constants.blue), env);
    GeomDraw.vec(Polygon.center(mainPoly), vel, env);

    mainPoly.Polygon.vertices |> Array.iter(p => {
      GeomDraw.vec(p, vel, env)
    });

    Draw.stroke(withAlpha(0.1, Constants.red), env);
    GeomDraw.polygon(moved, env);

    if (Geom.Polygon.testPolygon(moved, poly)) {
      let response =  GeomCollide.polyToPoly(vel, mainPoly, poly);
      Draw.stroke(Constants.blue, env);
      GeomDraw.vec(Polygon.center(moved), response, env);
      let final = Polygon.push(moved, response);
      GeomDraw.polygon(final, env);
      Draw.noStroke(env);
    };
  });

};



let rectRectResponse = (env) => {
  let mainRect = Geom.Rect.create(mousePos(env), 40., 40.);

  let rect = Geom.Rect.create({Geom.x: 400., y: 400.}, 200., 200.);
  Draw.fill(withAlpha(0.1, Constants.green), env);
  GeomDraw.rect(rect, env);
  Draw.noFill(env);
  Draw.stroke(withAlpha(0.2, Constants.black), env);
  /* GeomDraw.rect(Rect.addMargin(rect, circle.rad, circle.rad), env); */

  angleRing(10) |> List.iter(theta => {
    let vel = {Geom.magnitude: 150., theta};

    let moved = Rect.push(mainRect, vel);
    Draw.stroke(withAlpha(0.3, Constants.blue), env);
    GeomDraw.vec(mainRect.Rect.pos, vel, env);

    Rect.points(mainRect) |> List.iter(p => {
      GeomDraw.vec(p, vel, env)
    });

    Draw.fill(withAlpha(0.1, Constants.red), env);
    GeomDraw.rect(moved, env);

    Draw.noStroke(env);
    if (Geom.Rect.testRect(rect, moved)) {
      Draw.noFill(env);
      let response =  GeomCollide.rectToRect(vel, mainRect, rect);
      Draw.stroke(Constants.blue, env);
      GeomDraw.vec(moved.Rect.pos, response, env);
      let final = Rect.push(moved, response);
      GeomDraw.rect(final, env);
      Draw.noStroke(env);
    };
  });

};

let circleRectResponse = (env) => {
  let circle = {Circle.center: mousePos(env), rad: 20.};

  let rect = Geom.Rect.create({Geom.x: 400., y: 400.}, 200., 200.);
  Draw.fill(withAlpha(0.1, Constants.green), env);
  GeomDraw.rect(rect, env);
  Draw.noFill(env);
  Draw.stroke(withAlpha(0.2, Constants.black), env);
  GeomDraw.rect(Rect.addMargin(rect, circle.rad, circle.rad), env);

  angleRing(20) |> List.iter(theta => {
    let vel = {Geom.magnitude: 150., theta};

    let moved = Circle.push(circle, vel);
    Draw.stroke(withAlpha(0.3, Constants.blue), env);
    GeomDraw.vec(circle.Circle.center, vel, env);

    Draw.fill(withAlpha(0.1, Constants.red), env);
    GeomDraw.circle(moved, env);

    Draw.noStroke(env);
    if (Geom.Rect.testCircle(rect, moved)) {
      Draw.noFill(env);
      let response =  GeomCollide.circleToRect(vel, circle, rect);
      Draw.stroke(Constants.blue, env);
      GeomDraw.vec(moved.Circle.center, response, env);
      let final = Circle.push(moved, response);
      GeomDraw.circle(final, env);
      Draw.noStroke(env);
    };
  });

};

let pointRectResponse = (env) => {
  let point = mousePos(env);
  let rect = Geom.Rect.create({Geom.x: 400., y: 400.}, 200., 200.);

  let waggle = float_of_int(Reprocessing.Env.frameCount(env)) /. 50.;
  /* let waggle = 1.; */
  let vec = {Geom.magnitude: 150., theta: waggle /. 2.};

  Draw.noFill(env);
  Draw.strokeWeight(3, env);
  Draw.stroke(withAlpha(0.2, Constants.black), env);

  GeomDraw.rect(rect, env);
  GeomDraw.vec(point, vec, env);

  let response = GeomCollide.pointToRect(vec, point, rect);
  GeomDraw.vec(Geom.addVectorToPoint(vec, point), response, env);

  Rect.sides(rect) |> List.iter(((p1, p2)) => {
    let response = GeomCollide.pointToLine(vec, point, p1, p2) |> fst;
    Draw.stroke(
      response.magnitude > 0. ? Constants.green : Constants.red
    , env);
    /* GeomDraw.vec(Geom.addVectorToPoint(vec, point), response, env); */
    GeomDraw.vec(p1, response, env);
  });

};

let pointLineResponse = (env) => {
  let point = mousePos(env);
  /* let rect = Geom.Rect.create({Geom.x: 400., y: 400.}, 100., 100.); */

  Draw.noFill(env);
  Draw.strokeWeight(3, env);
  Draw.stroke(withAlpha(0.2, Constants.black), env);
  GeomDraw.point(point, env);

  /* GeomDraw.rect(rect, env); */
  let waggle = float_of_int(Reprocessing.Env.frameCount(env)) /. 100.;
  let vec = {Geom.magnitude: 400., theta: cos(waggle *. 2.) *. halfPi /. 5.};

  let p1 = {x: 400., y: 400.};
  let p2 = Geom.addVectorToPoint({
    magnitude: 300.,
    theta: waggle *. tau /. 5.
  }, p1);
  /* let p2 = {x: 400., y: 600.}; */

  GeomDraw.line(p1, p2, env);

  GeomDraw.vec(point, vec, env);

  let response = GeomCollide.pointToLine(vec, point, p1, p2) |> fst;

  /* GeomDraw.vec(Geom.addVectorToPoint(vec, point), response, env); */



  let vecOfLine = Geom.vectorBetweenPoints(p1, p2);
  let vecToFirstPoint = Geom.vectorBetweenPoints(point, p1);
  GeomDraw.vec(point, vecToFirstPoint, env);
  let vecToP1RelativeToVec = {
    magnitude: vecToFirstPoint.magnitude,
    theta: vecToFirstPoint.theta -. vec.theta
  };
  /* GeomDraw.vec(point, vecToP1RelativeToVec, env); */
  let bd = Geom.vy(vecToP1RelativeToVec);

  Draw.stroke(withAlpha(0.2, Constants.blue), env);
  /* GeomDraw.vec(p1, {
    magnitude: bd,
    theta: vec.theta -. halfPi
  }, env); */

  /** DANGER ZONE */
  let angleDBC = vecOfLine.theta -. vec.theta -. halfPi;
  /* let angleDBC = angleDBC -. pi; */
  /* let angleDBC = normalize(angleDBC); */
  /* let angleDBC = angleDBC < 0. ? angleDBC +. tau : angleDBC; */
  /* print_endline(string_of_float(angleDBC)); */
  /* halfPi -. (vecOfLine.theta -. vec.theta); */

  /* Draw.fill(Constants.red, env); */
  GeomDraw.angle(p1, {
    magnitude: bd,
    theta: vec.theta -. halfPi
    /* theta: 0. */
  }, angleDBC, env);

  /* GeomDraw.vec(p1, {
    magnitude: bd,
    theta: angleDBC
  }, env); */


  let distFromP1ToIntersection = -. bd /. cos(angleDBC);
  if (distFromP1ToIntersection > 0. && distFromP1ToIntersection < vecOfLine.magnitude) {

    Draw.stroke(Constants.red, env);
    GeomDraw.vec(p1, {
      magnitude: distFromP1ToIntersection,
      theta: vecOfLine.theta
    }, env);
    let distToIntersection = Geom.vx(vecToP1RelativeToVec) -. distFromP1ToIntersection *. sin(angleDBC);
    let distFromPenetrationToIntersection = vec.magnitude -. distToIntersection;
    GeomDraw.vec(Geom.addVectorToPoint(vec, point), {
      magnitude: distFromPenetrationToIntersection,
      theta: vec.theta +. pi
    }, env);
    let vecFromIntersectionToPenetrationFromP1Perspective = {
      magnitude: distFromPenetrationToIntersection,
      theta: vec.theta -. vecOfLine.theta,
    };
    /* GeomDraw.vec(point, vecFromIntersectionToPenetrationFromP1Perspective, env); */
    let vecBack = {
      magnitude: Geom.vy(vecFromIntersectionToPenetrationFromP1Perspective),
      theta: vecOfLine.theta -. halfPi
    };
    GeomDraw.vec(Geom.addVectorToPoint(vec, point), vecBack, env)
  }
  /* if (distFromP1ToIntersection > vecOfLine.magnitude) {
    /* Doesn't intersect the line */
    v0
  } else { */
    /* {
      magnitude: ,
      theta: vecOfLine.theta +. halfPi
    } */
  /* } */



};

let circleCollisionResponse = (env) => {
  let circle = {Geom.Circle.center: mousePos(env), rad: 50.};
  let circle2 = {Geom.Circle.center: {x: 400., y: 400.}, rad: 100.};

  Draw.fill(withAlpha(0.2, Constants.green), env);
  GeomDraw.circle(circle, env);
  GeomDraw.circle(circle2, env);
  Draw.strokeWeight(5, env);
  Draw.stroke(withAlpha(0.4, Constants.black), env);
  Draw.noFill(env);
  GeomDraw.circle({...circle2, rad: circle2.rad +. circle.rad}, env);
  let vec = Geom.pectorToVector({dx: 200., dy: 100.});
  GeomDraw.circle(Circle.push(circle, vec), env);
  GeomDraw.vec(circle.Circle.center, vec, env);
  let response = GeomCollide.circleToCircle(vec, circle, circle2);
  GeomDraw.vec(Geom.addVectorToPoint(vec, circle.Circle.center), response, env);
  let final = Geom.addVectorToPoint(vec, circle.Circle.center)
    |> Geom.addVectorToPoint(response);
  GeomDraw.vec(final, {theta: response.theta +. halfPi, magnitude: 300.}, env);
  GeomDraw.vec(final, {theta: response.theta +. halfPi, magnitude: -300.}, env);
  GeomDraw.circle(Circle.push(Circle.push(circle, vec), response), env);
  Draw.noStroke(env);
};

let draw = (state, context, env) => {
  Draw.background(Constants.white, env);
  Draw.strokeCap(Reprocessing_Common.Project, env);
  Draw.strokeWeight(1, env);

  /* GeomDraw.rect(rect, env); */

  let rect = Geom.Rect.create({Geom.x: 300., y: 300.}, 100., 100.);
  let aabb = Geom.Rect.aabb(rect);

  let circle = {Geom.Circle.center: mousePos(env), rad: 50.};
  let rect = Geom.Rect.create(mousePos(env), 20., 20.);

  let circle2 = {Geom.Circle.center: {x: 400., y: 400.}, rad: 100.};

  /* let spots = angleRing(20) |> List.map(theta => ({Geom.magnitude: 100., theta}, circle.Geom.Circle.center)); */
  /* pointRectResponse(env); */
  /* circleRectResponse(env); */
  /* rectRectResponse(env); */
  polyPolyResponse(env);

};
