open Reprocessing;
open Geom;

let range = x => {
  let rec loop = i => {
    i >= x ? [] : [i, ...loop(i + 1)]
  };
  loop(0)
};

let mousePos = env => Geom.fromIntTuple(Env.mouse(env));

let withAlpha = (alpha, color) => {...color, Reprocessing_Common.a: 0.1};

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

let pointRectResponse = (env) => {
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

  let response = GeomCollide.pointToLine(vec, point, p1, p2);

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
  pointRectResponse(env);

};
