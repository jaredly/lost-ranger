open Reprocessing;

let range = x => {
  let rec loop = i => {
    i >= x ? [] : [i, ...loop(i + 1)]
  };
  loop(0)
};

let draw = (state, context, env) => {
  Draw.background(Constants.white, env);

  let rect = Geom.Rect.create({Geom.x: 300., y: 300.}, 100., 100.);
  let aabb = Geom.Rect.aabb(rect);
  let circle = {Geom.Circle.center: Geom.fromIntTuple(Env.mouse(env)), rad: 20.};

  Draw.noStroke(env);

  Draw.fill(Constants.green, env);
  /* Draw.stroke(Constants.green, env); */
  GeomDraw.rect(rect, env);

  /* Draw.fill(Constants.red, env);
  GeomDraw.circle(circle, env); */

  let thetas = range(10)
  |> List.map(i => 3.14159 *. 2. /. 10. *. float_of_int(i));

  let vecs = thetas |> List.map(theta => {Geom.magnitude: 100., theta});

  let spots = vecs |> List.map(vec => (vec, Geom.addVectorToPoint({theta: vec.Geom.theta, magnitude: circle.Geom.Circle.rad}, circle.Geom.Circle.center)));

  Draw.strokeCap(Reprocessing_Common.Project, env);

  Draw.stroke(Constants.blue, env);
  spots |> List.iter(((vec, pos)) => {
    /* let add = {Geom.magnitude: circle.Geom.Circle.rad, theta: vec.Geom.theta} |> Geom.addVectorToPoint; */
    GeomDraw.vec((pos), vec, env)
  });
  Draw.noStroke(env);

  spots |> List.iter(((vec, pos)) => {
    let moved = Geom.Circle.push(circle, vec);
    if (Geom.Aabb.testCircle(aabb, moved)) {
      Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=50), env);
      GeomDraw.circle(moved, env);
      let back = Geom.Aabb.collideToCircle(vec, aabb, moved);
      Draw.stroke(Constants.black, env);
      Draw.noFill(env);
      GeomDraw.vec(Geom.addVectorToPoint(vec, pos), back, env);
      GeomDraw.circle(Geom.Circle.push(moved, back), env);
      Draw.noStroke(env);
    }
  });

  /* for (i in 0 to 10) {
    let theta = ;
    let pos = circle.Geom.Circle.center;
    let vec = {Geom.magnitude: 50., theta};
    let p1 = Geom.addVectorToPoint({Geom.magnitude: circle.Geom.Circle.rad, theta}, pos);
    let p2 = Geom.addVectorToPoint(Geom.addMagnitude(vec, circle.Geom.Circle.rad), pos);
    GeomDraw.line(p1, p2, env);
  } */
};
