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

  Draw.fill(Constants.green, env);
  Draw.strokeWeight(1, env);
  GeomDraw.rect(rect, env);

  let aabb = Geom.Rect.aabb(rect);
  let circle = {Geom.Circle.center: Geom.fromIntTuple(Env.mouse(env)), rad: 20.};
  let rect = Geom.Rect.create(Geom.fromIntTuple(Env.mouse(env)), 20., 20.);

  Draw.noStroke(env);

  let thetas = range(20)
  |> List.map(i => 3.14159 *. 2. /. 20. *. float_of_int(i));

  let vecs = thetas |> List.map(theta => {Geom.magnitude: 100., theta});

  let spots = vecs |> List.map(vec => (vec, circle.Geom.Circle.center));

  Draw.strokeCap(Reprocessing_Common.Project, env);

  Draw.stroke(Constants.blue, env);
  spots |> List.iter(((vec, pos)) => {
    /* let add = {Geom.magnitude: circle.Geom.Circle.rad, theta: vec.Geom.theta} |> Geom.addVectorToPoint; */
    GeomDraw.vec((pos), vec, env)
  });
  Draw.noStroke(env);

  spots |> List.iter(((vec, pos)) => {
    let moved = Geom.Rect.push(rect, vec);
    if (Geom.Rect.testAabb(moved, aabb)) {
      Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=50), env);
      GeomDraw.rect(moved, env);
      let back = Geom.Rect.collideToAabb(vec, moved, aabb);
      Draw.stroke(Constants.black, env);
      Draw.noFill(env);
      GeomDraw.vec(Geom.addVectorToPoint(vec, pos), back, env);
      GeomDraw.rect(Geom.Rect.push(moved, back), env);
      Draw.noStroke(env);
    }
  });

  /* spots |> List.iter(((vec, pos)) => {
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
  }); */
};
