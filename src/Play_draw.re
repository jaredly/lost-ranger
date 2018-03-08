open Play_types;
open Reprocessing;

let draw = (state, context, env) => {
  Draw.background(Constants.white, env);

  let (dx, dy, w, h) = state.camera;

  Draw.translate(~x=-.dx, ~y=-.dy, env);


  let x0 = int_of_float(dx /. blockSize) - 1;
  let y0 = int_of_float(dy /. blockSize);
  let ww = int_of_float(w /. blockSize) + 1;
  let hh = int_of_float(h /. blockSize);
  for (x in x0 to x0 + ww) {
    for (y in y0 to y0 + ww) {
      switch (getBlock(state.blocks, (x, y))) {
      | None => ()
      | Some(block) => {

        let x = float_of_int(x) *. blockSize;
        let y = float_of_int(y) *. blockSize;
        let color = switch block.Block.kind {
        | Block.Dirt => Reprocessing.Utils.color(~r=120, ~g=100, ~b=50, ~a=255)
        | Block.Rock => Reprocessing.Utils.color(~r=50, ~g=50, ~b=70, ~a=255)
        };
        Draw.fill(color, env);
        Draw.stroke(Constants.white, env);
        Draw.rectf(~pos=(x, y), ~width=blockSize , ~height=blockSize , env);

      }
      }
    }
  };
  /* Hashtbl.iter(((x, y), block) => { */
    /* let x = float_of_int(x) *. blockSize;
    let y = float_of_int(y) *. blockSize;
    let color = switch block.Block.kind {
    | Block.Dirt => Reprocessing.Utils.color(~r=120, ~g=100, ~b=50, ~a=255)
    | Block.Rock => Reprocessing.Utils.color(~r=50, ~g=50, ~b=70, ~a=255)
    };
    Draw.fill(color, env);
    Draw.rectf(~pos=(x, y), ~width=blockSize , ~height=blockSize , env); */
    /* Draw.rectf(~pos=(x, y), ~width=blockSize -. 1., ~height=blockSize -. 1., env); */
  /* }, state.blocks); */

  Draw.noStroke(env);
  Draw.fill(Constants.green, env);
  /* Draw.stroke(Constants.green, env); */
  GeomDraw.rect(state.player.box, env);
  /* GeomDraw.circle({Geom.Circle.center: state.player.box.pos, rad: blockSize /. 2.}, env); */

  state.stones |> List.iter(stone => {
    Draw.fill(Reprocessing.Utils.color(~r=150, ~g=150, ~b=170, ~a=255), env);
    GeomDraw.circle(stone.Stone.circle, env);
    /* Draw.ellipsef(~center=Geom.tuple(stone.Stone.circle.center), ~radx=10., ~rady=10., env); */
  });

  Draw.stroke(Constants.red, env);
  switch (state.userInput.throw) {
  | None => ()
  | Some((p1, vec)) => GeomDraw.line(state.player.box.pos, Geom.addVectorToPoint(vec, state.player.box.pos), env)
  };
  Draw.noStroke(env);
};

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