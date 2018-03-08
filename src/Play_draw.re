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