open Play_types;
open Reprocessing;

let draw = (state, context, env) => {
  Draw.background(Constants.white, env);
  Hashtbl.iter(((x, y), block) => {
    let x = float_of_int(x) *. blockSize;
    let y = float_of_int(y) *. blockSize;
    let color = switch block.Block.kind {
    | Block.Dirt => Reprocessing.Utils.color(~r=120, ~g=100, ~b=50, ~a=255)
    | Block.Rock => Reprocessing.Utils.color(~r=50, ~g=50, ~b=70, ~a=255)
    };
    Draw.fill(color, env);
    Draw.rectf(~pos=(x, y), ~width=blockSize , ~height=blockSize , env);
    /* Draw.rectf(~pos=(x, y), ~width=blockSize -. 1., ~height=blockSize -. 1., env); */
  }, state.blocks);

  Draw.noStroke(env);
  Draw.fill(Constants.green, env);
  /* Draw.stroke(Constants.green, env); */
  GeomDraw.rect(state.player.box, env);
  /* GeomDraw.circle({Geom.Circle.center: state.player.box.pos, rad: blockSize /. 2.}, env); */
};