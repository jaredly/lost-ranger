open Play_types;
open Reprocessing;


let gameWidth = blockSize *. 100.;

let drawPlayer = (state, context, env) => {
  /* Draw.fill(Constants.green, env); */
  /* GeomDraw.rect(state.player.box, env); */

  let (x, y) = Geom.tuple(state.player.box.pos);
  let y = y -. 10.;

  /* Legs */
  let amp = Geom.halfPi /. 2.;
  let amp = abs_float(Geom.vx(state.player.vel)) > 0.001 ? amp : 0.;
  let walk = sin(state.player.walkTimer *. 10.) *. amp;
  let walk2 = sin(state.player.walkTimer *. 10. +. Geom.pi) *. amp;
  let (walk, walk2) = state.player.isOnGround ? (walk, walk2) : {
    let amp = min(5., abs_float(Geom.vx(state.player.vel))) /. 5.;
    (Geom.halfPi *. amp /. 2., -. Geom.halfPi *. amp /. 2.)
  };
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y=y +. Play_assets.male_body_height *. 0.2 +. 5., env);
  Draw.rotate(walk, env);
  Play_assets.male_leg(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Play_assets.male_leg_width *. 0.2, 0.), ~flip=false, env);
  Draw.rotate(-.walk +. walk2, env);
  Play_assets.male_leg(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Play_assets.male_leg_width *. 0.2, 0.), ~flip=false, env);
  Draw.popMatrix(env);


  /* arm behind */
  let loff = state.player.facingLeft ? 1. : -1.;
  let shoulder = -4.;
  let armTop = y -. Play_assets.male_body_height *. 0.2 +. 10.;
  Draw.pushMatrix(env);
  Draw.translate(~x=x +. shoulder *. loff, ~y=armTop, env);
  Draw.rotate(state.player.facingLeft ? walk/.2. : walk2/.2., env);
  Play_assets.male_arm(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Play_assets.male_arm_width *. 0.2, 0.), ~flip=false, env);
  Draw.popMatrix(env);



  /* body */
  Play_assets.male_body(context.Shared.charSheet, ~scale=0.4, ~flip=false,
  ~pos=(x -. Play_assets.male_body_width *. 0.2, y -. Play_assets.male_body_height *. 0.2 +. 5.), env);

  /* Head! */
  Draw.pushMatrix(env);
  Draw.translate(~x=x, ~y=y -. Play_assets.male_head_height *. 0.2 -. 5., env);
  let bob = sin(state.player.walkTimer *. 20.) *. amp;
  let bob = state.player.isOnGround ? bob : 0.;
  Draw.translate(~x=0., ~y=bob *. 2., env);
  /* Draw.rotate(state.player.facingLeft ? walk : -. walk, env); */
  Play_assets.male_head(context.Shared.charSheet, ~scale=0.4, ~flip=!state.player.facingLeft,
  ~pos=( -. Play_assets.male_head_width *. 0.2, -. Play_assets.male_head_width *. 0.2 ), env);
  Draw.popMatrix(env);

  /* arm in front */
  Draw.pushMatrix(env);
  Draw.translate(~x=x +. shoulder *. loff *. -1., ~y=armTop, env);
  Draw.rotate(state.player.facingLeft ? walk2/.2. : walk/.2., env);
  Play_assets.male_arm(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Play_assets.male_arm_width *. 0.2, 0.), ~flip=false, env);
  Draw.popMatrix(env);


};

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

  Draw.strokeWeight(3, env);
  Draw.stroke(Utils.color(~r=150, ~g=150, ~b=150, ~a=100), env);
  switch (state.userInput.throw) {
  | None => ()
  | Some((p1, vec)) => GeomDraw.line(Geom.addPoints({Geom.x: 0., y: -10.}, state.player.box.pos), Geom.addVectorToPoint(vec, state.player.box.pos), env)
  };
  Draw.noStroke(env);

  Draw.noStroke(env);
  /* Draw.stroke(Constants.green, env); */
  drawPlayer(state, context, env);
  /* GeomDraw.circle({Geom.Circle.center: state.player.box.pos, rad: blockSize /. 2.}, env); */


  Draw.fill(Reprocessing.Utils.color(~r=150, ~g=150, ~b=170, ~a=255), env);
  let drawPadding = 200.;
  let x0 = dx -. drawPadding;
  let y0 = dy -. drawPadding;
  let x1 = dx +. w +. drawPadding;
  let y1 = dy +. h +. drawPadding;
  state.stones |> List.iter(stone => {
    let {Geom.x,y} = stone.Stone.circle.center;
    if (y > y0 && y < y1) {
      if (x > x0 && x < x1) {
        GeomDraw.circle(stone.Stone.circle, env);
      } else {
        let left = x -. gameWidth;
        if (left > x0 && left < x1) {
          GeomDraw.circle(Geom.Circle.translate(stone.Stone.circle, {x: -.gameWidth, y: 0.}), env)
        } else {
          let right = x +. gameWidth;
          if (right > x0 && right < x1) {
            GeomDraw.circle(Geom.Circle.translate(stone.Stone.circle, {x: +.gameWidth, y: 0.}), env)
          }
        }
      }
    };
    /* Draw.ellipsef(~center=Geom.tuple(stone.Stone.circle.center), ~radx=10., ~rady=10., env); */
  });
};