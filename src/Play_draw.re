open Play_types;
open Reprocessing;

let pithyTexts = [
  "Throw rocks",
  "That's the whole game",
  "Try throwing upward while jumping",
  "There's often a cliff on the other side of the world",
  "You could try playing golf",
  "Many thanks to kenney.nl for the assets"
];

let textColor = Constants.black;

let gameWidthInt = 120;
let gameWidth = blockSize *. float_of_int(gameWidthInt);

let drawStone = (context, pos, stone, env) => {
  let (x, y) = Geom.tuple(pos);
  let scale = stone.Stone.circle.rad /. Play_assets.Items.ore_coal_width *. 5.;
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.rotate(stone.Stone.rotation, env);
  /* Play_assets.ExtraItems.arrow_stone(context.Shared.extraItemsSheet, ~scale=scale, ~pos=( */
  Play_assets.Items.ore_coal(context.Shared.itemSheet, ~scale=scale, ~pos=(
    -. Play_assets.Items.ore_coal_width *. scale /. 2.,
    -. Play_assets.Items.ore_coal_height *. scale /. 2.,
  ), ~flip=false, env);
  Draw.popMatrix(env);

};

let shoulder = -4.;

let rockPos = (state, v, env) => {
  let angle = if (v.Geom.magnitude > 5.) {
    let percent = v.Geom.magnitude /. 200.;
    let extra = Geom.halfPi /. 2. *. percent;
    (state.player.facingLeft ? v.Geom.theta +. extra : v.Geom.theta +. Geom.pi -. extra)
  } else {
    0.
  };
  let (x, y) = Geom.tuple(state.player.box.pos);
  let y = y -. 10.;

  let loff = state.player.facingLeft ? 1. : -1.;
  let armTop = y -. Play_assets.Players.male_body_height *. 0.2 +. 10.;
  let arm = {Geom.x: x +. shoulder *. loff *. -1., y: armTop};
  let relativeToArm = {Geom.theta: angle +. Geom.halfPi, magnitude: Play_assets.Players.male_arm_height *. 0.4};

  Geom.addVectorToPoint(relativeToArm, arm);
};

let drawPlayer = (state, context, env) => {
  /* Draw.fill(Constants.green, env); */
  /* GeomDraw.rect(state.player.box, env); */

  let (x, y) = Geom.tuple(state.player.box.pos);
  let y = y -. 10.;

  let module Players = Play_assets.Players;

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
  Draw.translate(~x, ~y=y +. Players.male_body_height *. 0.2 +. 5., env);
  Draw.rotate(walk, env);
  Players.male_leg(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Players.male_leg_width *. 0.2, 0.), ~flip=false, env);
  Draw.rotate(-.walk +. walk2, env);
  Players.male_leg(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Players.male_leg_width *. 0.2, 0.), ~flip=false, env);
  Draw.popMatrix(env);


  /* arm behind */
  let loff = state.player.facingLeft ? 1. : -1.;
  let armTop = y -. Players.male_body_height *. 0.2 +. 10.;
  Draw.pushMatrix(env);
  Draw.translate(~x=x +. shoulder *. loff, ~y=armTop, env);
  Draw.rotate(state.player.facingLeft ? walk/.2. : walk2/.2., env);
  Players.male_arm(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Players.male_arm_width *. 0.2, 0.), ~flip=false, env);
  Draw.popMatrix(env);



  /* body */
  Players.male_body(context.Shared.charSheet, ~scale=0.4, ~flip=false,
  ~pos=(x -. Players.male_body_width *. 0.2, y -. Players.male_body_height *. 0.2 +. 5.), env);

  /* Head! */
  Draw.pushMatrix(env);
  Draw.translate(~x=x, ~y=y -. Players.male_head_height *. 0.1 -. 5., env);
  let bob = sin(state.player.walkTimer *. 20.) *. amp;
  let bob = state.player.isOnGround ? bob : 0.;
  Draw.translate(~x=0., ~y=bob *. 2., env);
  let rot = switch state.player.throw {
  | Some((_, v, stone)) when v.Geom.magnitude > 5. => Draw.rotate(((
    state.player.facingLeft ? v.Geom.theta /. 2. +. (v.Geom.theta < 0. ? Geom.halfPi : -. Geom.halfPi)
    : v.Geom.theta /. 2.
  )), env)
  | _ => ()
  };
  /* Draw.rotate(state.player.facingLeft ? walk : -. walk, env); */
  Players.male_head(context.Shared.charSheet, ~scale=0.4, ~flip=!state.player.facingLeft,
  ~pos=( -. Players.male_head_width *. 0.2, -. Players.male_head_width *. 0.3 ), env);
  Draw.popMatrix(env);

  /* arm in front */
  let faceRot = state.player.facingLeft ? walk2/.2. : walk/.2.;
  let rot = switch state.player.throw {
  | None => faceRot
  | Some((_, v, stone)) => {
    if (v.Geom.magnitude > 5.) {
      let percent = v.Geom.magnitude /. 200.;
      let extra = Geom.halfPi /. 2. *. percent;
      (state.player.facingLeft ? v.Geom.theta +. extra : v.Geom.theta +. Geom.pi -. extra)
    } else {
      faceRot
    }
  }
  };
  Draw.pushMatrix(env);
  Draw.translate(~x=x +. shoulder *. loff *. -1., ~y=armTop, env);
  Draw.rotate(rot, env);
  Players.male_arm(context.Shared.charSheet, ~scale=0.4, ~pos=(-.Players.male_arm_width *. 0.2, 0.), ~flip=false, env);

  switch state.player.throw {
  | None => ()
  | Some((_, v, stone)) => {
    drawStone(context, {Geom.x: 0., y: Play_assets.Players.male_arm_height *. 0.4}, stone, env)
  }
  };

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

        let x = float_of_int(x) *. blockSize +. blockSize /. 2.;
        let y = float_of_int(y) *. blockSize +. blockSize /. 2.;
        let sprite = switch block.Block.kind {
        | Block.Dirt => Play_assets.Tiles.dirt
        | Block.Rock => Play_assets.Tiles.rock
        };

        let xx = mod_float(x, gameWidth);
        let xx = xx < 0. ? xx +. gameWidth : xx;
        let rot = floor((sin(xx *. xx +. y *. y) +. 1.) /. 2. *. 4.);
        let rot = rot *. Geom.halfPi;

        Draw.pushMatrix(env);
        Draw.translate(~x, ~y, env);
        Draw.rotate(rot, env);
        sprite(context.Shared.tileSheet, ~pos=(-.blockSize /. 2., -.blockSize/.2.), ~scale=blockSize /. 128., ~flip=false, env);
        if (block.top) {
          Draw.rotate(-.rot, env);
          Play_assets.ExtraItems.grass_top(context.Shared.extraItemsSheet, ~pos=(-.blockSize /. 2., -.blockSize/.2.), ~scale=blockSize /. 128., ~flip=false, env);
        };
        Draw.popMatrix(env);

      }
      }
    }
  };

  Draw.noStroke(env);
  drawPlayer(state, context, env);

  let inPosition = (drawPadding, {Geom.x, y} as pos) => {
    let x0 = dx -. drawPadding;
    let y0 = dy -. drawPadding;
    let x1 = dx +. w +. drawPadding;
    let y1 = dy +. h +. drawPadding;

    if (y > y0 && y < y1) {
      if (x > x0 && x < x1) {
        Some(pos)
      } else {
        let left = x -. gameWidth;
        if (left > x0 && left < x1) {
          Some(Geom.addPoints(pos, {x: -.gameWidth, y: 0.}))
        } else {
          let right = x +. gameWidth;
          if (right > x0 && right < x1) {
            Some(Geom.addPoints(pos, {x: +.gameWidth, y: 0.}))
          } else {
            None
          }
        }
      };
    } else {
      None
    }
  };

  Draw.fill(Reprocessing.Utils.color(~r=150, ~g=150, ~b=170, ~a=255), env);
  state.stones |> List.iter(stone => {
    let {Geom.x,y} = stone.Stone.circle.center;
    switch (inPosition(200., stone.Stone.circle.center)) {
    | None => ()
    | Some(pos) => {
      drawStone(context, pos, stone, env)
    }
    };
  });

  state.textPos |> List.iter(((text, pos)) => {
    switch (inPosition(800., pos)) {
    | None => ()
    | Some(pos) => {
      Draw.tint(Constants.black, env);
      Draw.text(~font=context.Shared.smallFont, ~pos=Geom.intTuple(pos), ~body=text, env);
      Draw.noTint(env);
      Draw.text(~font=context.Shared.smallFont, ~pos=Geom.intTuple(Geom.addPoints(pos, {Geom.x: -1., y: -1.})), ~body=text, env);
    }
    }
  });

};