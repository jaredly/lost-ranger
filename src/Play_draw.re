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


let module PlayerSprites = {
  let module Players = Play_assets.Players;
  let module Male = {
    let top = 0.;
    let head_off = 0.;
    let height = 56.;
    let width = 28.;
    let leg = Players.male_leg;
    let arm = Players.male_arm;
    let head = Players.male_head;
    let body = Players.male_body;
    let head_width = Players.male_head_width;
    let head_rx = Players.male_head_width /. 2.;
    let head_height = Players.male_head_height;
    let body_height = Players.skeleton_body_height;
    let arm_height = Players.skeleton_arm_height;
    let arm_width = Players.skeleton_arm_width;
    let leg_width = Players.skeleton_leg_width;
    let body_width = Players.skeleton_body_width;
  };

  let module Female = {
    let top = 0.;
    let head_off = 0.;
    let height = 56.;
    let width = 28.;
    let leg = Players.female_leg;
    let arm = Players.female_arm;
    let head = Players.female_head;
    let body = Players.female_body;
    let head_width = Players.female_head_width;
    let head_rx = Players.male_head_width /. 2.;
    let head_height = Players.male_head_height;
    let body_height = Players.skeleton_body_height;
    let arm_height = Players.skeleton_arm_height;
    let arm_width = Players.skeleton_arm_width;
    let leg_width = Players.skeleton_leg_width;
    let body_width = Players.skeleton_body_width;
  };

  let module Skeleton = {
    let top = 0.;
    let head_off = 0.;
    let height = 56.;
    let width = 28.;
    let leg = Players.skeleton_leg;
    let arm = Players.skeleton_arm;
    let head = Players.skeleton_head;
    let body = Players.skeleton_body;
    let head_width = Players.skeleton_head_width;
    let head_rx = Players.male_head_width /. 2.;
    let head_height = Players.skeleton_head_height;
    let body_height = Players.skeleton_body_height;
    let arm_height = Players.skeleton_arm_height;
    let arm_width = Players.skeleton_arm_width;
    let leg_width = Players.skeleton_leg_width;
    let body_width = Players.skeleton_body_width;
  };

  let module Zombie = {
    let top = 0.;
    let head_off = 0.;
    let height = 56.;
    let width = 28.;
    let leg = Players.zombie_leg;
    let arm = Players.zombie_arm;
    let head = Players.zombie_head;
    let body = Players.zombie_body;
    let head_width = Players.zombie_head_width;
    let head_rx = Players.male_head_width /. 2.;
    let head_height = Players.zombie_head_height;
    let body_height = Players.skeleton_body_height;
    let arm_height = Players.skeleton_arm_height;
    let arm_width = Players.skeleton_arm_width;
    let leg_width = Players.skeleton_leg_width;
    let body_width = Players.skeleton_body_width;
  };

  let module Alien = {
    let top = 0.;
    let head_off = 8.;
    let height = 36.;
    let width = 14.;
    let leg = Players.alien_leg;
    let arm = Players.alien_arm;
    let head = Players.alien_head;
    let body = Players.alien_body;
    let head_width = Players.alien_head_width;
    let head_rx = Players.alien_head_width /. 2.;
    let head_height = Players.alien_head_height;
    let body_height = Players.alien_body_height;
    let arm_height = Players.alien_arm_height;
    let arm_width = Players.alien_arm_width;
    let leg_width = Players.alien_leg_width;
    let body_width = Players.alien_body_width;
  };

  let module Gnome = {
    let top = 0.;
    let head_off = 10.;
    let height = 30.;
    let width = 14.;
    let leg = Players.gnome_leg;
    let arm = Players.gnome_arm;
    let head = Players.gnome_head;
    let body = Players.gnome_body;
    let head_width = Players.gnome_head_width;
    let head_rx = Players.gnome_head_width /. 2.;
    let head_height = Players.gnome_head_height;
    let body_height = Players.gnome_body_height;
    let body_width = Players.gnome_body_width;
    let arm_height = Players.gnome_arm_height;
    let arm_width = Players.gnome_arm_width;
    let leg_width = Players.gnome_leg_width;
  };

  open Play_assets;
  module type Sprite = {
    let top: float;
    let head_off: float;
    let height: float;
    let width: float;
    let leg: sprite;
    let arm: sprite;
    let head: sprite;
    let body: sprite;
    let head_width: float;
    let head_rx: float;
    let head_height: float;
    let body_height: float;
    let body_width: float;
    let arm_height: float;
    let arm_width: float;
    let leg_width: float;
  };

  let get = which => switch which {
  | `Male => (module Male: Sprite)
  | `Female => (module Female: Sprite)
  | `Zombie => (module Zombie: Sprite)
  | `Skeleton => (module Skeleton: Sprite)
  | `Gnome => (module Gnome: Sprite)
  | `Alien => (module Alien: Sprite)
  };

  let options = [|`Female, `Male, `Zombie, `Skeleton, `Gnome, `Alien|];
  let getNum = num => get(options[num mod Array.length(options)]);
  /* let  */
};

let spriteScale = 0.4;

let spriteBox = (pos, skin, prevHeight) => {
  let module PlayerSprite = (val PlayerSprites.getNum(skin): PlayerSprites.Sprite);
  Geom.Rect.create(Geom.addPoints(pos, {Geom.x: 0., y: prevHeight /. 2. -.PlayerSprite.height /. 2.}), PlayerSprite.width, PlayerSprite.height)
};

let rockPos = (state, v, env) => {
  let module PlayerSprite = (val PlayerSprites.getNum(state.player.skin): PlayerSprites.Sprite);

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
  let armTop = y -. PlayerSprite.body_height *. spriteScale /. 2. +. 10.;
  let arm = {Geom.x: x +. shoulder *. loff *. -1., y: armTop};
  let relativeToArm = {Geom.theta: angle +. Geom.halfPi, magnitude: PlayerSprite.arm_height *. spriteScale};

  Geom.addVectorToPoint(relativeToArm, arm);
};


let drawPlayer = (state, context, env) => {
  /* Draw.fill(Constants.green, env); */
  /* GeomDraw.rect(state.player.box, env); */

  let (x, y) = Geom.tuple(state.player.box.pos);

  let module PlayerSprite = (val PlayerSprites.getNum(state.player.skin): PlayerSprites.Sprite);
  let y = y -. 10. +. PlayerSprite.top;

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
  Draw.translate(~x, ~y=y +. PlayerSprite.body_height *. spriteScale /. 2. +. 5., env);
  Draw.rotate(walk, env);
  PlayerSprite.leg(context.Shared.charSheet, ~scale=spriteScale, ~pos=(-.PlayerSprite.leg_width *. spriteScale /. 2., 0.), ~flip=false, env);
  Draw.rotate(-.walk +. walk2, env);
  PlayerSprite.leg(context.Shared.charSheet, ~scale=spriteScale, ~pos=(-.PlayerSprite.leg_width *. spriteScale /. 2., 0.), ~flip=false, env);
  Draw.popMatrix(env);


  /* arm behind */
  let loff = state.player.facingLeft ? 1. : -1.;
  let armTop = y -. PlayerSprite.body_height *. spriteScale /. 2. +. 10.;
  Draw.pushMatrix(env);
  Draw.translate(~x=x +. shoulder *. loff, ~y=armTop, env);
  Draw.rotate(state.player.facingLeft ? walk/.2. : walk2/.2., env);
  PlayerSprite.arm(context.Shared.charSheet, ~scale=spriteScale, ~pos=(-.PlayerSprite.arm_width *. spriteScale /. 2., 0.), ~flip=false, env);
  Draw.popMatrix(env);



  /* body */
  PlayerSprite.body(context.Shared.charSheet, ~scale=spriteScale, ~flip=!state.player.facingLeft,
  ~pos=(x -. PlayerSprite.body_width *. spriteScale /. 2., y -. PlayerSprite.body_height *. spriteScale /. 2. +. 5.), env);

  /* Head! */
  Draw.pushMatrix(env);
  Draw.translate(~x=x, ~y=y -. PlayerSprite.head_height *. spriteScale /. 4. -. 5. +. PlayerSprite.head_off, env);
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
  PlayerSprite.head(context.Shared.charSheet, ~scale=spriteScale, ~flip=!state.player.facingLeft,
  ~pos=(
    -. (state.player.facingLeft ? PlayerSprite.head_rx : PlayerSprite.head_width -. PlayerSprite.head_rx) *. spriteScale,
    -. PlayerSprite.head_height *. spriteScale /. 4. *. 3.
  ), env);
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
  PlayerSprite.arm(context.Shared.charSheet, ~scale=spriteScale, ~pos=(-.PlayerSprite.arm_width *. spriteScale /. 2., 0.), ~flip=false, env);

  switch state.player.throw {
  | None => ()
  | Some((_, v, stone)) => {
    drawStone(context, {Geom.x: 0., y: PlayerSprite.arm_height *. spriteScale}, stone, env)
  }
  };

  Draw.popMatrix(env);

};

let draw = (state, context, env) => {
  Draw.background(Utils.color(~r=200, ~g=230, ~b=255, ~a=255), env);
  /* Draw.background(Constants.white, env); */

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