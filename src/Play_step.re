open Play_types;

let followPlayer = (player, (a, b, w, h), gameWidth) => {
  let (a, player) = if (player.box.pos.x < 0.) {
    (a +. gameWidth, {...player, box: {...player.box, pos: {...player.box.pos, x: gameWidth +. player.box.pos.x}}})
  } else if (player.box.pos.x > gameWidth) {
    (a -. gameWidth, {...player, box: {...player.box, pos: {...player.box.pos, x: player.box.pos.x -. gameWidth}}})
  } else {
    (a, player)
  };
  let {Geom.x, y} = player.box.pos;
  let hpadding = w /. 3.;
  let vpadding = h /. 3.;
  ((
    max(min(x -. hpadding, a), x +. hpadding -. w),
    max(min(y -. vpadding, b), y +. vpadding -. h),
    w,
    h
  ), player)
};

let gameWidth = Play_draw.gameWidth;

let start = (env) => {
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let height = Reprocessing.Env.height(env) |> float_of_int;

  let cols = int_of_float(width /. blockSize);
  let ground = int_of_float(height /. blockSize) / 2;

  let textPositions = ref([]);

  let blocks = Array.make_matrix(500, Play_draw.gameWidthInt, None);
  let d = ref(0);
  for (x in 0 to Play_draw.gameWidthInt - 1) {
    let change = Random.float(10.) > 4.;
    if (change && x > 2) {
      /* let off = 5.; */
      let off = ceil(sqrt(Random.float(16.)));
      d := min(10, max(-10, d^ + Random.int(int_of_float(off)) - (int_of_float(off /. 2.))));
    };

    if (x mod int_of_float(800. /. blockSize) == 0) {
      textPositions := [{Geom.y: float_of_int(ground + d^ + (x == 0 ? 1 : 3)) *. blockSize, x: float_of_int(x) *. blockSize}, ...textPositions^]
    };
    if (x > Play_draw.gameWidthInt - 2) {
      d := 0;
    };

    for (y in d^ to 50) {
      blocks[ground + y][x] = Some(Block.init(Block.Dirt, y == d^));
    };
  };

  let texts = {
    let rec loop = (a, b) => {
      switch (a, b) {
      | ([], _) => []
      | (_, []) => []
      | ([text, ...texts], [pos, ...poss]) => [(text, pos), ...loop(texts, poss)]
      }
    };
    loop(Play_draw.pithyTexts, textPositions^ |> List.rev)
  };

  let box = Play_draw.spriteBox({Geom.x: 0., y: 0.}, 0, 0.);
  let playerPos = {Geom.y: float_of_int(ground) *. blockSize -. box.Geom.Rect.hh, x: blockSize};
  let player = {
    vel: Geom.v0,
    throw: None,
    /* box: Geom.Rect.create(playerPos, blockSize *. 0.7, blockSize *. 1.4), */
    box: {...box, Geom.Rect.pos: playerPos},
    throwSkill: 0.,
    skin: 0,
    isOnGround: false,
    facingLeft: true,
    walkTimer: 0.,
    inventory: {
      Inventory.size: 20,
      slots: Array.make(20, Inventory.Nothing),
      selected: 0
    }
  };
  let (camera, player) = followPlayer(player, (0., 0., width, height), gameWidth);

  {
    textPos: texts,
    animals: [],
    trees: [],
    blocks,
    looseFruit: [],
    stones: [],
    userInput: {
      left: false, right: false, jump: false, action: false,
    },
    camera,
    player
  }
};

let maxWalk = 5.;
let walkSpeed = 0.5;

let module MovePlayer = Play_collide.Mover({
  let gravity = 0.5;
  let friction = 0.7;
  let jump = 11.;
  let bounce = false;
});

let module MoveStone = Play_collide.Mover({
  let gravity = 0.5;
  let friction = 0.9;
  let jump = 0.;
  let bounce = true;
});

let movePlayer = (userInput, player, blocks) => {
  let vx = Geom.vx(player.vel);
  let acc = userInput.left ? (vx > -. maxWalk ? Geom.{magnitude: walkSpeed, theta: pi} : Geom.v0) : (
    userInput.right ? (vx < maxWalk ? Geom.{magnitude: walkSpeed, theta: 0.} : Geom.v0) : Geom.v0
  );
  let (isOnGround, vel) = MovePlayer.moveObject(player.box.pos, player.vel, acc, userInput.jump, Play_collide.testRect(player.box), Play_collide.collideRect(player.box), blocks);
  {...player, vel, box: Geom.Rect.push(player.box, vel), isOnGround}
};

let wrapX = (x, gameWidth) => x < 0. ? gameWidth +. x : (x > gameWidth ? x -. gameWidth : x);

let wrap = ({Geom.x, y}, gameWidth) => {Geom.x: wrapX(x, gameWidth), y};

let moveStone = (stone, otherStones, blocks) => {
  open! Stone;
  let (_, vel) = MoveStone.moveObject(
    stone.circle.center,
    stone.vel,
    Geom.v0,
    false,
    Play_collide.testCircle(stone.circle),
    Play_collide.collideCircle(stone.circle),
    blocks
  );
  {vel, rotation: stone.rotation, circle: {...stone.circle, center: wrap(Geom.addVectorToPoint(vel, stone.circle.center), gameWidth)}}
};

let moveStones = (stones, blocks) => {
  let rec loop = stones => switch stones {
    | [] => []
    | [stone, ...stones] => [moveStone(stone, stones, blocks), ...loop(stones)]
  };
  loop(stones)
};

let getUserInput = (prev, env) => Reprocessing.({
  left: Env.key(Events.Left, env),
  right: Env.key(Events.Right, env),
  jump: Env.key(Events.Up, env),
  action: false,
});

let joystickButton = (pos, env) => {
  Play_draw.touchButtons(env) |> List.fold_left((result, (action, shape) ) => {
    switch result {
    | Some(x) => result
    | None => {
      if (Geom.Circle.testPoint(shape, pos)) {
        Some(action)
      } else {
        None
      }
    }
    }
  }, None);
};

let getTouchInput = (state, env) => {
  let throwId = switch state.player.throw {
  | None => None
  | Some((_, _, _, id)) => Some(id)
  };
  Hashtbl.fold((key, pos, input) => {
    if (Some(key) == throwId) {
      input /* skip this one */
    } else {
      switch (joystickButton(Geom.fromTuple(pos), env)) {
      | Some(`Left) => {...input, left: true}
      | Some(`Right) => {...input, right: true}
      | Some(`Jump) => {...input, jump: true}
      | Some(`JumpLeft) => {...input, jump: true, left: true}
      | Some(`JumpRight) => {...input, jump: true, right: true}
      | _ => input
      }
    }
  }, Reprocessing.Env.touches(env), {
    left: false, right: false, jump: false, action: false,
  })
};

let mergeInputs = (one, two) => {
  left: one.left || two.left,
  right: one.right || two.right,
  jump: one.jump || two.jump,
  action: one.action || two.action
};

let collisionPairs = (items, test) => {
  let rec loop = (items) => {
    switch items {
    | [] => []
    | [one, ...rest] => {
      (List.filter(test(one), rest) |> List.map(other => (one, other)))
      @ loop(rest)
    }
    }
  };
  loop(items)
};

let collideStones = stones => {
  open Stone;
  let boxWidth = blockSize *. 1.;
  let boxes = Play_draw.gameWidthInt / 1;
  let broad = Array.make(boxes + 1, []);
  let mutableStones = List.mapi((i, s) => ref((s, Geom.Circle.push(s.Stone.circle, s.Stone.vel), false, i)), stones);
  List.iter(stoneRef => {
    let (stone, moved, _, _) = stoneRef.contents;
    let {Geom.Circle.center: {Geom.x}, rad} = moved;
    let leftBox = (x -. rad) /. boxWidth |> floor |> int_of_float;
    let leftBox = leftBox == -1 ? boxes - 1 : leftBox;
    let rightBox = (x +. rad) /. boxWidth |> floor |> int_of_float;
    broad[leftBox] = [stoneRef, ...broad[leftBox]];
    if (leftBox != rightBox) {
      let rightBox = rightBox >= boxes ? 0 : rightBox;
      broad[rightBox] = [stoneRef, ...broad[rightBox]];
    };
  }, mutableStones);
  let pairHash = Hashtbl.create(100);
  Array.iteri((i, box) => {
    switch box {
    | [] => ()
    | [_alone] => ()
    | stones => {
      let checkLoop = i == 0 || i == boxes - 1;
      let pairs = collisionPairs(
        stones,
        (a, b) => {
          let (a, ma, _, ai) = a^;
          let (b, mb, _, bi) = b^;
          let key = (min(ai, bi), max(ai, bi));
          if (Hashtbl.mem(pairHash, key)) {
            false
          } else {
            if (Geom.Circle.testCircle(ma, mb)) {
              Hashtbl.replace(pairHash, key, true);
              true
            } else if (checkLoop) {
              let dx = ma.center.x -. mb.center.x;
              /* We've looped */
              if (abs_float(dx) > boxWidth) {
                if (ma.center.x > mb.center.x) {
                  Geom.Circle.testCircle(Geom.Circle.translate(ma, {Geom.x: -.gameWidth, y: 0.}), mb)
                } else {
                  Geom.Circle.testCircle(Geom.Circle.translate(mb, {Geom.x: -.gameWidth, y: 0.}), ma)
                }
              } else {
                false
              }
            } else {
              false
            }
          }
        }
      );
      List.iter(((oneRef, otherRef)) => {
        let (one, mone, _, ai) = oneRef^;
        let (other, mother, _, bi) = otherRef^;
        let getVector = (c1, c2) => {
          let vel = Geom.addVectors(one.Stone.vel, Geom.invertVector(other.Stone.vel));
          GeomCollide.circleToCircle(vel, c1, c2)
        };
        let v = if (checkLoop) {
          let dx = one.circle.center.x -. other.circle.center.x;
          /* We've looped */
          if (abs_float(dx) > boxWidth) {
            if (one.circle.center.x > other.circle.center.x) {
              getVector(Geom.Circle.translate(one.circle, {Geom.x: -.gameWidth, y: 0.}), other.circle)
            } else {
              getVector(one.circle, Geom.Circle.translate(other.circle, {Geom.x: -.gameWidth, y: 0.}))
            }
          } else {
            getVector(one.circle, other.circle);
          }
        } else {
          getVector(one.circle, other.circle);
        };
        let v = v |> Geom.invertVector;
        let first = (one.Stone.circle.rad *. one.Stone.circle.rad);
        let second = (other.Stone.circle.rad *. other.Stone.circle.rad);
        let fshare = first /. (first +. second);
        oneRef := (Stone.force(one, v |> Geom.invertVector |> x => Geom.scaleVector(x, 1. -. fshare)), mone, true, ai);
        otherRef := (Stone.force(other, Geom.scaleVector(v, fshare)), mother, true, bi);
      }, pairs)
    }
    }
  }, broad);
  /* Some cool down */
  List.map(x => {
    let (stone, _, hit, _) = x^;
    if (hit) {
      let vel = Geom.scaleVector(stone.Stone.vel, 0.8);
      /* let vel = abs_float(vel.Geom.magnitude) < 1. ? Geom.v0 : vel; */
      {...stone, Stone.vel}
    } else {
      stone
    }
  }, mutableStones)
};

let switchSkin = player => {
  let skin = player.skin + 1;
  {...player, skin, box: Play_draw.spriteBox(player.box.pos, skin, player.box.height)};
};

let step = (state, context, env) => {
  let userInput = getUserInput(state.userInput, env);
  let userInput = if (Reprocessing.Env.isTouchScreen(env)) {
    mergeInputs(userInput, getTouchInput(state, env));
  } else {
    userInput
  };

  let stones = collideStones(state.stones);
  let stones = moveStones(stones, state.blocks);
  let player = state.player;

  let player = movePlayer(userInput, player, state.blocks);
  let inputLeft = userInput.left ? true : (userInput.right ? false : player.facingLeft);
  let facingLeft = switch player.throw {
  | None => inputLeft
  | Some((_, v, _, _)) => v.Geom.magnitude > 5. ? Geom.vx(v) < 0. : inputLeft
  };
  let walkTimer = (userInput.left || userInput.right ? player.walkTimer +. Reprocessing.Env.deltaTime(env) : 0.);
  let (camera, player) = followPlayer(player, state.camera, gameWidth);
  let player = if (Reprocessing.Env.keyPressed(Reprocessing.Events.Space, env)) {
    switchSkin(player)
  } else {
    player
  };
  {
    ...state,
    userInput,
    stones,
    camera,
    player: {...player, facingLeft, walkTimer}
  }
};

let touchStart = (state, ctx, env) => {
  List.fold_left((state, (id, x, y)) => {
    let pos = {Geom.x, y};
    switch state.player.throw {
    | Some(_) => state
    | None => {
      switch (joystickButton(pos, env)) {
      | Some(_) => state
      | None => {
        if (Geom.Rect.testPoint(Play_draw.spritePickerPos(env), Geom.fromIntTuple(Reprocessing.Env.mouse(env)))) {
          {...state, player: switchSkin(state.player)}
        } else {
          let throw = Some((pos, Geom.v0, {
            let size = Random.float(10.) +. 5.;
            {Stone.vel: Geom.v0, circle: {Geom.Circle.rad: size, center: {Geom.x: 0., y: 0.}},
              rotation: Random.float(Geom.tau),
            }
          }, id));

          {...state, player: {...state.player, throw}}
        }
      }
      }
    }
    }
  }, state, Reprocessing.Env.changedTouches(env))
};

let touchMove = (state, ctx, env) => {
  switch (state.player.throw) {
  | None => state
  | Some((p1, vec, item, touch)) => {
    switch (List.find(((id, x, y)) => id == touch, Reprocessing.Env.changedTouches(env))) {
    | exception Not_found => state
    | (id, x, y) => {

      let pos = Geom.fromTuple((x, y));
      {...state, player: {...state.player, throw: Some((p1, Geom.pectorToVector(Geom.pdiff(pos, p1)) |> x => Geom.limitVector(x, 200.), item, id))}}
    }
    }
  }
  }
};

let touchEnd = (state, ctx, env) => {
  switch state.player.throw {
  | None => state
  | Some((p1, vec, {Stone.circle: {rad}, rotation} as stone, touch)) => {
    switch (List.find(((id, x, y)) => id == touch, Reprocessing.Env.changedTouches(env))) {
    | exception Not_found => state
    | (id, x, y) => {

      let vel = Geom.limitVector(Geom.scaleVector(vec, 0.1), 25. -. rad);
      let pos = Play_draw.rockPos(state, vec, stone);
      let circle = Geom.Circle.{center: pos, rad: rad};
      let circle = {
        let center = circle.Geom.Circle.center;
        let notFree = Play_collide.blockCollision(center, Play_collide.testCircle(circle, Geom.p0), state.blocks);
        if (notFree) {
          Geom.Circle.{...circle, center: Geom.addPoints(state.player.box.pos, {Geom.x: 0., y: -40.})}
        } else {
          circle
        }
      };
      let stones = [{
        Stone.vel: Geom.addVectors(vel, state.player.vel),
        circle,
        rotation,
        }, ...state.stones];
      let player = {...state.player,
        throw: None,
        vel: Geom.addVectors(state.player.vel, Geom.scaleVector(vel, -0.01 *. rad))};

      {...state, player, stones}

    }
  };
}};
};
