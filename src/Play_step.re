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
  let padding = w /. 3.;
  ((
    max(min(x -. padding, a), x +. padding -. w),
    max(min(y -. padding, b), y +. padding -. h),
    w,
    h
  ), player)
};

let gameWidth = Play_draw.gameWidth;

let start = (env) => {
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let height = Reprocessing.Env.height(env) |> float_of_int;

  let ground = 20;
  let cols = int_of_float(width /. blockSize);

  let textPositions = ref([]);

  let blocks = Array.make_matrix(500, Play_draw.gameWidthInt, None);
  let d = ref(0);
  for (x in 0 to Play_draw.gameWidthInt - 1) {
    let change = Random.float(10.) > 4.;
    if (change && x > 2) {
      let off = ceil(sqrt(Random.float(16.)));
      d := min(10, max(-10, d^ + Random.int(int_of_float(off)) - (int_of_float(off /. 2.))));
    };

    if (x mod int_of_float(width /. blockSize) == 0) {
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
    loop(Play_draw.pithyTexts |> List.rev, textPositions^)
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

/* let ensureStoneIsFree = (circle, vel, blocks) => {
  let center = circle.Geom.Circle.center;
  let notFree = Play_collide.blockCollision(center, Play_collide.testCircle(circle, Geom.p0), blocks);
  if (notFree) {
    let rec loop = (circle, i) => {
      let notFree = Play_collide.blockCollision(center, Play_collide.testCircle(circle, Geom.p0), blocks);
      if (notFree && i < 10) {

        let fake = Geom.invertVector(vel);
        let (_, push) = MoveStone.moveObject(
          Geom.addVectorToPoint(vel, center),
          /* center, */
          fake,
          /* Geom.v0, */
          Geom.v0,
          false,
          Play_collide.testCircle(circle),
          Play_collide.collideCircle(circle),
          blocks
        );
        let push = Geom.addVectors(vel, push);

        Printf.printf("Push: %0.2f %0.2f", push.Geom.theta, push.Geom.magnitude);
        print_newline();
        loop(Geom.Circle.push(circle, push), i + 1)
      } else {
        circle
      }
    };
    loop(circle, 0)
  } else {
    circle
  }
}; */

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
  /* print_endline(string_of_float(vel.Geom.magnitude)); */
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
  let broad = Array.make(boxes, []);
  let mutableStones = List.mapi((i, s) => ref((s, false, i)), stones);
  List.iter(stoneRef => {
    let (stone, _, _) = stoneRef.contents;
    let {Geom.Circle.center: {Geom.x}, rad} = stone.Stone.circle;
    let leftBox = (x -. rad) /. boxWidth |> floor |> int_of_float;
    let leftBox = leftBox == -1 ? boxes - 1 : leftBox;
    let rightBox = (x +. rad) /. boxWidth |> floor |> int_of_float;
    broad[leftBox] = [stoneRef, ...broad[leftBox]];
    if (leftBox != rightBox) {
      let rightBox = rightBox == boxes ? 0 : rightBox;
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
          let (a, _, ai) = a^;
          let (b, _, bi) = b^;
          let key = (min(ai, bi), max(ai, bi));
          if (Hashtbl.mem(pairHash, key)) {
            false
          } else {
            if (Geom.Circle.testCircle(a.Stone.circle, b.Stone.circle)) {
              Hashtbl.replace(pairHash, key, true);
              true
            } else if (checkLoop) {
              let dx = a.circle.center.x -. b.circle.center.x;
              /* We've looped */
              if (abs_float(dx) > boxWidth) {
                if (a.circle.center.x > b.circle.center.x) {
                  Geom.Circle.testCircle(Geom.Circle.translate(a.circle, {Geom.x: -.gameWidth, y: 0.}), b.circle)
                } else {
                  Geom.Circle.testCircle(Geom.Circle.translate(b.circle, {Geom.x: -.gameWidth, y: 0.}), a.circle)
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
        let (one, _, ai) = oneRef^;
        let (other, _, bi) = otherRef^;
        let v = if (checkLoop) {
          let dx = one.circle.center.x -. other.circle.center.x;
          /* We've looped */
          if (abs_float(dx) > boxWidth) {
            if (one.circle.center.x > other.circle.center.x) {
              Geom.Circle.vectorToCircle(Geom.Circle.translate(one.circle, {Geom.x: -.gameWidth, y: 0.}), other.circle)
            } else {
              Geom.Circle.vectorToCircle(one.circle, Geom.Circle.translate(other.circle, {Geom.x: -.gameWidth, y: 0.}))
            }
          } else {
            Geom.Circle.vectorToCircle(one.circle, other.circle);
          }
        } else {
          Geom.Circle.vectorToCircle(one.circle, other.circle);
        };
         /* |> x => Geom.scaleVector(x, 0.5); */
        let first = (one.Stone.circle.rad *. one.Stone.circle.rad);
        let second = (other.Stone.circle.rad *. other.Stone.circle.rad);
        let fshare = first /. (first +. second);
        /* |> x => Geom.scaleVector(x, 0.25); */
        oneRef := (Stone.force(one, v |> Geom.invertVector |> x => Geom.scaleVector(x, 1. -. fshare)), true, ai);
        otherRef := (Stone.force(other, Geom.scaleVector(v, fshare)), true, bi);
      }, pairs)
    }
    }
  }, broad);
  /* Some cool down */
  List.map(x => {
    let (stone, hit, _) = x^;
    if (hit) {
      let vel = Geom.scaleVector(stone.Stone.vel, 0.8);
      /* let vel = abs_float(vel.Geom.magnitude) < 1. ? Geom.v0 : vel; */
      {...stone, Stone.vel}
    } else {
      stone
    }
  }, mutableStones)
};

let step = (state, context, env) => {
  let userInput = getUserInput(state.userInput, env);

  let stones = collideStones(state.stones);
  let stones = moveStones(stones, state.blocks);
  let player = state.player;
  let (stones, player) =

  Reprocessing.(if (Env.mousePressed(env)) {
    let pos = Geom.fromIntTuple(Env.mouse(env));
    (stones, {...player, throw: switch player.throw {
    | None => Some((pos, Geom.v0, {
        let size = Random.float(10.) +. 5.;
        {Stone.vel: Geom.v0, circle: {Geom.Circle.rad: size, center: {Geom.x: 0., y: 0.}},
          rotation: Random.float(Geom.tau),
        }
      }))
    | Some((p1, p2, item)) => {
      /* let vec =  */
      Some((p1, Geom.pectorToVector(Geom.pdiff(pos, p1)) |> x => Geom.limitVector(x, 200.), item))
    }
    }})
  } else {
    switch state.player.throw {
    | None => (stones, player)
    | Some((p1, vec, {Stone.circle: {rad}, rotation} as stone)) => {
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
      ([{
        Stone.vel: Geom.addVectors(vel, state.player.vel),
        circle,
        rotation,
        }, ...stones], {...player,
        throw: None,
        vel: Geom.addVectors(player.vel, Geom.scaleVector(vel, -0.01 *. rad))})
      }
    }
  });


  /* let player */
  let player = movePlayer(userInput, player, state.blocks);
  let inputLeft = userInput.left ? true : (userInput.right ? false : player.facingLeft);
  let facingLeft = switch player.throw {
  | None => inputLeft
  | Some((_, v, _)) => v.Geom.magnitude > 5. ? Geom.vx(v) < 0. : inputLeft
  };
  let walkTimer = (userInput.left || userInput.right ? player.walkTimer +. Reprocessing.Env.deltaTime(env) : 0.);
  let (camera, player) = followPlayer(player, state.camera, gameWidth);
  let player = if (Reprocessing.Env.keyPressed(Reprocessing.Events.Space, env)) {
    let skin = player.skin + 1;
    {...player, skin, box: Play_draw.spriteBox(player.box.pos, skin, player.box.height)};
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
