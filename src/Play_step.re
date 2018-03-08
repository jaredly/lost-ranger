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

let gameWidth = blockSize *. 100.;

let start = (env) => {
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let height = Reprocessing.Env.height(env) |> float_of_int;

  let ground = 20;
  let cols = int_of_float(width /. blockSize);

  let blocks = Array.make_matrix(500, 100, None);
  /* let blocks = Hashtbl.create(1000); */
  let d = ref(0);
  for (x in 0 to 99) {
    let change = Random.float(10.) > 4.;
    if (change) {
      let off = ceil(sqrt(Random.float(16.)));
      d := min(10, max(-10, d^ + Random.int(int_of_float(off)) - (int_of_float(off /. 2.))));
    };
    /* let d = Random.int(5); */
    for (y in d^ to 50) {
      /* print_endline(string_of_int(x) ++ " " ++ string_of_int(ground + y)); */
      blocks[ground + y][x] = Some(Block.init(Block.Dirt));
      /* Hashtbl.add(blocks, (x, ground + y), Block.init(Block.Dirt)) */
    };
  };
  print_endline("populated");

  /* for (y in 0 to ground - 1) {
    Hashtbl.add(blocks, (0, y), Block.init(Block.Rock));
    Hashtbl.add(blocks, (cols - 1, y), Block.init(Block.Rock))
  }; */

  /* for (y in 0 to 3) {
    Hashtbl.replace(blocks, (y + cols - 10, ground - y), Block.init(Block.Rock))
  }; */

  let player = {
    vel: Geom.v0,
    box: Geom.Rect.create({Geom.y: float_of_int(ground - 5) *. blockSize -. blockSize *. 1.9, x: blockSize}, blockSize *. 0.7, blockSize *. 1.4),
    throwSkill: 0.,
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
    animals: [],
    trees: [],
    blocks,
    looseFruit: [],
    stones: [],
    userInput: {
      left: false, right: false, jump: false, action: false, throw: None,
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
  /* print_endline(string_of_float(vel.Geom.magnitude)); */
  {vel, circle: {...stone.circle, center: wrap(Geom.addVectorToPoint(vel, stone.circle.center), gameWidth)}}
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
  throw: if (Env.mousePressed(env)) {
    let pos = Geom.fromIntTuple(Env.mouse(env));
    switch prev.throw {
    | None => Some((pos, Geom.v0))
    | Some((p1, p2)) => {
      /* let vec =  */
      Some((p1, Geom.pectorToVector(Geom.pdiff(pos, p1)) |> x => Geom.limitVector(x, 200.)))
    }
    }
  } else {
    None
  }
});

let step = (state, context, env) => {
  let userInput = getUserInput(state.userInput, env);

  let stones = moveStones(state.stones, state.blocks);
  let stones = userInput.throw == None
  ? switch state.userInput.throw {
  | None => stones
  | Some((p1, vec)) => {
    [Stone.{
      vel: Geom.limitVector(Geom.scaleVector(vec, 0.1), 20.),
      circle: Geom.Circle.{center: state.player.box.pos, rad: 7.}
    }, ...stones]
  }
  } : stones;

  let player = movePlayer(userInput, state.player, state.blocks);
  let inputLeft = userInput.left ? true : (userInput.right ? false : player.facingLeft);
  let facingLeft = switch userInput.throw {
  | None => inputLeft
  | Some((_, v)) => v.Geom.magnitude > 5. ? Geom.vx(v) < 0. : inputLeft
  };
  let walkTimer = (userInput.left || userInput.right ? player.walkTimer +. Reprocessing.Env.deltaTime(env) : 0.);
  let (camera, player) = followPlayer(player, state.camera, gameWidth);
  {
    ...state,
    userInput,
    stones,
    camera,
    player: {...player, facingLeft, walkTimer}
  }
};
