open Play_types;

let followPlayer = ({Geom.x, y}, (a, b, w, h)) => {
  let padding = w /. 3.;
  (
    max(min(x -. padding, a), x +. padding -. w),
    max(min(y -. padding, b), y +. padding -. h),
    w,
    h
  )
};

let start = (env) => {
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let height = Reprocessing.Env.height(env) |> float_of_int;

  let ground = int_of_float(height /. blockSize) * 2 / 3;
  let cols = int_of_float(width /. blockSize);

  let blocks = Hashtbl.create(1000);
  let d = ref(0);
  for (x in 0 to cols * 3) {
    d := d^ + Random.int(4) - 2;
    /* let d = Random.int(5); */
    for (y in d^ to 10) {
      Hashtbl.add(blocks, (x, ground + y), Block.init(Block.Dirt))
    };
  };

  /* for (y in 0 to ground - 1) {
    Hashtbl.add(blocks, (0, y), Block.init(Block.Rock));
    Hashtbl.add(blocks, (cols - 1, y), Block.init(Block.Rock))
  }; */

  for (y in 0 to 3) {
    Hashtbl.replace(blocks, (y + cols - 10, ground - y), Block.init(Block.Rock))
  };

  let player = {
    vel: Geom.v0,
    box: Geom.Rect.create({Geom.y: float_of_int(ground - 5) *. blockSize -. blockSize *. 1.9, x: blockSize}, blockSize *. 0.8, blockSize *. 1.8),
    throwSkill: 0.,
    inventory: {
      Inventory.size: 20,
      slots: Array.make(20, Inventory.Nothing),
      selected: 0
    }
  };

  {
    animals: [],
    trees: [],
    blocks,
    looseFruit: [],
    stones: [],
    userInput: {
      left: false, right: false, jump: false, action: false, throw: None,
    },
    camera: followPlayer(player.box.pos, (0., 0., width, height)),
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
  let vel = MovePlayer.moveObject(player.box.pos, player.vel, acc, userInput.jump, Play_collide.testRect(player.box), Play_collide.collideRect(player.box), blocks);
  {...player, vel, box: Geom.Rect.push(player.box, vel)}
};

let moveStone = (stone, otherStones, blocks) => {
  open! Stone;
  let vel = MoveStone.moveObject(
    stone.circle.center,
    stone.vel,
    Geom.v0,
    false,
    Play_collide.testCircle(stone.circle),
    Play_collide.collideCircle(stone.circle),
    blocks
  );
  /* print_endline(string_of_float(vel.Geom.magnitude)); */
  {vel, circle: Geom.Circle.push(stone.circle, vel)}
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
  {
    ...state,
    userInput,
    stones,
    camera: followPlayer(player.box.pos, state.camera),
    player
  }
};