open Play_types;

/* let at = (state, x, y) => {

}; */

let start = (env) => {
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let height = Reprocessing.Env.height(env) |> float_of_int;

  let ground = int_of_float(height /. blockSize) * 2 / 3;
  let cols = int_of_float(width /. blockSize);

  let blocks = Hashtbl.create(1000);
  for (y in 0 to 10) {
    for (x in 0 to cols) {
      Hashtbl.add(blocks, (x, ground + y), Block.init(Block.Dirt))
    };
  };

  for (y in 0 to ground - 1) {
    Hashtbl.add(blocks, (0, y), Block.init(Block.Rock));
    Hashtbl.add(blocks, (cols - 1, y), Block.init(Block.Rock))
  };

  for (y in 0 to 10) {
    Hashtbl.add(blocks, (y + cols - 10, ground - y), Block.init(Block.Rock))
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
    player: {
      vel: Geom.v0,
      box: Geom.Rect.create({Geom.y: float_of_int(ground) *. blockSize -. blockSize *. 1.9, x: blockSize *. 4.1}, blockSize *. 0.8, blockSize *. 1.8),
      throwSkill: 0.,
      inventory: {
        Inventory.size: 20,
        slots: Array.make(20, Inventory.Nothing),
        selected: 0
      }
    }
  }
};

let getUserInput = (prev, env) => Reprocessing.({
  left: Env.key(Events.Left, env),
  right: Env.key(Events.Right, env),
  jump: Env.key(Events.Up, env),
  action: false,
  throw: None
});

/* let find_opt = (h, k) => Hashtbl.mem(h, k) ? Some(Hashtbl.find(h, k)) : None; */


let maxWalk = 5.;
let walkSpeed = 0.5;

let module MovePlayer = Play_collide.Mover({
  let gravity = 0.5;
  let friction = 0.7;
  let jump = 15.;
});

let movePlayer = (userInput, player, blocks) => {
  let vx = Geom.vx(player.vel);
  let acc = userInput.left ? (vx > -. maxWalk ? Geom.{magnitude: walkSpeed, theta: pi} : Geom.v0) : (
    userInput.right ? (vx < maxWalk ? Geom.{magnitude: walkSpeed, theta: 0.} : Geom.v0) : Geom.v0
  );
  let vel = MovePlayer.moveObject(player.box.pos, player.vel, acc, userInput.jump, Play_collide.testRect(player.box), Play_collide.collideRect(player.box), blocks);
  {...player, vel, box: Geom.Rect.push(player.box, vel)}
};

let step = (state, context, env) => {
  let userInput = getUserInput(state.userInput, env);
  {
    ...state,
    userInput,
    player: movePlayer(userInput, state.player, state.blocks)
  }
};