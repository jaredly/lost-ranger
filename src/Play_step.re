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

let find_opt = (h, k) => Hashtbl.mem(h, k) ? Some(Hashtbl.find(h, k)) : None;

/** The ninesquare plus 3 on top & bottom */
let fifteenbox = [
  (0, 0),
  (0, 1),
  (0, -1),
  (1, 0),
  (-1, 0),
  (-1, 1),
  (1, -1),
  (-1, -1),
  (1, 1),
  (0, 2),
  (0, -2),
  (1, 2),
  (1, -2),
  (-1, 2),
  (-1, -2)
];

let blockCollision = (center, check, blocks) => {
  let x = int_of_float(center.Geom.x /. blockSize);
  let y = int_of_float(center.Geom.y /. blockSize);
  List.exists(
    ((dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (Hashtbl.mem(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        check(blockBox)
      } else {
        false
      }
    },
    fifteenbox
  )
};

let blockCollide = (center, vel, check, getCollisionVector, blocks) => {
  let x = int_of_float(center.Geom.x /. blockSize);
  let y = int_of_float(center.Geom.y /. blockSize);
  List.fold_left(
    ((moved, vel), (dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (Hashtbl.mem(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        if (check(moved, blockBox)) {
          let add = getCollisionVector(vel, moved, blockBox);
          let vel = Geom.addVectors(add, vel);
          (Geom.vectorToPector(vel), vel)
        } else {
          (moved, vel)
        }
      } else {
        (moved, vel)
      }
    },
    (Geom.vectorToPector(vel), vel),
    fifteenbox
  ) |> snd
};

let hasCollision = (rect, blocks) => blockCollision(rect.Geom.Rect.pos, Geom.Rect.testAabb(rect), blocks);

/* let hasCollision = (rect, blocks) => {
  let x = int_of_float(rect.Geom.Rect.pos.x /. blockSize);
  let y = int_of_float(rect.Geom.Rect.pos.y /. blockSize);
  List.exists(
    ((dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (Hashtbl.mem(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        Geom.Rect.testAabb(rect, blockBox)
      } else {
        false
      }
    },
    fifteenbox
  )
}; */

let collide = (rect, vel, blocks) => blockCollide(
  rect.Geom.Rect.pos,
  vel,
  (move, aabb) => Geom.Rect.testAabb(Geom.Rect.ptranslate(rect, move), aabb),
  (vel, move, aabb) => Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb),
  blocks
);

/* let collide = (rect, vel, blocks) => {
  let x = int_of_float(rect.Geom.Rect.pos.x /. blockSize);
  let y = int_of_float(rect.Geom.Rect.pos.y /. blockSize);
  List.fold_left(
    ((moved, vel), (dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (Hashtbl.mem(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        if (Geom.Rect.testAabb(moved, blockBox)) {
          let add = Geom.Rect.collideToAabb(vel, moved, blockBox);
          let vel = Geom.addVectors(add, vel);
          (Geom.Rect.push(rect, vel), vel)
        } else {
          (moved, vel)
        }
      } else {
        (moved, vel)
      }
    },
    (Geom.Rect.push(rect, vel), vel),
    fifteenbox
  ) |> snd
}; */


let walkSpeed = 0.5;
let maxWalk = 5.;
let gravity = 0.5;
let friction = 0.7;
let jump = 15.;



let movePlayer = (userInput, player, blocks) => {
  let vx = Geom.vx(player.vel);
  let acc = userInput.left ? (vx > -. maxWalk ? Geom.{magnitude: walkSpeed, theta: pi} : Geom.v0) : (
    userInput.right ? (vx < maxWalk ? Geom.{magnitude: walkSpeed, theta: 0.} : Geom.v0) : Geom.v0
  );
  let isOnGround = abs_float(Geom.vy(player.vel)) < 0.01 && hasCollision(Geom.Rect.translate(player.box, {Geom.x: 0., y: 2.}), blocks);
  let vel = player.vel;
  let vel = isOnGround
    ? (acc === Geom.v0 ? Geom.scaleVector(vel, friction) : vel)
    : Geom.addVectors(Geom.{magnitude: gravity, theta: pi /. 2.}, vel);
  let vel = Geom.addVectors(vel, acc);
  let vel = (isOnGround && userInput.jump)
    ? Geom.addVectors(vel, Geom.{magnitude: jump, theta: -.pi /. 2.})
    : vel;
  let vel = collide(player.box, vel, blocks);
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