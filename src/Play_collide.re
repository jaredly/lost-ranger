open Play_types;

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

let collide = (rect, vel, blocks) => blockCollide(
  rect.Geom.Rect.pos,
  vel,
  (move, aabb) => Geom.Rect.testAabb(Geom.Rect.ptranslate(rect, move), aabb),
  (vel, move, aabb) => Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb),
  blocks
);

module type MoveConfig = {
  let gravity: float;
  let friction: float;
  let jump: float;
};

let module Mover = (Config: MoveConfig) => {
  let moveObject = (pos, vel, acc, jumping, checkCollision, vecToCollision, blocks) => {
    let groundShift = {Geom.dx: 0., dy: 0.1};
    let isOnGround = abs_float(Geom.vy(vel)) < 0.01 && blockCollision(Geom.addPectorToPoint(groundShift, pos), checkCollision(groundShift), blocks);
    let vel = isOnGround
      ? (acc === Geom.v0 ? Geom.scaleVector(vel, Config.friction) : vel)
      : Geom.addVectors(Geom.{magnitude: Config.gravity, theta: pi /. 2.}, vel);
    let vel = Geom.addVectors(vel, acc);
    let vel = (isOnGround && jumping)
      ? Geom.addVectors(vel, Geom.{magnitude: Config.jump, theta: -.pi /. 2.})
      : vel;
    let vel = blockCollide(pos, vel, checkCollision, vecToCollision, blocks);
    vel
  };
};

let testRect = (rect, move, aabb) => Geom.Rect.testAabb(Geom.Rect.ptranslate(rect, move), aabb);
let collideRect = (rect, vel, move, aabb) => Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb);

let testCircle = (circle, move, aabb) => Geom.Aabb.testCircle(aabb, Geom.Circle.ptranslate(circle, move));
let collideCircle = (circle, vel, move, aabb) => Geom.Aabb.vectorToCircle(aabb, Geom.Circle.ptranslate(circle, move));
