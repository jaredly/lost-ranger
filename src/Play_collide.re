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

let below = [
  (0, 1),
  (-1, 1),
  (1, 1)
];

let blockCollision = (center, check, blocks) => {
  let x = int_of_float(center.Geom.x /. blockSize);
  let y = int_of_float(center.Geom.y /. blockSize);
  List.exists(
    ((dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (hasBlock(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        check(blockBox)
      } else {
        false
      }
    },
    fifteenbox
  )
};

let blockCollisionBelow = (center, check, blocks) => {
  let x = int_of_float(center.Geom.x /. blockSize);
  let y = int_of_float(center.Geom.y /. blockSize);
  List.exists(
    ((dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (hasBlock(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        check(blockBox)
      } else {
        false
      }
    },
    below
  )
};

let blockCollide = (center, vel, bounce, check, getCollisionVector, blocks) => {
  let x = int_of_float(center.Geom.x /. blockSize);
  let y = int_of_float(center.Geom.y /. blockSize);
  List.fold_left(
    ((moved, vel, hits), (dx, dy)) => {
      let x = x + dx; let y = y + dy;
      if (hasBlock(blocks, (x,y))) {
        let blockBox = Geom.Aabb.init(float_of_int(x) *. blockSize, float_of_int(y) *. blockSize, blockSize, blockSize);
        if (check(moved, blockBox)) {
          let add = getCollisionVector(vel, moved, blockBox);
          /* let add = Geom.scaleVector(add, 0.5); */
          /* let add = bounce ? {
            open Geom;
            let amount = vx({...vel, theta: vel.theta -. add.theta}) *. 0.9;
            let amount = abs_float(amount) < 1. ? 0. : amount;
            /* print_endline(string_of_float(amount)); */
            {...add, magnitude: add.magnitude -. amount}
          } : add;
          let vel = bounce ? Geom.scaleVector(vel, 0.95) : vel; */
          let newVel = Geom.addVectors(add, vel);
          (Geom.vectorToPector(newVel), newVel, [(vel, newVel, (x, y), moved, blockBox), ...hits])
        } else {
          (moved, vel, hits)
        }
      } else {
        (moved, vel, hits)
      }
    },
    (Geom.vectorToPector(vel), vel, []),
    fifteenbox
  ) |> ((moved, vel, hits)) => (vel, hits)
};

let incPoint = blockSize /. 2.;
let incrementalCollide = (center, vel, bounce, check, getCollisionVector, blocks) => {
  if (vel.Geom.magnitude > incPoint) {
    let steps = int_of_float(vel.Geom.magnitude /. incPoint);
    let stepSize = vel.Geom.magnitude /. float_of_int(steps);
    let rec loop = (i) => {
      if (i > steps) {
        (vel, [])
      } else {
        let fi = float_of_int(i);
        let v = Geom.{magnitude: fi *. stepSize, theta: vel.theta};
        let (res, hits) = blockCollide(center, v, bounce, check, getCollisionVector, blocks);
        if (res == v) {
          loop(i + 1)
        } else {
          (res, hits)
        }
      }
    };
    loop(1)
  } else {
    blockCollide(center, vel, bounce, check, getCollisionVector, blocks)
  }
};

/* let hasCollision = (rect, blocks) => blockCollision(rect.Geom.Rect.pos, Geom.Rect.testAabb(rect), blocks);

let collide = (rect, vel, blocks) => blockCollide(
  rect.Geom.Rect.pos,
  vel,
  false,
  (move, aabb) => Geom.Rect.testAabb(Geom.Rect.ptranslate(rect, move), aabb),
  (vel, move, aabb) => Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb),
  blocks
); */

type worldObject('a) = {
  vel: Geom.vector,
  shape: Geom.Shape.t,
  obj: 'a
};

module type MoveConfig = {
  let gravity: float;
  let friction: float;
  let jump: float;
  let bounce: bool;
};

let module Mover = (Config: MoveConfig) => {
  let moveObject = (pos, vel, acc, jumping, checkCollision, vecToCollision, blocks) => {
    let groundShift = {Geom.dx: 0., dy: 0.1};
    let isOnGround = abs_float(Geom.vy(vel)) < 0.01 && blockCollisionBelow(Geom.addPectorToPoint(groundShift, pos), checkCollision(groundShift), blocks);
    let vel = isOnGround
      ? (acc === Geom.v0 ? Geom.scaleVector(vel, Config.friction) : vel)
      : Geom.addVectors(Geom.{magnitude: Config.gravity, theta: pi /. 2.}, vel);
    let vel = Geom.addVectors(vel, acc);
    let vel = (isOnGround && jumping)
      ? Geom.addVectors(vel, Geom.{magnitude: Config.jump, theta: -.pi /. 2.})
      : vel;
    let vel = incrementalCollide(pos, vel, Config.bounce, checkCollision, vecToCollision, blocks);
    (isOnGround, vel)
  };
};

let testRect = (rect, move, aabb) => Geom.Rect.testAabb(Geom.Rect.ptranslate(rect, move), aabb);
let collideRect = (rect, vel, move, aabb) => Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb);

/* let collideRectOld = (rect, vel, move, aabb) => Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb);

let collideRect = (rect, vel, move, aabb) => {
  let delta = GeomCollide.rectToRect(vel, rect, Geom.Rect.fromAabb(aabb));
  if (abs_float(delta.Geom.magnitude) > abs_float(vel.Geom.magnitude) *. 1.1) {
    Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb);
  } else {

  /* let delta = GeomCollide.rectToRect(vel, Geom.Rect.ptranslate(rect, move), Geom.Rect.fromAabb(aabb)); */
  /* let delta = GeomCollide.rectToRect(vel, rect, Geom.Rect.fromAabb(aabb)); */
  /* Geom.addVectors(vel, delta) */
    delta
  }
}; */

/* Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb); */
/* let collideRect = (rect, vel, move, aabb) => {
  let res = GeomCollide.rectToRect(vel, Geom.Rect.ptranslate(rect, move), Geom.Rect.fromAabb(aabb));
  let res = Geom.scaleVector(res, 0.5);
  if (abs_float(res.magnitude) > abs_float(vel.magnitude)) {
    Geom.Rect.collideToAabb(vel, Geom.Rect.ptranslate(rect, move), aabb);
  } else {
    res
  }
}; */

let testCircle = (circle, move, aabb) => Geom.Aabb.testCircle(aabb, Geom.Circle.ptranslate(circle, move));
let collideCircle = (circle, vel, move, aabb) => Geom.Aabb.collideToCircle(vel, aabb, Geom.Circle.ptranslate(circle, move));
/* let collideCircle = (circle, vel, move, aabb) => Geom.Aabb.vectorToCircle(aabb, Geom.Circle.ptranslate(circle, move)); */
