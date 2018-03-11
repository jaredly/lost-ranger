
type line = ((float, float), (float, float));

let module LineSet = Set.Make({
  type t = line;
  let compare = compare;
});

module Animal = {
  type shape = {
    collision: Geom.Polygon.t,
    visual: Geom.Polygon.t,
    box: Geom.Aabb.t,
  };

  type t = {
    pos: Geom.point,
    walkingShape: shape,
    eatingShape: shape,
  };
};

module Arrow = {
  type kind = | Wood | Stone | Iron;
  type t = {
    pos: Geom.point,
    vel: Geom.vector,
    kind,
    length: float,
  };
};

module Block = {
  type blockKind = | Dirt | Rock;
  type t = {
    kind: blockKind,
    top: bool,
    damage: float,
    stuckArrows: list(Arrow.t),
  };
  let init = (kind, top) => {kind, damage: 0., stuckArrows: [], top};
};

module Fruit = {
  type kind = Apple | Peach;
  type t = {kind, vel: Geom.vector, circle: Geom.Circle.t, rotation: float}
};

module Stone = {
  type t = {circle: Geom.Circle.t, vel: Geom.vector, rotation: float};
  let push = (stone, vec) => {...stone, circle: Geom.Circle.push(stone.circle, vec)};
  let translate = (stone, pos) => {...stone, circle: Geom.Circle.translate(stone.circle, pos)};
  let force = (stone, vec) => {...stone, vel: Geom.addVectors(stone.vel, vec)};
};

module FruitTree = {
  type t = {
    pos: Geom.point,
    growTimer: Timer.t,
    fruitTimer: Timer.t,
    fruitBox: Geom.Aabb.t,
    fruit: list((Fruit.t, Timer.t))
  };
};

module Projectile = {
  type kind = Stone | Fruit(Fruit.kind) | Arrow(Arrow.kind);
};

module Inventory = {
  type item =
    | Nothing
    | Fruit(Fruit.kind)
    | Stone
    | Arrow(Arrow.kind);
  type t = {
    size: int,
    slots: array(item),
    selected: int,
  };
};

type player = {
  /* pos: Geom.point, */
  vel: Geom.vector,
  box: Geom.Rect.t,
  throwSkill: float,
  facingLeft: bool,
  isOnGround: bool,
  walkTimer: float,
  inventory: Inventory.t,
  throw: option((Geom.point, Geom.vector, Stone.t, float)),
  skin: int,
};

type userInput = {
  left: bool,
  right: bool,
  jump: bool,
  action: bool,
};

type state = {
  blocks: array(array(option(Block.t))),
  textPos: list((string, Geom.point)),
  animals: list(Animal.t),
  trees: list(FruitTree.t),
  looseFruit: list(Fruit.t),
  stones: list(Stone.t),
  camera: (float, float, float, float),
  lastSavedHighScore: int,
  paused: bool,
  userInput,
  player
};

type status =
  /* | AnimateIn(option((state, Timer.t, score)), state, Timer.t) */
  | Playing(state)
  ;

let blockSize = 40.;

let getBlock = (blocks, (x, y)) => {
  let maxy = Array.length(blocks) - 1;
  if (y < 0 || y > maxy) {
    None
  } else {
    let maxx = Array.length(blocks[0]) - 1;
    let x = x < 0 ? maxx + 1 + x : (x > maxx ? x - maxx - 1: x);
    blocks[y][x]
  }
};

let hasBlock = (blocks, (x, y)) => {
  let maxy = Array.length(blocks) - 1;
  if (y < 0 || y > maxy) {
    false
  } else {
    let maxx = Array.length(blocks[0]) - 1;
    let x = x < 0 ? maxx + 1 + x : (x > maxx ? x - maxx - 1 : x);
    blocks[y][x] != None
  }
};