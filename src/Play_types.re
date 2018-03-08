
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
    damage: float,
    stuckArrows: list(Arrow.t),
  };
  let init = kind => {kind, damage: 0., stuckArrows: []};
};

module Fruit = {
  type kind = Apple | Peach;
  type t = {kind, vel: Geom.vector, circle: Geom.Circle.t}
};

module Stone = {
  type t = {circle: Geom.Circle.t, vel: Geom.vector};
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
  /* throwing: option((Geom.vector, Projectile.kind)), */
  inventory: Inventory.t,
  /* crouching, crouchingBox */
};

type userInput = {
  left: bool,
  right: bool,
  jump: bool,
  action: bool,
  throw: option((Geom.point, Geom.vector)),
};

type state = {
  blocks: Hashtbl.t((int, int), Block.t),
  animals: list(Animal.t),
  trees: list(FruitTree.t),
  looseFruit: list(Fruit.t),
  stones: list(Stone.t),
  camera: (float, float, float, float),
  userInput,
  player
};

type status =
  /* | AnimateIn(option((state, Timer.t, score)), state, Timer.t) */
  | Playing(state)
  ;

let blockSize = 40.;