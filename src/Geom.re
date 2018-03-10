
module Ease = {
  let linear = t => t;
  /* // accelerating from zero velocity */
  let easeInQuad = t => t *. t;
  /* // decelerating to zero velocity */
  let easeOutQuad = t => t*. (2. -. t);
  /* // acceleration until halfway, then deceleration */
  let easeInOutQuad = (t) => t<0.5 ? 2. *. t*. t : -1. +. (4. -. 2. *. t)*. t;
  /* // accelerating from zero velocity  */
  /* easeInCubic: function (t) { return t*t*t },
  // decelerating to zero velocity
  easeOutCubic: function (t) { return (--t)*t*t+1 },
  // acceleration until halfway, then deceleration
  easeInOutCubic: function (t) { return t<.5 ? 4*t*t*t : (t-1)*(2*t-2)*(2*t-2)+1 },
  // accelerating from zero velocity
  easeInQuart: function (t) { return t*t*t*t },
  // decelerating to zero velocity
  easeOutQuart: function (t) { return 1-(--t)*t*t*t },
  // acceleration until halfway, then deceleration
  easeInOutQuart: function (t) { return t<.5 ? 8*t*t*t*t : 1-8*(--t)*t*t*t },
  // accelerating from zero velocity
  easeInQuint: function (t) { return t*t*t*t*t },
  // decelerating to zero velocity
  easeOutQuint: function (t) { return 1+(--t)*t*t*t*t },
  // acceleration until halfway, then deceleration
  easeInOutQuint: function (t) { return t<.5 ? 16*t*t*t*t*t : 1+16*(--t)*t*t*t*t } */
};

/* http://www.metanetsoftware.com/technique/tutorialA.html */

type point = {x: float, y: float};
type vector = {magnitude: float, theta: float};
let v0 = {magnitude: 0., theta: 0.};
type pector = {dx: float, dy: float};
let p0 = {dx: 0., dy: 0.};
let origin = {x: 0., y: 0.};

let tuple = ({x, y}) => (x, y);
let intTuple = ({x, y}) => (int_of_float(x), int_of_float(y));
let fromIntTuple = ((x, y)) => {x: float_of_int(x), y: float_of_int(y)};
let fromTuple = ((x, y)) => {x, y};
let dist = (p1, p2) => {
  let dx = p2.x -. p1.x;
  let dy = p2.y -. p1.y;
  sqrt(dx *. dx +. dy *. dy)
};

let scalePector = ({dx, dy}, scale) => {dx: dx *. scale, dy: dy *. scale};

let pdist = ({dx, dy}) => sqrt(dx *. dx +. dy *. dy);
let pdiff = (p1, p2) => {dx: p2.x -. p1.x, dy: p2.y -. p1.y};

let addVectorToPoint = ({magnitude, theta}, {x, y}) => {
  {x: x +. cos(theta) *. magnitude, y: y +. sin(theta) *. magnitude}
};
let vx = ({magnitude, theta}) => cos(theta) *. magnitude;
let vy = ({magnitude, theta}) => sin(theta) *. magnitude;
let vectorToPector = ({magnitude, theta}) => {dx: cos(theta) *. magnitude, dy: sin(theta) *. magnitude};
let angleTo = (p1, p2) => atan2(p2.y -. p1.y, p2.x -. p1.x);
let pectorToVector = (p) => {
  magnitude: pdist(p),
  theta: atan2(p.dy, p.dx)
};
let vectorBetweenPoints = (p1, p2) => pectorToVector(pdiff(p1, p2));
let addPectors = (p1, p2) => {dx: p1.dx +. p2.dx, dy: p1.dy +. p2.dy};
let clampVector = ({magnitude, theta}, maxMag) => {magnitude: min(maxMag, magnitude), theta};
let addVectors = (v1, v2) => addPectors(vectorToPector(v1), vectorToPector(v2)) |> pectorToVector;
let addPectorToVector = (p, v) => addPectors(p, vectorToPector(v)) |> pectorToVector;
let invertVector = ({magnitude, theta}) => {magnitude, theta: theta +. 3.14159};
let invertPector = ({dx, dy}) => {dx: -.dx, dy: -.dy};
let scaleVector = ({magnitude, theta}, scale) => {theta, magnitude: magnitude *. scale};
let addMagnitude = ({magnitude, theta}, mag) => {theta, magnitude: magnitude +. mag};
let limitVector = ({magnitude, theta}, maxM) => {theta, magnitude: min(maxM, max(-.maxM, magnitude))};
let addPoints = (p1, p2) => {x: p1.x +. p2.x, y: p1.y +. p2.y};
let addPectorToPoint = ({dx, dy}, {x, y}) => {x: x +. dx, y: y +. dy};

let flipX = ({x, y}) => {x: -.x, y};
let flipY = ({x, y}) => {x, y: -.y};
let invertPoint = ({x, y}) => {x: -.x, y: -.y};
let flipXY = ({x, y}) => {x: y, y: x};

let lerpPos = (p1, p2, amount) => {
  let dx = p2.x -. p1.x;
  let dy = p2.y -. p1.y;
  {x: p1.x +. dx *. amount, y: p1.y +. dy *. amount}
};

let lerpTuples = ((x1, y1), (x2, y2), amount) => {
  let dx = x2 -. x1;
  let dy = y2 -. y1;
  (x1 +. dx *. amount, y1 +. dy *. amount)
};

let lerp = (a, b, amount) => a +. (b -. a) *. amount;

module Circle = {
  type t = {rad: float, center: point};
  let translate = ({rad, center}, pos) => {rad, center: addPoints(center, pos)};
  let ptranslate = ({rad, center}, pector) => {rad, center: addPectorToPoint(pector, center)};
  let push = ({rad, center}, vec) => {rad, center: addVectorToPoint(vec, center)};

  let testPoint = ({rad, center}, point) => dist(point, center) <= rad;
  let testCircle = (c1, c2) => dist(c1.center, c2.center) <= c1.rad +. c2.rad;
  let vectorToCircle = (c1, c2) => {
    let magnitude = dist(c1.center, c2.center) -. c1.rad -. c2.rad;
    {magnitude, theta: angleTo(c1.center, c2.center)}
  };
  /** based on http://www.jeffreythompson.org/collision-detection/poly-circle.php */
  let testLine = (c, p1, p2) => {
    testPoint(c, p1) ||
    testPoint(c, p2) || {
      let len = dist(p1, p2);
      let dot = (
        ((c.center.x -. p1.x)*.(p2.x -. p1.x)) +. ((c.center.y -. p1.y) *. (p2.y -. p1.y))
      ) /. (len *. len);

      let closestX = p1.x +. (dot *. (p2.x -. p1.x));
      let closestY = p1.y +. (dot *. (p2.y -. p1.y));

      let xa = min(p1.x, p2.x);
      let xb = max(p1.x, p2.x);
      let ya = min(p1.y, p2.y);
      let yb = max(p1.y, p2.y);

      /* tangent point is within the line segment */
      (xa == xb || (xa <= closestX && closestX <= xb)) &&
      (ya == yb || (ya <= closestY && closestY <= yb)) &&
      /* tangent point is within the circle */
      testPoint(c, {x: closestX, y: closestY})
    }
  };

  /** NOTE this magnitude is too large, you need to subtract circle.rad */
  let vectorToLine = (c, p1, p2) => {
    let len = dist(p1, p2);
    let dot = (
      ((c.center.x -. p1.x)*.(p2.x -. p1.x)) +. ((c.center.y -. p1.y) *. (p2.y -. p1.y))
    ) /. (len *. len);

    let closestX = p1.x +. (dot *. (p2.x -. p1.x));
    let closestY = p1.y +. (dot *. (p2.y -. p1.y));

    let xa = min(p1.x, p2.x);
    let xb = max(p1.x, p2.x);
    let ya = min(p1.y, p2.y);
    let yb = max(p1.y, p2.y);

    /* tangent point is within the line segment */
    if ((xa == xb || (xa <= closestX && closestX <= xb)) &&
        (ya == yb || (ya <= closestY && closestY <= yb))) {
      {
        dx: closestX -. c.center.x,
        dy: closestY -. c.center.y
      }
    } else {
      let p1diff = pdiff(c.center, p1);
      let p2diff = pdiff(c.center, p2);
      let d1 = pdist(p1diff);
      let d2 = pdist(p2diff);
      d1 < d2 ? p1diff : p2diff
    }
  };
};

let pi = 3.14159;
let halfPi = pi /. 2.;
let tau = pi *. 2.;

let rec normalize = x => {
  if (x < -. pi) normalize(x +. tau)
  else if (x > pi) normalize(x -. tau)
  else x
};

let thetaDiff = (a, b) => {
  let d = abs_float(mod_float(abs_float(b -. a), tau));
  if (d > pi) {
    tau -. d
  } else {
    d
  }
};

let isThetaBetween = (low, high, test) => {
  let low = low -. test |> normalize;
  let high = high -. test |> normalize;
  if (low *. high >= 0.) {
    false
  } else {
    abs_float(high -. low) < pi
  }
};

module Arc = {
  type t = {cx: float, cy: float, r: float, t1: float, t2: float};
  let translate = ({cx, cy} as c, {x, y}) => {...c, cx: cx +. x, cy: cy +. y};
  let points = ({cx, cy, r, t1, t2}) => {
    (
      {x: cos(t1) *. r +. cx, y: sin(t1) *. r +. cy},
      {x: cos(t2) *. r +. cx, y: sin(t2) *. r +. cy},
    )
  };

  let testCircle = ({cx, cy, r, t1, t2} as arc, c) => {
    let t2 = (t1 > t2) ? t2 +. tau : t2;
    let (p1, p2) = points(arc);
    let v = pectorToVector({dx: c.Circle.center.x -. cx, dy: c.Circle.center.y -. cy});
    if (isThetaBetween(t1, t2, v.theta)) {
      v.magnitude < r +. c.Circle.rad &&
      v.magnitude > r -. c.Circle.rad
    } else {
      Circle.testPoint(c, p1) || Circle.testPoint(c, p2)
    }
  };

  let vectorToCircle = ({cx, cy, r, t1, t2} as arc, c) => {
    let t2 = (t1 > t2) ? t2 +. tau : t2;
    let (p1, p2) = points(arc);
    let v = pectorToVector({dx: c.Circle.center.x -. cx, dy: c.Circle.center.y -. cy});
    if (isThetaBetween(t1, t2, v.theta)) {
      vectorToPector({magnitude: v.magnitude -. r, theta: v.theta})
    } else {
      let p1diff = pdiff(c.Circle.center, p1);
      let p2diff = pdiff(c.Circle.center, p2);
      let d1 = pdist(p1diff);
      let d2 = pdist(p2diff);
      invertPector(d1 < d2 ? p1diff : p2diff)
    }
  };

  /* TODO test line */
};

let minFst = (default, items) => switch items {
| [] => default
| [first, ...rest] => List.fold_left(((a1, a2), (b1, b2)) => (a1 < b1 ? (a1, a2) : (b1, b2)), first, rest)
};

let minMag = items => switch items {
| [] => v0
| [first, ...rest] => List.fold_left((a, b) => (a.magnitude < b.magnitude ? a : b), first, rest)
};

let maxMag = items => switch items {
| [] => v0
| [first, ...rest] => List.fold_left((a, b) => (a.magnitude > b.magnitude ? a : b), first, rest)
};

let pythag = (hypotenous, side) => sqrt(hypotenous *. hypotenous -. side *. side);

module Aabb = {
  type t = {x0: float, y0: float, x1: float, y1: float};
  let testPoint = ({x0, y0, x1, y1}, {x, y}) => {
    x0 <= x && x <= x1 &&
    y0 <= y && y <= y1
  };

  let init = (x0, y0, w, h) => {x0, y0, x1: x0 +. w, y1: y0 +. h};
  let translate = ({x0, y0, x1, y1}, {x, y}) => {x0: x0 +. x, x1: x1 +. x, y0: y0 +. y, y1: y1 +. y};
  let ptranslate = ({x0, y0, x1, y1}, {dx, dy}) => {x0: x0 +. dx, x1: x1 +. dx, y0: y0 +. dy, y1: y1 +. dy};
  let fromPoint = ({x, y}) => {x0: x, y0: y, x1: x, y1: y};
  let push = (r, v) => ptranslate(r, vectorToPector(v));
  let center = ({x0, y0, x1, y1}) => {x: x0 +. (x1 -. x0) /. 2., y: y0 +. (y1 -. y0) /. 2.};

  let fromPoints = points => {
    Array.fold_left(
      ({x0, y0, x1, y1}, {x, y}) => (
        {x0: min(x0, x), y0: min(y0, y), x1: max(x1, x), y1: max(y1, y)}
      ),
      fromPoint(points[0]),
      points
    )
  };
  let testAabb = (b1, b2) => {
    b1.x1 >= b2.x0 &&
    b1.x0 <= b2.x1 &&
    b1.y1 >= b2.y0 &&
    b1.y0 <= b2.y1
  };
  let vectorToAabb = (b1, b2) => {
    let sides = [
      (b1.x1 -. b2.x0, pi),
      (b2.x1 -. b1.x0, 0.),
      (b1.y1 -. b2.y0, -.halfPi),
      (b2.y1 -. b1.y0, halfPi),
    ];
    let (magnitude, theta) = minFst((0., 0.), sides);
    {theta, magnitude}
  };
  /* let vectorToPoint = (b1, {x, y}) => {
    let sides = [
      (b1.x1 -. b2.x0, pi),
      (b2.x1 -. b1.x0, 0.),
      (b1.y1 -. b2.y0, -.halfPi),
      (b2.y1 -. b1.y0, halfPi),
    ];
    let (magnitude, theta) = minFst(sides);
    {theta, magnitude}
  }; */

  let vectorToCircle = (r, {Circle.center, rad} as c) => {
    if (testPoint(r, center)) {
      /* vectorToPoint(r, center); */
      let sides = [
        (r.x1 -. (center.x -. rad), pi),
        ((center.x +. rad) -. r.x0, 0.),
        (r.y1 -. (center.y -. rad), -.halfPi),
        ((center.y +. rad) -. r.y0, halfPi),
      ];
    /* ] |> List.filter(((magnitude, theta)) => addVectors({magnitude, theta}, vec).magnitude -. vec.magnitude < 0.001 ); */
      let (magnitude, theta) = minFst((0., 0.), sides);
      {theta, magnitude: -.magnitude}
    } else {
      let {x, y} = center;
      if (r.x0 <= x && x <= r.x1) {
        y < r.y0
        ? {magnitude: rad -. abs_float(r.y0 -. y), theta: -. halfPi}
        : {magnitude: rad -. abs_float(y -. r.y1), theta: halfPi}
        /* v0 */
      } else if (r.y0 <= y && y <= r.y1) {
        /* v0 */
        x < r.x0
        ? {magnitude: rad -. abs_float(r.x0 -. x), theta: pi}
        : {magnitude: rad -. abs_float(x -. r.x1), theta: 0.}
      } else {
        let tx = x < r.x0 ? r.x0 : r.x1;
        let ty = y < r.y0 ? r.y0 : r.y1;
        /* Circle.testPoint(c, {x: tx, y: ty}) */
        let {magnitude, theta} = pectorToVector(pdiff(center, {x: tx, y: ty}));
        {magnitude: magnitude -. rad, theta}
        /* v0 */
        /* assert(false) */
      }
    }
  };

  /* TODO get this working right */
  let collideToCircle = (vec, r, {Circle.center: {x, y}, rad} as c) => {
    /* let {magnitude, theta} = pectorToVector(pdiff(center, {x: tx, y: ty})); */
    let hoff = y < r.y0 ? rad -. pythag(rad, r.y0 -. y) : (y > r.y1 ? rad -. pythag(rad, y -. r.y1) : 0.);
    let voff = x < r.x0 ? rad -. pythag(rad, r.x0 -. x) : (x > r.x1 ? rad -. pythag(rad, x -. r.x1) : 0.);
    let sides =
    /* [
      (r.x0, r.y0),
      (r.x0, r.y1),
      (r.x1, r.y0),
      (r.x1, r.y1),
    ]
    |> List.map(((a, b)) => addMagnitude(pectorToVector(pdiff({x: a, y: b}, {x, y})), rad))
    |> items => items @ */
    ([
      (x, r.y0 -. rad +. voff),
      (x, r.y1 +. rad -. voff),
      (r.x0 -. rad +. hoff, y),
      (r.x1 +. rad -. hoff, y),
    ]
    |> List.map(((a, b)) => pectorToVector(pdiff({x, y},{x: a, y: b})))
    );
    let valid = sides
    |> List.filter((vec2) => addVectors(vec2, vec).magnitude -. vec.magnitude < 0.001 );


    minMag(valid == [] ? sides : valid);
  };

  /* TODO get this working right */
  /* let circleVectors = (vec, r, {Circle.center: {x, y}, rad} as c) => {
    /* let {magnitude, theta} = pectorToVector(pdiff(center, {x: tx, y: ty})); */
    let sides = [
      (r.x0, r.y0),
      (r.x0, r.y1),
      (r.x1, r.y0),
      (r.x1, r.y1),
      (x, r.y0),
      (x, r.y1),
      (r.x0, y),
      (r.x1, y)
    ]
    |> List.map(((a, b)) => addMagnitude(pectorToVector(pdiff({x, y},{x: a, y: b})), rad))
    /* |> List.filter((vec2) => addVectors(vec2, vec).magnitude -. vec.magnitude < 0.001 ); */

    /* minMag(sides); */
  }; */

  let testCircle = (r, {Circle.center, rad} as c) => {
    testPoint(r, center)
    || {
      let {x, y} = center;
      if (r.x0 <= x && x <= r.x1) {
        y < r.y0
        ? (r.y0 -. y) < rad
        : (y -. r.y1) < rad
      } else if (r.y0 <= y && y <= r.y1) {
        x < r.x0
        ? (r.x0 -. x) < rad
        : (x -. r.x1) < rad
      } else {
        let tx = x < r.x0 ? r.x0 : r.x1;
        let ty = y < r.y0 ? r.y0 : r.y1;
        Circle.testPoint(c, {x: tx, y: ty})
      }
    }
  };
};

module Rect = {
  type t = {pos: point, width: float, height: float, hw: float, hh: float};
  let create = (pos, width, height) => {pos, width, height, hw: width /. 2., hh: height /. 2.};
  let translate = (r, pos) => {...r, pos: addPoints(r.pos, pos)};
  let ptranslate = (r, pector) => {...r, pos: addPectorToPoint(pector, r.pos)};
  let push = (r, vector) => {...r, pos: addVectorToPoint(vector, r.pos)};
  let aabb = ({pos: {x, y}, hw, hh}) => Aabb.{x0: x -. hw, x1: x +. hw, y0: y -. hh, y1: y +. hh};
  let addMargin = (r, x, y) => {...r, width: r.width +. x *. 2., hw: r.hw +. x, height: r.height +. y *. 2., hh: r.hh +. y};
  let fromAabb = ({Aabb.x0, y0, x1, y1}) => create({x: x0 +. (x1 -. x0) /. 2., y: y0 +. (y1 -. y0) /. 2.}, x1 -. x0, y1 -. y0);
  let testPoint = ({pos: {x, y}, hw, hh}, p) => {
    x -. hw <= p.x && p.x <= x +. hw &&
    y -. hh <= p.y && p.y <= y +. hh
  };
  let testRect = (r1, r2) => {
    r1.pos.x +. r1.hw > r2.pos.x -. r2.hw &&
    r1.pos.x -. r1.hw < r2.pos.x +. r2.hw &&
    r1.pos.y +. r1.hh > r2.pos.y -. r2.hh &&
    r1.pos.y -. r1.hh < r2.pos.y +. r2.hh
  };
  let testAabb = (r1, b) => Aabb.({
    r1.pos.x +. r1.hw > b.x0 &&
    r1.pos.x -. r1.hw < b.x1 &&
    r1.pos.y +. r1.hh > b.y0 &&
    r1.pos.y -. r1.hh < b.y1
  });

  /** Points clockwise */
  let points = ({pos: {x, y}, hh, hw}) => {
    let tl = {x: x -. hw, y: y -. hh};
    let tr = {x: x +. hw, y: y -. hh};
    let bl = {x: x -. hw, y: y +. hh};
    let br = {x: x +. hw, y: y +. hh};
    [tl, tr, br, bl]
  };

  /** Sides clockwise */
  let sides = ({pos: {x, y}, hh, hw}) => {
    let tl = {x: x -. hw, y: y -. hh};
    let tr = {x: x +. hw, y: y -. hh};
    let bl = {x: x -. hw, y: y +. hh};
    let br = {x: x +. hw, y: y +. hh};
    [
      (tl, tr),
      (tr, br),
      (br, bl),
      (bl, tl),
    ]
    /* [(tl, tr), (tl, bl), (tr, br), (bl, br)] */
  };
  let vectorToRect = (r1, r2) => {
    let sides = [
      (r1.pos.x +. r1.hw -. (r2.pos.x -. r2.hw), pi),
      (r2.pos.x +. r2.hw -. (r1.pos.x -. r1.hw), 0.),
      (r1.pos.y +. r1.hh -. (r2.pos.y -. r2.hh), -.halfPi),
      (r2.pos.y +. r2.hh -. (r1.pos.y -. r1.hh), halfPi),
    ];
    let (magnitude, theta) = minFst((0., 0.), sides);
    {theta, magnitude}
  };
  let vectorToAabb = (r1, b) => {
    open Aabb;
    let sides = [
      (r1.pos.x +. r1.hw -. b.x0, pi),
      (b.x1 -. (r1.pos.x -. r1.hw), 0.),
      (r1.pos.y +. r1.hh -. b.y0, -.halfPi),
      (b.y1 -. (r1.pos.y -. r1.hh), halfPi),
    ];
    let (magnitude, theta) = minFst((0., 0.), sides);
    {theta, magnitude}
  };
  /** TODO this needs to do a line collision test with each side, and then return the one that gets it back to the collided side... I think */
  let collideToAabb = (vec, r1, b) => {
    open Aabb;
    let sides = [
      (r1.pos.x +. r1.hw -. b.x0, pi),
      (b.x1 -. (r1.pos.x -. r1.hw), 0.),
      (r1.pos.y +. r1.hh -. b.y0, -.halfPi),
      (b.y1 -. (r1.pos.y -. r1.hh), halfPi),
    /* ]; */
    ] |> List.filter(((magnitude, theta)) => addVectors({magnitude, theta}, vec).magnitude -. vec.magnitude < 0.001 );
    /* ] |> List.filter(((m, t)) => !isThetaBetween(theta -. halfPi, theta +. halfPi, t)); */
    /* ] |> List.filter(((m, t)) => !isThetaBetween(theta -. halfPi, theta +. halfPi, t)); */
    /* ] |> List.filter(((m, t)) => thetaDiff(theta, t) > halfPi); */
    /* let (magnitude, theta) = minFst((0., (0., 0.)), sides |> List.map(((magnitude, theta)) => (addVectors({magnitude, theta}, vec).magnitude, (magnitude, theta)))) |> snd; */
    let (magnitude, theta) = minFst((0., 0.), sides);
    {theta, magnitude}
  };
  let testCircle = (r, {Circle.center, rad} as c) => {
    testPoint(r, center)
    || {
      let {x, y} = center;
      if (r.pos.x -. r.hw <= x && x <= r.pos.x +. r.hw) {
        y < r.pos.y -. r.hh
        ? (r.pos.y -. r.hh -. y) < rad
        : (y -. (r.pos.y +. r.hh)) < rad
      } else if (r.pos.y -. r.hh <= y && y <= r.pos.y +. r.hh) {
        x < r.pos.x -. r.hw
        ? (r.pos.x -. r.hw -. x) < rad
        : (x -. (r.pos.x +. r.hw)) < rad
      } else {
        let tx = x < r.pos.x -. r.hw ? r.pos.x -. r.hw : r.pos.x +. r.hw;
        let ty = y < r.pos.y -. r.hh ? r.pos.y -. r.hh : r.pos.y +. r.hh;
        Circle.testPoint(c, {x: tx, y: ty})
      }
    }
  };
};

module Polygon = {
  /** Vertices should be *clockwise* */
  type t = {
    aabb: Aabb.t,
    vertices: array(point),
  };

  let push = ({aabb, vertices}, vec) => {aabb: Aabb.push(aabb, vec), vertices: Array.map(addVectorToPoint(vec), vertices)};
  let translate = ({aabb, vertices}, pos) => {aabb: Aabb.translate(aabb, pos), vertices: Array.map(addPoints(pos), vertices)};
  let center = ({aabb}) => Aabb.center(aabb);

  /** Should return lines in a clockwise orientation...
   * Not sure if it does
   * Might need to reverse them to get truly clockwise
   */
  let lines = p => {
    Array.fold_left(
      ((prev, lines), point) => (
        point, [(prev, point), ...lines]
      ), (p.vertices[Array.length(p.vertices) - 1], []), p.vertices)
      |> snd
      /* |> List.rev */
  };

  let fromVertices = vertices => {vertices, aabb: Aabb.fromPoints(vertices)};

  let verticesToPoint = (vertices, point) => {
    let px = point.x;
    let py = point.y;
    Array.fold_left(
      ((inside, prev), current) => {
        let vc = prev;
        let vn = current;
        if (
          ((vc.y > py && vn.y < py) || (vc.y < py && vn.y > py)) &&
          (px < (vn.x -. vc.x)*.(py -. vc.y) /. (vn.y -. vc.y) +. vc.x)
        ) {
          (!inside, current)
        } else {
          (inside, current)
        }
      },
      (false, vertices[Array.length(vertices) - 1]),
      vertices
    ) |> fst
  };

  let testPoint = ({aabb, vertices}, point) => {
    Aabb.testPoint(aabb, point) &&
    verticesToPoint(vertices, point)
  };

  let testPolygon = (p1, p2) => {
    Aabb.testAabb(p1.aabb, p2.aabb) &&
    {
      /* TODO maybe pick the one with fewer points? */
      let l = Array.length(p1.vertices);
      let rec loop = i => {
        if (i >= l) {
          false
        } else if (testPoint(p2, p1.vertices[i])) {
          true
        } else {
          loop(i+1)
        }
      };
      loop(0)
    }
  };

  let testCircle = ({aabb, vertices}, {Circle.rad, center} as c) => {
    Aabb.testPoint(aabb, center) &&
    (
      /** TODO benchmark which of these should go first */
      verticesToPoint(vertices, center)
      ||
      {
        let len = Array.length(vertices);
        let rec loop = (i) => {
          let pi = i == 0 ? len - 1 : i - 1;

          Circle.testLine(c, vertices[pi], vertices[i]) ||
          (i == len - 1 ? false : loop(i + 1))
        };
        loop(0);
      }
    )
  };
};

module Shape = {
  type t =
    | Circle(Circle.t)
    | Aabb(Aabb.t)
    | Rect(Rect.t)
    ;

  let circle = c => Circle(c);
  let aabb = a => Aabb(a);
  let rect = r => Rect(r);

  let center = shape => switch shape {
  | Circle(c) => c.Circle.center
  | Rect(r) => r.Rect.pos
  | Aabb(a) => Rect.fromAabb(a).pos
  };

  let testShapes = (me, other) => {
    switch (me, other) {
    | (Circle(me), Circle(other)) => Circle.testCircle(me, other)
    | (Aabb(me), Aabb(other)) => Aabb.testAabb(me, other)
    | (Rect(me), Rect(other)) => Rect.testRect(me, other)

    | (Circle(me), Aabb(other)) => Aabb.testCircle(other, me)
    | (Aabb(me), Circle(other)) => Aabb.testCircle(me, other)

    | (Rect(me), Circle(other)) => Rect.testCircle(me, other)
    | (Circle(me), Rect(other)) => Rect.testCircle(other, me)

    | (Aabb(me), Rect(other)) => Rect.testAabb(other, me)
    | (Rect(me), Aabb(other)) => Rect.testAabb(me, other)
    }
  };

  let collideToShape = (vel, me, other) => switch (me, other) {
    | (Aabb(me), Aabb(other)) => Rect.collideToAabb(vel, Rect.fromAabb(me), other)
    | (Circle(me), Circle(other)) => Circle.vectorToCircle(me, other)
    | (Rect(me), Rect(other)) => Rect.collideToAabb(vel, me, Rect.aabb(other))

    | (Circle(me), Aabb(other)) => Aabb.collideToCircle(vel, other, me)

    | (Circle(me), Rect(other)) => Aabb.collideToCircle(vel, Rect.aabb(other), me)

    | (Aabb(me), Rect(other)) => Rect.collideToAabb(vel, Rect.fromAabb(me), Rect.aabb(other))
    | (Rect(me), Aabb(other)) => Rect.collideToAabb(vel, me, other)

    /* Needs work */
    | (Rect(me), Circle(other)) => Aabb.collideToCircle(vel, Rect.aabb(me), other) |> invertVector
    | (Aabb(me), Circle(other)) => Aabb.collideToCircle(vel, me, other) |> invertVector
  };

  let translate = (shape, pt) => switch shape {
  | Circle(c) => Circle(Circle.translate(c, pt))
  | Rect(c) => Rect(Rect.translate(c, pt))
  | Aabb(c) => Aabb(Aabb.translate(c, pt))
  };

  let push = (shape, vel) => switch shape {
  | Circle(c) => Circle(Circle.push(c, vel))
  | Rect(c) => Rect(Rect.push(c, vel))
  | Aabb(c) => Aabb(Aabb.push(c, vel))
  };
};

module Object = {
  type t = {shape: Shape.t, vel: vector};
};
