
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

type point = {x: float, y: float};
type vector = {magnitude: float, theta: float};
let v0 = {magnitude: 0., theta: 0.};
type pector = {dx: float, dy: float};

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
let vectorToPector = ({magnitude, theta}) => {dx: cos(theta) *. magnitude, dy: sin(theta) *. magnitude};
let angleTo = (p1, p2) => atan2(p2.y -. p1.y, p2.x -. p1.x);
let pectorToVector = (p) => {
  magnitude: pdist(p),
  theta: atan2(p.dy, p.dx)
};
let addPectors = (p1, p2) => {dx: p1.dx +. p2.dx, dy: p1.dy +. p2.dy};
let clampVector = ({magnitude, theta}, maxMag) => {magnitude: min(maxMag, magnitude), theta};
let addVectors = (v1, v2) => addPectors(vectorToPector(v1), vectorToPector(v2)) |> pectorToVector;
let addPectorToVector = (p, v) => addPectors(p, vectorToPector(v)) |> pectorToVector;
let invertVector = ({magnitude, theta}) => {magnitude, theta: theta +. 3.14159};
let invertPector = ({dx, dy}) => {dx: -.dx, dy: -.dy};
let scaleVector = ({magnitude, theta}, scale) => {theta, magnitude: magnitude *. scale};
let addPoints = (p1, p2) => {x: p1.x +. p2.x, y: p1.y +. p2.y};

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

  let testPoint = ({rad, center}, point) => dist(point, center) <= rad;
  let testCircle = (c1, c2) => dist(c1.center, c2.center) <= c1.rad +. c2.rad;
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
let halfPi = pi /. 2;
let tau = pi *. 2.;

let rec normalize = x => {
  if (x < -. pi) normalize(x +. tau)
  else if (x > pi) normalize(x -. tau)
  else x
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

let minFst = items => switch items {
| [] => assert(false)
| [first, ...rest] => List.fold_left(((a1, a2), (b1, b2)) => (a1 < b1 ? (a1, a2) : (b1, b2)), first, rest)
};

module Aabb = {
  type t = {x0: float, y0: float, x1: float, y1: float};
  let testPoint = ({x0, y0, x1, y1}, {x, y}) => {
    x0 <= x && x <= x1 &&
    y0 <= y && y <= y1
  };

  let translate = ({x0, y0, x1, y1}, {x, y}) => {x0: x0 +. x, x1: x1 +. x, y0: y0 +. y, y1: y1 +. y};
  let fromPoint = ({x, y}) => {x0: x, y0: y, x1: x, y1: y};

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
    let (magnitude, theta) = minFst(sides);
    {theta, magnitude}
  };
};

module Rect = {
  type t = {pos: point, width: float, height: float, hw: float, hh: float};
  let translate = (r, pos) => {...r, pos: addPoints(r.pos, pos)};
  let aabb = ({pos: {x, y}, hw, hh}) => Aabb.{x0: x -. hw, x1: x +. hw, y0: y -. hh, y1: y +. hh};
  let testPoint = ({pos: {x, y}, hw, hh}, p) => {
    x -. hw <= p.x && p.x <= x +. hw &&
    y -. hh <= p.y && p.y <= y +. hh
  };
  let testRect = (r1, r2) => {
    r1.pos.x +. r1.hw >= r2.pos.x -. r2.hw &&
    r1.pos.x -. r1.hw <= r2.pos.x +. r2.hw &&
    r1.pos.y +. r1.hh >= r2.pos.y -. r2.hh &&
    r1.pos.y -. r1.hh <= r2.pos.y +. r2.hh
  };
  let testAabb = (r1, b) => Aabb.({
    r1.pos.x +. r1.hw >= b.x0 &&
    r1.pos.x -. r1.hw <= b.x1 &&
    r1.pos.y +. r1.hh >= b.y0 &&
    r1.pos.y -. r1.hh <= b.y1
  });
  let vectorToRect = (r1, r2) => {
    let sides = [
      (r1.pos.x +. r1.hw -. (r2.pos.x -. r2.hw), pi),
      (r2.pos.x +. r2.hw -. (r1.pos.x -. r1.hw), 0.),
      (r1.pos.y +. r1.hh -. (r2.pos.y -. r2.hh), -.halfPi),
      (r2.pos.y +. r2.hh -. (r1.pos.y -. r1.hh), halfPi),
    ];
    let (magnitude, theta) = minFst(sides);
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
    let (magnitude, theta) = minFst(sides);
    {theta, magnitude}
  };
};

module Polygon = {
  type t = {
    aabb: Aabb.t,
    vertices: array(point),
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


/* boolean polyPoint(PVector[] vertices, float px, float py) {
  boolean collision = false;

  // go through each of the vertices, plus
  // the next vertex in the list
  int next = 0;
  for (int current=0; current<vertices.length; current++) {

    // get next vertex in list
    // if we've hit the end, wrap around to 0
    next = current+1;
    if (next == vertices.length) next = 0;

    // get the PVectors at our current position
    // this makes our if statement a little cleaner
    PVector vc = vertices[current];    // c for "current"
    PVector vn = vertices[next];       // n for "next"

    // compare position, flip 'collision' variable
    // back and forth
    if (((vc.y > py && vn.y < py) || (vc.y < py && vn.y > py)) &&
         (px < (vn.x-vc.x)*(py-vc.y) / (vn.y-vc.y)+vc.x)) {
            collision = !collision;
    }
  }
  return collision;
} */
