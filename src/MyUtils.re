type pos = (float, float);

let rectCollide = ((x, y), ((a, b), (w, h))) => x >= a && x <= a + w && y >= b && y <= b + h;

let flDiv = (a, b) => float_of_int(a) /. float_of_int(b);

type vec = {
  mag: float,
  theta: float
};

/* let range = (num) => {
  Array.make(num, 0)
  |> Array.to_list
  |> List.mapi((i, _) => i)
}; */

/** current, max */
type counter = (float, float);

type counteri = (int, int);

let counter = (num) => (0., num);

let v0 = {mag: 0., theta: 0.};

let dx = ({theta, mag}) => cos(theta) *. mag;

let dy = ({theta, mag}) => sin(theta) *. mag;

let vecToPos = (vec) => (dx(vec), dy(vec));

let vecFromPos = ((dx, dy)) => {mag: sqrt(dx *. dx +. dy *. dy), theta: atan2(dy, dx)};

let dist = ((dx, dy)) => sqrt(dx *. dx +. dy *. dy);

let thetaToward = ((x0, y0), (x1, y1)) => atan2(y1 -. y0, x1 -. x0);

let posAdd = ((x0, y0), (x1, y1)) => (x0 +. x1, y0 +. y1);

let posSub = ((x0, y0), (x1, y1)) => (x0 -. x1, y0 -. y1);

let vecAdd = (v1, v2) => vecFromPos(posAdd(vecToPos(v1), vecToPos(v2)));

let vecToward = (p1, p2) => vecFromPos(posSub(p2, p1));

let scaleVec = ({mag, theta}, scale) => {mag: mag *. scale, theta};

let scalePos = ((x, y), scale) => (x *. scale, y *. scale);

let range = (num) => {
  let rec loop = (num, collect) => num <= 0 ? collect : loop(num - 1, [num - 1, ...collect]);
  loop(num, [])
};

let withRange = (num, fn) => {
  let rec loop = i => num <= 0 ? [] : [fn(num - 1), ...loop(num - 1)];
  loop(num);
};

let withAlpha = ({Reprocessing_Common.r, g, b, a}, alpha) => {
  Reprocessing_Common.r,
  g,
  b,
  a: a *. alpha
};

let collides = (p1, p2, d) => dist(posSub(p1, p2)) <= d;

let increment = (env) => Reprocessing_Env.deltaTime(env) *. 1000. /. 16.;

let stepTimer = ((current, max), env) => {
  let time = increment(env);
  if (current +. time >= max) {
    ((max, max), true)
  } else {
    ((current +. time, max), false)
  }
};

let isFullTimer = ((current, max)) => current === max;

let loopTimer = ((current, max), env) => {
  let time = increment(env);
  if (current +. time >= max) {
    ((0., max), true)
  } else {
    ((current +. time, max), false)
  }
};

let countDown = ((current, max)) =>
  if (current <= 1) {
    ((0, max), true)
  } else {
    ((current - 1, max), false)
  };