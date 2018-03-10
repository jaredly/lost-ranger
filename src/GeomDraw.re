
open Geom;
open Reprocessing;

let line = (p1, p2, env) => {
  Draw.linef(~p1=tuple(p1), ~p2=tuple(p2), env)
};

let vec = (start, vec, env) => {
  Draw.linef(~p1=tuple(start), ~p2=tuple(addVectorToPoint(vec, start)), env);
};

let angle = (pos, v, theta, env) => {
  for (i in 0 to 10) {
    let part = float_of_int(i) /. 10. *. theta;
    vec(pos, {...v, theta: v.theta +. part}, env)
  };
  /* Draw.arcf(
    ~center=Geom.tuple(pos),
    ~radx=vec.magnitude,
    ~rady=vec.magnitude,
    ~start=(theta < 0. ? vec.theta +. theta : vec.theta),
    ~stop=(theta < 0. ? vec.theta : vec.theta +. theta),
    ~isOpen=true,
    ~isPie=false,
    env
  ) */
};

let circle = ({Circle.rad, center}, env) => {
  Draw.ellipsef(~center=tuple(center), ~radx=rad, ~rady=rad, env);
};

let point = (p1, env) => circle({Circle.rad: 3., center: p1}, env);

let aabb = ({Aabb.x0, y0, x1, y1}, env) => {
  Draw.rectf(~pos=(x0, y0), ~width=x1 -. x0, ~height=y1 -. y0, env);
};

let rect = ({Rect.pos: {x, y}, width, height, hw, hh}, env) =>
  Draw.rectf(~pos=(x -. hw, y -. hh), ~width, ~height, env);

let polygon = ({Polygon.vertices}, env) => {
  Array.iteri((i, p) => {
    let i = i == 0 ? Array.length(vertices) - 1: i - 1;
    let prev = vertices[i];
    Draw.linef(~p1=tuple(prev), ~p2=tuple(p), env);
  }, vertices)
};

let shape = shape => switch shape {
| Shape.Circle(c) => circle(c)
| Rect(r) => rect(r)
| Aabb(a) => aabb(a)
};