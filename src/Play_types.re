
type line = ((float, float), (float, float));

let module LineSet = Set.Make({
  type t = line;
  let compare = compare;
});

type player = {pos: Geom.point, vel: Geom.vector, size: float};

type state = {

};

type status =
  /* | AnimateIn(option((state, Timer.t, score)), state, Timer.t) */
  | Playing(state)
  ;