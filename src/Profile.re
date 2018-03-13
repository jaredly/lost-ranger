
type node = {
  name: string,
  start: bool,
  ts: float,
  mutable tail: node
};

let rec dummy = {name: "", start: false, ts: 0., tail: dummy};

type root = {head: node, mutable rtail: node};

let head = {
  name: "root",
  start: true,
  ts: 0.,
  tail: dummy
};
let root = {
  head,
  rtail: head
};

let profiling = ref(false);

let start = (name) => {
  if (profiling^) {
    let node = {name, start: true, ts: Unix.gettimeofday(), tail: dummy};
    root.rtail.tail = node;
    root.rtail = node;
  }
};

let stop = (name) => {
  if (profiling^) {
    let node = {name, start: false, ts: Unix.gettimeofday(), tail: dummy};
    root.rtail.tail = node;
    root.rtail = node;
  }
};

let wrap = (name, fn) => {
  start(name);let res = fn();stop(name);res
};

let beginProfiling = () => profiling := true;
let endProfiling = () => profiling := false;
let nodeToJsonString = ({name, ts, start}) => Printf.sprintf({|{"name": %S, "ts": %d, "ph": %S, "tid": 0, "pid": 0}|}, name, int_of_float(ts *. 1000000.), start ? "B" : "E");
let nodeToJson = node => {
  `Assoc([
    ("name", `String(node.name)),
    ("ts", `Int(int_of_float(node.ts *. 1000000.))),
    ("ph", `String(node.start ? "B" : "E")),
    ("pid", `Int(0)),
    ("tid", `Int(0)),
  ])
};

let export = dest => {
  let rec loop = node => {
    if (node === dummy) {
      []
    } else {
      [nodeToJsonString(node), ...loop(node.tail)]
    }
  };
  /** skip the root node */
  let json = "[" ++ String.concat(",", loop(root.head.tail)) ++ "]";
  let chan = open_out(dest);
  output_string(chan, json);
  close_out(chan);
};
