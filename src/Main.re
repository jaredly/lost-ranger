
Printexc.record_backtrace(true);

let getEnv = name => try { Some(Sys.getenv(name)) } { | Not_found => None };

let (/+) = Filename.concat;
let setup = (assetDir, env) => {
  if (!Shared.isPhone) {
    Reprocessing.Env.resizeable(false, env);
  };

  if (!Shared.isPhone) {
    if (Reprocessing.Env.maxWidth(env) < 800 && Reprocessing.Env.maxHeight(env) < 800) {
      /* Mobile web perf is not too hot :( turning off round line caps makes it a bit better */
      Reprocessing.Draw.strokeCap(Reprocessing_Common.Project, env);
      Reprocessing.Env.size(~width=Reprocessing.Env.maxWidth(env), ~height=Reprocessing.Env.maxHeight(env), env);
    } else {
      Reprocessing.Env.size(~width=800, ~height=800, env);
    };
  };

  /* Random.init(100); */
  let height = Reprocessing.Env.height(env) |> float_of_int;
  let width = Reprocessing.Env.width(env) |> float_of_int;

  /** This size pegs my cpu drawing the walls */
  /* let size = 17; */

  /* let size = 13; */
  /* let size = 5; */
  /* let size = 3; */

  let context = {
    height,
    width,
    Shared.titleFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Black-48.fnt", ~isPixel=false, env),
    smallTitleFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Regular-24.fnt", ~isPixel=false, env),
    boldTextFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Black-24.fnt", ~isPixel=false, env),
    textFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Regular-24.fnt", ~isPixel=false, env),
    smallFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Regular-16.fnt", ~isPixel=false, env),
  };

  let gameState = FreePlay.init(context, env);

  {
    Shared.screenState: gameState,
    context
  }
};

let draw = ({Shared.screenState, context}, env) => {
  open Shared;
  let screenState = FreePlay.step(screenState, context, env);
  FreePlay.draw(screenState, context, env);
  {screenState, context}
};

let run = (assetDir, _) => Reprocessing.run(
  ~setup=setup(assetDir),
  ~title="PurpleMaze",
  ~draw,
  ()
);

let noop = () => {
  print_endline("noop");
};