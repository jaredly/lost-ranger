
Printexc.record_backtrace(true);

let getEnv = name => try { Some(Sys.getenv(name)) } { | Not_found => None };

let (/+) = Filename.concat;
let setup = (assetDir, env) => {
  if (!Shared.isPhone) {
    Reprocessing.Env.resizeable(false, env);
  };

  if (!Shared.isPhone) {
    if (Reprocessing.Env.maxWidth(env) < 800 && Reprocessing.Env.maxHeight(env) < 800) {
      Reprocessing.Env.size(~width=Reprocessing.Env.maxWidth(env), ~height=Reprocessing.Env.maxHeight(env), env);
    } else {
      Reprocessing.Env.size(~width=800, ~height=800, env);
    };
  } else {
    Reprocessing.Env.resizeable(true, env);
  };
    Reprocessing.Env.resizeable(true, env);

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
    charSheet: Reprocessing.Draw.loadImage(~filename=assetDir/+"Spritesheets"/+Play_assets.Players.source, env),
    itemSheet: Reprocessing.Draw.loadImage(~filename=assetDir/+"Spritesheets"/+Play_assets.Items.source, env),
    tileSheet: Reprocessing.Draw.loadImage(~filename=assetDir/+"Spritesheets"/+Play_assets.Tiles.source, env),
    extraItemsSheet: Reprocessing.Draw.loadImage(~filename=assetDir/+"Spritesheets"/+Play_assets.ExtraItems.source, env),
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
  if (Reprocessing.Env.keyPressed(Reprocessing.Events.Num_1, env)) {
    Profile.beginProfiling();
  } else if (Reprocessing.Env.keyPressed(Reprocessing.Events.Num_2, env)) {
    Profile.endProfiling();
    Profile.export("./profile_" ++ string_of_float(Profile.getTime()) ++ ".json");
  };
  open Shared;
  let screenState = Profile.wrap("step", () => FreePlay.step(screenState, context, env));
  Profile.wrap("draw", () => FreePlay.draw(screenState, context, env));
  {screenState, context}
};

let run = (fn, {Shared.screenState, context}, env) => {
  let screenState = fn(screenState, context, env);
  {Shared.screenState, context}
};

let run = (assetDir, _) => Reprocessing.run(
  ~setup=setup(assetDir),
  ~title="Throw Rocks",
  ~touchStart=run(FreePlay.touchStart),
  ~touchMove=run(FreePlay.touchMove),
  ~touchEnd=run(FreePlay.touchEnd),
  ~backPressed=({Shared.screenState, context}, env) => {
    switch (FreePlay.backPressed(screenState, context, env)) {
    | None => None
    | Some(newState) => Some({Shared.screenState: newState, context})
    }
  },
  ~draw,
  ()
);

let noop = () => {
  print_endline("noop");
};