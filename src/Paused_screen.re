
open Reprocessing;
open Geom;

let hcenter = env => Env.width(env) / 2;
let vcenter = env => Env.height(env) / 2;

/* let playButton = env => Geom.Rect.create({x: hcenter(env), y: vcenter(env)}, 100., 30.);
let restartButton = env => Geom.Rect.create({x: hcenter(env), y: vcenter(env) +. 50.}, 100., 30.); */

let buttonStyle = ctx => UIManager.{
  textStyle: {
    font: ctx.Shared.smallFont,
    tint: Some(Constants.black)
  },
  bgColor: Utils.color(~r=255, ~g=255, ~b=255, ~a=100),
  borderColor: Constants.black,
  hoverBorderColor: Utils.color(~r=200, ~g=200, ~b=200, ~a=200),
  innerBorder: None,
  fixedWidth: None,
  enabled: true,
  margin: 15
};

let ui = (state, ctx, env) => UIManager.{
  pos: (hcenter(env), vcenter(env)),
  align: Center,
  valign: Middle,
  el: VBox([
    Text("Drag & release", {font: ctx.Shared.smallFont, tint: None}, Center),
    Text("to throw", {font: ctx.Shared.smallFont, tint: None}, Center),
    Spacer(5),
    Button("Resume", `Resume, buttonStyle(ctx)),
    Button("Restart", `Restart, buttonStyle(ctx)),
    Spacer(5),
    Text("High score:", {
      font: ctx.Shared.smallFont,
      tint: None
    }, Center),
    Text(state.Play_types.lastSavedHighScore == 1 ? "1 rock" : string_of_int(state.Play_types.lastSavedHighScore) ++ " rocks",
    {
      font: ctx.Shared.smallFont,
      tint: None

    }, Center)
  ], 10, Center)
};

let draw = (state, ctx, env) => {
  Play_draw.draw(state, ctx, env);
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=200), env);
  /* Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=200), env); */
  Draw.rect(~pos=(0, 0), ~width=Env.width(env), ~height=Env.height(env), env);
  UIManager.draw(env, ui(state, ctx, env));
};

let step = (state, ctx, env) => {
  if (Env.mousePressed(env)) {
    switch (UIManager.act(env, ui(state, ctx, env))) {
    | None => state
    | Some(`Resume) => {...state, Play_types.paused: false}
    | Some(`Restart) => Play_step.start(env)
    }
  } else {
    state
  }
};