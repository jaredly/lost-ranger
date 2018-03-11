
type t = Play_types.status;

let init = (context, env) => {
  Play_step.start(env)
};

let step = (status, context, env) => {
  if (Reprocessing.Env.keyPressed(Reprocessing.Events.Escape, env)) {
    init(context, env)
  } else {
    if (status.Play_types.paused) {
      Paused_screen.step(status, context, env)
    } else {
      Play_step.step(status, context, env)
    }
    /* | `Continue(status) => (size, num, status)
    | `Won(prevState) => {
      let size = num >= 4 ? size + 1 : size;
      (size, num >= 4 ? 0 : num + 1, Play_step.continue(prevState, size, env))
    }
    } */
  }
};

let touchStart = Play_step.touchStart;
let touchMove = Play_step.touchMove;
let touchEnd = Play_step.touchEnd;

let draw = (state, ctx, env) => {
  if (state.Play_types.paused) {
    Paused_screen.draw(state, ctx, env)
  } else {
    Play_draw.draw(state, ctx, env)
  }
};