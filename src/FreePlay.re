
type t = Play_types.status;

let init = (context, env) => {
  Play_step.start(env)
};

let step = (status, context, env) => {
  if (Reprocessing.Env.keyPressed(Reprocessing.Events.Escape, env)) {
    init(context, env)
  } else {
    Play_step.step(status, context, env)
    /* | `Continue(status) => (size, num, status)
    | `Won(prevState) => {
      let size = num >= 4 ? size + 1 : size;
      (size, num >= 4 ? 0 : num + 1, Play_step.continue(prevState, size, env))
    }
    } */
  }
};

let draw = (status, context, env) => {
  Reprocessing.Draw.pushMatrix(env);
  Play_draw.draw(status, context, env);
  Reprocessing.Draw.popMatrix(env);

  /* GeomDebug.draw(status, context, env); */

  Reprocessing.Draw.tint(Play_draw.textColor, env);
  Reprocessing.Draw.text(~font=context.textFont, ~body=Printf.sprintf("Throw rocks"), ~pos=(10, 10), env);
  Reprocessing.Draw.text(~font=context.smallFont, ~body=Printf.sprintf("%d rocks thrown", List.length(status.Play_types.stones)), ~pos=(10, 40), env);

  Reprocessing.Draw.text(~font=context.smallFont, ~body="Lost Ranger - by Jared Forsyth", ~pos=(10, int_of_float(context.height) - 60), env);
  Reprocessing.Draw.text(~font=context.smallFont, ~body="Made with ReasonML and Reprocessing", ~pos=(10, int_of_float(context.height) - 30), env);
  Reprocessing.Draw.noTint(env);
};
