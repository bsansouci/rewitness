/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type tileType = {bottom: bool, left: bool, top: bool, right: bool};

type floatPointType = {x: float, y: float};

type intPointType = {x: int, y: int};

type wCoordType = | WCoord of floatPointType;

type gCoordType = | GCoord of intPointType;

type pCoordType = | PCoord of intPointType;

type puzzleType = {startTile: pCoordType, endTile: pCoordType, grid: list (list tileType)};

type mouseStateType = {mutable mousePos: gCoordType, mutable mouseDown: bool};

type tilePointType = {tile: tileType, position: pCoordType};

type gameStateType = {mutable currentPath: list tilePointType, mutable started: bool};

let module Color = {
  let red = (1., 0., 0.);
  let yellow = (0.97, 0.7, 0.);
  let brightYellow = (0.985, 0.88, 0.3);
  let brown = (0.28, 0.21, 0.02);
};

let module B = {
  let b = {bottom: true, left: false, top: false, right: false};
  let l = {bottom: false, left: true, top: false, right: false};
  let t = {bottom: false, left: false, top: true, right: false};
  let r = {bottom: false, left: false, top: false, right: true};
  let bl = {bottom: true, left: true, top: false, right: false};
  let br = {bottom: true, left: false, top: false, right: true};
  let lt = {bottom: false, left: true, top: true, right: false};
  let tr = {bottom: false, left: false, top: true, right: true};
  let bt = {bottom: true, left: false, top: true, right: false};
  let lr = {bottom: false, left: true, top: false, right: true};
  let blt = {bottom: true, left: true, top: true, right: false};
  let ltr = {bottom: false, left: true, top: true, right: true};
  let btr = {bottom: true, left: false, top: true, right: true};
  let blr = {bottom: true, left: true, top: false, right: true};
  let bltr = {bottom: true, left: true, top: true, right: true};
  let n = {bottom: false, left: false, top: false, right: false};
};

let lineWeight = 30;

let lineWeightf = float_of_int lineWeight;

let windowSize = 800;

let windowSizef = float_of_int windowSize;

let toWorldCoordinate (WCoord {x, y}) => (
  x /. (windowSizef /. 2.) -. 1.,
  y /. (windowSizef /. 2.) -. 1.
);

let drawRect width::width height::height color::color position::(GCoord {x, y}) => {
  GlDraw.begins `quads;
  GlDraw.color color;
  let bottomLeft = WCoord {x: float_of_int x, y: float_of_int y};
  let topLeft = WCoord {x: float_of_int x, y: float_of_int @@ y + height};
  let topRight = WCoord {x: float_of_int @@ x + width, y: float_of_int @@ y + height};
  let bottomRight = WCoord {x: float_of_int @@ x + width, y: float_of_int y};
  List.iter
    GlDraw.vertex2 (List.map toWorldCoordinate [bottomLeft, topLeft, topRight, bottomRight]);
  GlDraw.ends ()
};

let drawCircle radius::radius color::color position::(GCoord {x, y}) => {
  let deg2grad = 3.14159 /. 180.;
  let floatRadius = float_of_int radius;
  GlDraw.begins `triangle_fan;
  GlDraw.color color;
  for i in 0 to 360 {
    let degInGrad = float_of_int i *. deg2grad;
    GlDraw.vertex2 @@
      toWorldCoordinate (
        WCoord {
          x: cos degInGrad *. floatRadius +. float_of_int x,
          y: sin degInGrad *. floatRadius +. float_of_int y
        }
      )
  };
  GlDraw.ends ()
};

let centerPoint puzzleSize (GCoord {x, y}) =>
  GCoord {
    x: (x + windowSize / 2) - (puzzleSize * 3 * lineWeight) / 2,
    y: (y + windowSize / 2) - (puzzleSize * 3 * lineWeight) / 2
  };

let toGameCoord (PCoord {x, y}) => GCoord {x: x * 3 * lineWeight, y: y * 3 * lineWeight};

let getCircleCenter (GCoord {x, y}) =>
  GCoord {x: x + (3 * lineWeight) / 2, y: y + (3 * lineWeight) / 2};

let drawPuzzle puzzle => {
  let drawCell (GCoord {x, y}) {bottom, left, top, right} => {
    if bottom {
      drawRect
        width::lineWeight
        height::(2 * lineWeight)
        color::Color.brown
        position::(GCoord {x: x + lineWeight, y})
    };
    if left {
      drawRect
        width::(2 * lineWeight)
        height::lineWeight
        color::Color.brown
        position::(GCoord {x, y: y + lineWeight})
    };
    if top {
      drawRect
        width::lineWeight
        height::(2 * lineWeight)
        color::Color.brown
        position::(GCoord {x: x + lineWeight, y: y + lineWeight})
    };
    if right {
      drawRect
        width::(2 * lineWeight)
        height::lineWeight
        color::Color.brown
        position::(GCoord {x: x + lineWeight, y: y + lineWeight})
    }
  };
  let puzzleSize = List.length puzzle.grid;
  ignore @@
    List.mapi
      (
        fun y row =>
          List.mapi
            (fun x cell => drawCell (centerPoint puzzleSize (toGameCoord (PCoord {x, y}))) cell)
            row
      )
      (List.rev puzzle.grid);
  drawCircle
    radius::lineWeight
    color::Color.brown
    position::(getCircleCenter (centerPoint puzzleSize (toGameCoord puzzle.startTile)))
};

let didClickOnStartTile
    puzzle::puzzle
    mouseState::{mousePos: GCoord {x: mouseX, y: mouseY}, mouseDown} =>
  if mouseDown {
    let puzzleSize = List.length puzzle.grid;
    let GCoord {x, y} = getCircleCenter (centerPoint puzzleSize (toGameCoord puzzle.startTile));
    let dx = x - mouseX;
    let dy = y - mouseY;
    let distance = sqrt @@ float_of_int (dx * dx + dy * dy);
    distance <= lineWeightf
  } else {
    false
  };

let examplePuzzle = {
  startTile: PCoord {x: 0, y: 3},
  endTile: PCoord {x: 0, y: 0},
  grid: [
    [B.br, B.lr, B.bl, B.bl],
    [B.bt, B.br, B.bt, B.bt],
    [B.tr, B.lt, B.ltr, B.blt],
    [B.tr, B.blr, B.lr, B.lt]
  ]
};

let () = {
  ignore @@ Glut.init Sys.argv;
  Glut.initWindowSize windowSize windowSize;
  Glut.initDisplayMode double_buffer::true ();
  ignore @@ Glut.createWindow title::"A Reason To Witness";
  let mouseState = {mousePos: GCoord {x: 0, y: 0}, mouseDown: false};
  let gameState = {currentPath: [], started: false};
  Glut.mouseFunc (
    fun button::button state::state x::x y::y => {
      mouseState.mousePos = GCoord {x, y: windowSize - y};
      switch button {
      | Glut.LEFT_BUTTON =>
        mouseState.mouseDown = state == Glut.DOWN;
        if (state == Glut.DOWN) {
          if (mouseState.mouseDown && gameState.started) {
            gameState.started = false
          } else {
            gameState.started = didClickOnStartTile puzzle::examplePuzzle mouseState::mouseState
          }
        }
      | _ => ()
      }
    }
  );
  Glut.passiveMotionFunc (fun x::x y::y => mouseState.mousePos = GCoord {x, y: windowSize - y});
  let render () => {
    GlClear.clear [`color];
    GlMat.load_identity ();
    drawRect
      width::windowSize height::windowSize color::Color.yellow position::(GCoord {x: 0, y: 0});
    ignore @@ drawPuzzle examplePuzzle;
    if gameState.started {
      let puzzleSize = List.length examplePuzzle.grid;
      drawCircle
        radius::lineWeight
        color::Color.brightYellow
        position::(getCircleCenter (centerPoint puzzleSize (toGameCoord examplePuzzle.startTile)))
    };
    Glut.swapBuffers ()
  };
  GlMat.mode `modelview;
  Glut.displayFunc cb::render;
  Glut.idleFunc cb::(Some Glut.postRedisplay);
  Glut.mainLoop ()
};
