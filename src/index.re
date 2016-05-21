/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type tileType = {bottom: bool, left: bool, top: bool, right: bool};

type pointType = {x: float, y: float};

type bigBallThingy = pointType;

type puzzleType = {startTile: pointType, endTile: pointType, grid: list (list tileType)};

type mouseStateType = {mutable mouseX: float, mutable mouseY: float, mutable mouseDown: bool};

type tilePointType = {tile: tileType, position: pointType};

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

let gridSize = 500.;

let lineWeight = 30;

let lineWeightf = float_of_int lineWeight;

let toWorldCoordinate {x, y} => (x /. (gridSize /. 2.) -. 1., y /. (gridSize /. 2.) -. 1.);

let drawRect width::width height::height color::color position::position => {
  GlDraw.begins `quads;
  GlDraw.color color;
  let bottomLeft = position;
  let topLeft = {x: position.x, y: position.y +. float_of_int height};
  let topRight = {x: position.x +. float_of_int width, y: position.y +. float_of_int height};
  let bottomRight = {x: position.x +. float_of_int width, y: position.y};
  List.iter
    GlDraw.vertex2 (List.map toWorldCoordinate [bottomLeft, topLeft, topRight, bottomRight]);
  GlDraw.ends ()
};

let drawCircle radius::radius color::color position::position => {
  let deg2grad = 3.14159 /. 180.;
  let floatRadius = float_of_int radius;
  GlDraw.begins `triangle_fan;
  GlDraw.color color;
  for i in 0 to 360 {
    let degInGrad = float_of_int i *. deg2grad;
    GlDraw.vertex2 @@
      toWorldCoordinate {
        x: cos degInGrad *. floatRadius +. position.x,
        y: sin degInGrad *. floatRadius +. position.y
      }
  };
  GlDraw.ends ()
};

let drawPuzzle puzzle => {
  let drawCell x y {bottom, left, top, right} => {
    if bottom {
      drawRect
        width::lineWeight
        height::(2 * lineWeight)
        color::Color.brown
        position::{x: x +. lineWeightf, y}
    };
    if left {
      drawRect
        width::(2 * lineWeight)
        height::lineWeight
        color::Color.brown
        position::{x, y: y +. lineWeightf}
    };
    if top {
      drawRect
        width::lineWeight
        height::(2 * lineWeight)
        color::Color.brown
        position::{x: x +. lineWeightf, y: y +. lineWeightf}
    };
    if right {
      drawRect
        width::(2 * lineWeight)
        height::lineWeight
        color::Color.brown
        position::{x: x +. lineWeightf, y: y +. lineWeightf}
    }
  };
  let puzzleSize = float_of_int @@ List.length puzzle.grid;
  let centerPoint p => (p +. gridSize /. 2.) -. (puzzleSize *. 3. *. lineWeightf) /. 2.;
  ignore @@
    List.mapi
      (
        fun y row =>
          List.mapi
            (
              fun x cell =>
                drawCell
                  (centerPoint (float_of_int x *. 3. *. lineWeightf))
                  (centerPoint (float_of_int y *. 3. *. lineWeightf))
                  cell
            )
            row
      )
      (List.rev puzzle.grid);
  let {x: startX, y: startY} = puzzle.startTile;
  drawCircle
    radius::lineWeight
    color::Color.brown
    position::{
      x: centerPoint (startX *. 3. *. lineWeightf +. lineWeightf *. 1.5),
      y: centerPoint (startY *. 3. *. lineWeightf +. lineWeightf *. 1.5)
    }
};

let didClickOnStart puzzle::puzzle mouseState::{mouseX, mouseY, mouseDown} => {
  let puzzleSize = float_of_int @@ List.length puzzle.grid;
  let centerPoint p =>
    ((p +. gridSize /. 2.) -. (puzzleSize *. 3. *. lineWeightf) /. 2.) /. (gridSize /. 2.) -. 1.;
  if mouseDown {
    let {x: startX, y: startY} = puzzle.startTile;
    let dx = centerPoint (startX *. 3. *. lineWeightf +. lineWeightf *. 1.5) -. mouseX;
    let dy = centerPoint (startY *. 3. *. lineWeightf +. lineWeightf *. 1.5) -. mouseY;
    let distance = sqrt (dx *. dx +. dy *. dy);
    distance <= float_of_int lineWeight /. (gridSize /. 2.)
  } else {
    false
  }
};

let puzzle = {
  startTile: {x: 0., y: 3.},
  endTile: {x: 0., y: 0.},
  grid: [
    [B.br, B.lr, B.bl, B.bl],
    [B.bt, B.br, B.bt, B.bt],
    [B.tr, B.lt, B.ltr, B.blt],
    [B.tr, B.blr, B.lr, B.lt]
  ]
};

let windowSize = 800;

let () = {
  ignore @@ Glut.init Sys.argv;
  Glut.initWindowSize windowSize windowSize;
  Glut.initDisplayMode double_buffer::true ();
  ignore @@ Glut.createWindow title::"A Reason To Witness";
  let mouseState = {mouseX: 0., mouseY: 0., mouseDown: false};
  let gameState = {currentPath: [], started: false};
  Glut.mouseFunc (
    fun button::button state::state x::x y::y => {
      mouseState.mouseX = (float_of_int x /. float_of_int windowSize) *. 2. -. 1.;
      mouseState.mouseY = (float_of_int (windowSize - y) /. float_of_int windowSize) *. 2. -. 1.;
      switch button {
      | Glut.LEFT_BUTTON =>
        /* TODO(sansouci): revisit this stuff */
        if (mouseState.mouseDown && gameState.started) {
          gameState.started = false
        } else {
          gameState.started = didClickOnStart puzzle::puzzle mouseState::mouseState
        };
        mouseState.mouseDown = state == Glut.DOWN
      | _ => ()
      }
    }
  );
  Glut.passiveMotionFunc
    cb::(
      fun x::x y::y => {
        mouseState.mouseX = (float_of_int x /. float_of_int windowSize) *. 2. -. 1.;
        mouseState.mouseY = (float_of_int (windowSize - y) /. float_of_int windowSize) *. 2. -. 1.
      }
    );
  let render () => {
    GlClear.clear [`color];
    GlMat.load_identity ();
    drawRect
      width::(int_of_float gridSize)
      height::(int_of_float gridSize)
      color::Color.yellow
      position::{x: 0., y: 0.};
    ignore @@ drawPuzzle puzzle;
    if gameState.started {
      let puzzleSize = float_of_int @@ List.length puzzle.grid;
      let centerPoint p => (p +. gridSize /. 2.) -. (puzzleSize *. 3. *. lineWeightf) /. 2.;
      let {x: startX, y: startY} = puzzle.startTile;
      drawCircle
        radius::lineWeight
        color::Color.brightYellow
        position::{
          x: centerPoint (startX *. 3. *. lineWeightf +. lineWeightf *. 1.5),
          y: centerPoint (startY *. 3. *. lineWeightf +. lineWeightf *. 1.5)
        }
    };
    Glut.swapBuffers ()
  };
  GlMat.mode `modelview;
  Glut.displayFunc cb::render;
  Glut.idleFunc cb::(Some Glut.postRedisplay);
  Glut.mainLoop ()
};
