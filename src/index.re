/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type tileType = {bottom: bool, left: bool, top: bool, right: bool};

type tileSideType = | Bottom | Left | Top | Right | Center;

type floatPointType = {x: float, y: float};

type intPointType = {x: int, y: int};

type wCoordType = | WCoord of floatPointType;

type gCoordType = | GCoord of intPointType;

type pCoordType = | PCoord of intPointType;

type puzzleType = {startTile: pCoordType, endTile: pCoordType, grid: list (list tileType)};

type tilePointType = {tile: tileType, position: pCoordType};

type gameStateType = {mutable currentPath: list tilePointType, mutable lineEdge: option gCoordType};

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

let drawCell (GCoord {x, y}) {bottom, left, top, right} color => {
  if bottom {
    drawRect
      width::lineWeight
      height::(2 * lineWeight)
      color::color
      position::(GCoord {x: x + lineWeight, y})
  };
  if left {
    drawRect
      width::(2 * lineWeight)
      height::lineWeight
      color::color
      position::(GCoord {x, y: y + lineWeight})
  };
  if top {
    drawRect
      width::lineWeight
      height::(2 * lineWeight)
      color::color
      position::(GCoord {x: x + lineWeight, y: y + lineWeight})
  };
  if right {
    drawRect
      width::(2 * lineWeight)
      height::lineWeight
      color::color
      position::(GCoord {x: x + lineWeight, y: y + lineWeight})
  }
};

let drawPuzzle puzzle => {
  let puzzleSize = List.length puzzle.grid;
  ignore @@
    List.mapi
      (
        fun y row =>
          List.mapi
            (
              fun x cell =>
                drawCell (centerPoint puzzleSize (toGameCoord (PCoord {x, y}))) cell Color.brown
            )
            row
      )
      (List.rev puzzle.grid);
  drawCircle
    radius::lineWeight
    color::Color.brown
    position::(getCircleCenter (centerPoint puzzleSize (toGameCoord puzzle.startTile)))
};

let toTile puzzle (GCoord {x, y}) => {
  let puzzleSize = List.length puzzle.grid;
  let uncenteredPoint = {
    x: (x + (puzzleSize * 3 * lineWeight) / 2) - windowSize / 2,
    y: (y + (puzzleSize * 3 * lineWeight) / 2) - windowSize / 2
  };
  let puzzleCoord = {
    x: uncenteredPoint.x / (3 * lineWeight),
    y: uncenteredPoint.y / (3 * lineWeight)
  };
  if (
    puzzleCoord.x < 0 ||
      puzzleCoord.x >= puzzleSize || puzzleCoord.y < 0 || puzzleCoord.y >= puzzleSize
  ) {
    None
  } else {
    Some {
      tile: List.nth (List.nth (List.rev puzzle.grid) puzzleCoord.y) puzzleCoord.x,
      position: PCoord puzzleCoord
    }
  }
};

let getTileSide puzzle::puzzle tile::tile position::(GCoord {x: px, y: py}) => {
  let puzzleSize = List.length puzzle.grid;
  let GCoord {x: tx, y: ty} = getCircleCenter (centerPoint puzzleSize (toGameCoord tile.position));
  if (py < ty) {
    Bottom
  } else if (px < tx) {
    Left
  } else if (py > ty) {
    Top
  } else if (px > tx) {
    Right
  } else {
    Center
  }
};

let getDistance (GCoord {x: x1, y: y1}) (GCoord {x: x2, y: y2}) => {
  let dx = x2 - x1;
  let dy = y2 - y1;
  sqrt @@ float_of_int (dx * dx + dy * dy)
};

let print_tile tile => print_endline @@ (
  "bottom: " ^
    string_of_bool tile.bottom ^
    ", left: " ^
    string_of_bool tile.left ^
    ", top: " ^
    string_of_bool tile.top ^
    ", right: " ^
    string_of_bool tile.right
);

let getOptimalLineEdge
    puzzle::puzzle
    gameState::gameState
    possibleDirs::possibleDirs
    lineEdge::(GCoord {x: lex, y: ley})
    mousePos::mousePos => {
  let lineEdge = GCoord {x: lex, y: ley};
  let min = ref windowSizef;
  let minCoord = ref lineEdge;
  if possibleDirs.bottom {
    let potentialLineEdge = GCoord {x: lex, y: ley - 1};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !min) {
      min.contents = potentialDistance;
      minCoord.contents = potentialLineEdge
    }
  };
  if possibleDirs.left {
    let potentialLineEdge = GCoord {x: lex - 1, y: ley};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !min) {
      min.contents = potentialDistance;
      minCoord.contents = potentialLineEdge
    }
  };
  if possibleDirs.top {
    let potentialLineEdge = GCoord {x: lex, y: ley + 1};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !min) {
      min.contents = potentialDistance;
      minCoord.contents = potentialLineEdge
    }
  };
  if possibleDirs.right {
    let potentialLineEdge = GCoord {x: lex + 1, y: ley};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !min) {
      min.contents = potentialDistance;
      minCoord.contents = potentialLineEdge
    }
  };
  let nextTile = toTile puzzle !minCoord;
  let lineEdgeTile = toTile puzzle lineEdge;
  let getNextPath currentPath tile nextTile => {
    switch currentPath {
      | [] => [tile]
      | [head, ...tail] when head == nextTile => tail
      | l => [tile, ...l]
    }
  };
  switch nextTile {
  | None => gameState
  | Some t =>
    if (nextTile == lineEdgeTile) {
      {...gameState, lineEdge: Some !minCoord}
    } else {
      switch lineEdgeTile {
      | None => assert false
      | Some leTile =>
        switch (getTileSide puzzle::puzzle tile::t position::!minCoord) {
        | Bottom =>
          if t.tile.bottom {
            {currentPath: getNextPath gameState.currentPath leTile t, lineEdge: Some !minCoord}
          } else {
            gameState
          }
        | Left =>
          if t.tile.left {
            {currentPath: getNextPath gameState.currentPath leTile t, lineEdge: Some !minCoord}
          } else {
            gameState
          }
        | Top =>
          if t.tile.top {
            {currentPath: getNextPath gameState.currentPath leTile t, lineEdge: Some !minCoord}
          } else {
            gameState
          }
        | Right =>
          if t.tile.right {
            {currentPath: getNextPath gameState.currentPath leTile t, lineEdge: Some !minCoord}
          } else {
            gameState
          }
        | Center => assert false
        }
      }
    }
  }
};

let didClickOnStartTile puzzle::puzzle mousePos::mousePos => {
  let puzzleSize = List.length puzzle.grid;
  let circleCenter = getCircleCenter (centerPoint puzzleSize (toGameCoord puzzle.startTile));
  let distance = getDistance circleCenter mousePos;
  distance <= lineWeightf
};

let mouseDidClick puzzle::puzzle gameState::gameState button::button state::state x::x y::y => {
  let mousePos = GCoord {x, y: windowSize - y};
  let puzzleSize = List.length puzzle.grid;
  switch button {
  | Glut.LEFT_BUTTON =>
    if (state == Glut.DOWN) {
      switch gameState.lineEdge {
      | None when didClickOnStartTile puzzle::puzzle mousePos::mousePos =>
        gameState.lineEdge =
          Some (getCircleCenter (centerPoint puzzleSize (toGameCoord puzzle.startTile)))
      | Some _ =>
        gameState.lineEdge = None;
        gameState.currentPath = []
      | _ => ()
      }
    }
  | _ => ()
  }
};

let mouseDidMove puzzle::puzzle gameState::gameState x::x y::y => {
  let mousePos = GCoord {x, y: windowSize - y};
  let rec loop i =>
    switch gameState.lineEdge {
    | None => ()
    | Some lineEdge =>
      switch (toTile puzzle lineEdge) {
      | None => assert false
      | Some tilePoint =>
        let nextGameState =
          switch (getTileSide puzzle::puzzle tile::tilePoint position::lineEdge) {
          | Bottom
          | Top =>
            getOptimalLineEdge
              puzzle::puzzle
              gameState::gameState
              possibleDirs::B.bt
              lineEdge::lineEdge
              mousePos::mousePos
          | Left
          | Right =>
            getOptimalLineEdge
              puzzle::puzzle
              gameState::gameState
              possibleDirs::B.lr
              lineEdge::lineEdge
              mousePos::mousePos
          | Center =>
            getOptimalLineEdge
              puzzle::puzzle
              gameState::gameState
              possibleDirs::tilePoint.tile
              lineEdge::lineEdge
              mousePos::mousePos
          };
        if (nextGameState != gameState) {
          gameState.currentPath = nextGameState.currentPath;
          gameState.lineEdge = nextGameState.lineEdge;
          if (i > 0) {
            loop (i - 1)
          }
        }
      }
    };
  loop 100
};

let render puzzle::puzzle gameState::gameState () => {
  GlClear.clear [`color];
  GlMat.load_identity ();
  drawRect width::windowSize height::windowSize color::Color.yellow position::(GCoord {x: 0, y: 0});
  ignore @@ drawPuzzle puzzle;
  switch gameState.lineEdge {
  | None => ()
  | Some lineEdge =>
    let puzzleSize = List.length puzzle.grid;
    drawCircle
      radius::lineWeight
      color::Color.brightYellow
      position::(getCircleCenter (centerPoint puzzleSize (toGameCoord puzzle.startTile)));
    ignore @@
      List.map
        (
          fun tilePoint =>
            drawCell
              (centerPoint puzzleSize (toGameCoord tilePoint.position))
              tilePoint.tile
              Color.brightYellow
        )
        gameState.currentPath;
    /* drawCircle radius::(lineWeight / 3) color::Color.red position::lineEdge; */
  };
  Glut.swapBuffers ()
};

/**
 *
 * we have mousePos (which is a GCoord), current lineEdge position (which is a GCoord)
 * every frame we do:
 *   get available directions, based on the puzzle tile of the current lineEdge
 *   out of those, iterate through each direction
 *     add 1 to the lineEdge position in that direction
 *     check the distance from that new point to the mousePos
 *     if that distance is smaller than the running min, update running min
 *   now we have the direction to move in
 *   we now move as far as possible in that direction until moving further along the line moves
 *   you further away from the mouse OR we hit the end of a line
 *
 * later: moving as far as possible optimized algorithm
 *
 */
let examplePuzzle = {
  startTile: PCoord {x: 0, y: 3},
  endTile: PCoord {x: 0, y: 0},
  grid: [
    [B.r, B.lr, B.bl, B.bl],
    [B.bt, B.br, B.bt, B.bt],
    [B.btr, B.lt, B.tr, B.blt],
    [B.tr, B.blr, B.lr, B.lt]
  ]
};

let () = {
  ignore @@ Glut.init Sys.argv;
  Glut.initWindowSize windowSize windowSize;
  Glut.initDisplayMode double_buffer::true ();
  ignore @@ Glut.createWindow title::"A Reason To Witness";
  let gameState = {currentPath: [], lineEdge: None};
  let puzzle = examplePuzzle;
  Glut.mouseFunc (mouseDidClick puzzle::puzzle gameState::gameState);
  Glut.passiveMotionFunc (mouseDidMove puzzle::puzzle gameState::gameState);
  GlMat.mode `modelview;
  Glut.displayFunc (render puzzle::puzzle gameState::gameState);
  Glut.idleFunc cb::(Some Glut.postRedisplay);
  Glut.mainLoop ()
};
