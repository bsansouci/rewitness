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

type tileSidePointType = {position: pCoordType, tileSide: tileSideType};

type puzzleType = {startTile: pCoordType, endTile: tileSidePointType, grid: list (list tileType)};

type tilePointType = {tile: tileType, position: pCoordType};

type gameStateType = {mutable currentPath: list tilePointType, mutable lineEdge: option gCoordType};

let module Color = {
  let red = (1., 0., 0.);
  let yellow = (0.97, 0.7, 0.);
  let brightYellow = (0.985, 0.88, 0.3);
  let brown = (0.28, 0.21, 0.02);
  let grey = (0.27, 0.3, 0.32);
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

let frameWidth = lineWeight;

let windowSizef = float_of_int windowSize;

let toWorldCoord (WCoord {x, y}) => (
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
  List.iter GlDraw.vertex2 (List.map toWorldCoord [bottomLeft, topLeft, topRight, bottomRight]);
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
      toWorldCoord (
        WCoord {
          x: cos degInGrad *. floatRadius +. float_of_int x,
          y: sin degInGrad *. floatRadius +. float_of_int y
        }
      )
  };
  GlDraw.ends ()
};

let centerPoint puzzleSize::puzzleSize position::(GCoord {x, y}) =>
  GCoord {
    x: (x + windowSize / 2) - (puzzleSize * 3 * lineWeight) / 2,
    y: (y + windowSize / 2) - (puzzleSize * 3 * lineWeight) / 2
  };

let toGameCoord (PCoord {x, y}) => GCoord {x: x * 3 * lineWeight, y: y * 3 * lineWeight};

let getTileCenter (GCoord {x, y}) =>
  GCoord {x: x + (3 * lineWeight) / 2, y: y + (3 * lineWeight) / 2};

let drawCell tile::{bottom, left, top, right} color::color position::(GCoord {x, y}) => {
  let numOfSides = ref 0;
  if bottom {
    drawRect
      width::lineWeight
      height::(2 * lineWeight - lineWeight / 2)
      color::color
      position::(GCoord {x: x + lineWeight, y});
    numOfSides := !numOfSides + 1
  };
  if left {
    drawRect
      width::(2 * lineWeight - lineWeight / 2)
      height::lineWeight
      color::color
      position::(GCoord {x, y: y + lineWeight});
    numOfSides := !numOfSides + 1
  };
  if top {
    drawRect
      width::lineWeight
      height::(2 * lineWeight - lineWeight / 2)
      color::color
      position::(GCoord {x: x + lineWeight, y: y + lineWeight + lineWeight / 2});
    numOfSides := !numOfSides + 1
  };
  if right {
    drawRect
      width::(2 * lineWeight - lineWeight / 2)
      height::lineWeight
      color::color
      position::(GCoord {x: x + lineWeight + lineWeight / 2, y: y + lineWeight});
    numOfSides := !numOfSides + 1
  };
  if (!numOfSides >= 2) {
    drawCircle radius::(lineWeight / 2) color::color position::(getTileCenter (GCoord {x, y}))
  } else {
    let GCoord tileCenter = getTileCenter (GCoord {x, y});
    drawRect
      width::lineWeight
      height::lineWeight
      color::color
      position::(GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y - lineWeight / 2})
  }
};

let drawPuzzle puzzle::puzzle => {
  let puzzleSize = List.length puzzle.grid;
  ignore @@
    List.mapi
      (
        fun y row =>
          List.mapi
            (
              fun x tile =>
                drawCell
                  tile::tile
                  color::Color.brown
                  position::(
                    centerPoint puzzleSize::puzzleSize position::(toGameCoord (PCoord {x, y}))
                  )
            )
            row
      )
      (List.rev puzzle.grid);
  drawCircle
    radius::lineWeight
    color::Color.brown
    position::(
      getTileCenter (centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.startTile))
    )
};

let getTileSide puzzle::puzzle tile::tile position::(GCoord {x: px, y: py}) => {
  let puzzleSize = List.length puzzle.grid;
  let GCoord {x: tx, y: ty} = getTileCenter (
    centerPoint puzzleSize::puzzleSize position::(toGameCoord tile.position)
  );
  if (py < ty - 7) {
    Bottom
  } else if (px < tx - 7) {
    Left
  } else if (py > ty + 7) {
    Top
  } else if (
    px > tx + 7
  ) {
    Right
  } else {
    Center
  }
};

let drawTip puzzle::puzzle prevTile::maybePrevTile curTile::curTile lineEdge::(GCoord lineEdge) => {
  let puzzleSize = List.length puzzle.grid;
  let lineEdgeTileSide = getTileSide puzzle::puzzle tile::curTile position::(GCoord lineEdge);
  let GCoord tileCenter = getTileCenter (
    centerPoint puzzleSize::puzzleSize position::(toGameCoord curTile.position)
  );
  let drawSomething () =>
    switch lineEdgeTileSide {
    | Bottom =>
      drawRect
        width::lineWeight
        height::(tileCenter.y - lineEdge.y)
        color::Color.brightYellow
        position::(GCoord {x: tileCenter.x - lineWeight / 2, y: lineEdge.y})
    | Left =>
      drawRect
        width::(tileCenter.x - lineEdge.x)
        height::lineWeight
        color::Color.brightYellow
        position::(GCoord {x: lineEdge.x, y: tileCenter.y - lineWeight / 2})
    | Top =>
      drawRect
        width::lineWeight
        height::(lineEdge.y - tileCenter.y)
        color::Color.brightYellow
        position::(GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y})
    | Right =>
      drawRect
        width::(lineEdge.x - tileCenter.x)
        height::lineWeight
        color::Color.brightYellow
        position::(GCoord {x: tileCenter.x, y: tileCenter.y - lineWeight / 2})
    | Center => ()
    };
  switch maybePrevTile {
  | None => drawSomething ()
  | Some prevTile =>
    let prevTileSide =
      getTileSide
        puzzle::puzzle
        tile::curTile
        position::(
          getTileCenter (
            centerPoint puzzleSize::puzzleSize position::(toGameCoord prevTile.position)
          )
        );
    let distanceFromEdge = (3 * lineWeight) / 2;
    switch prevTileSide {
    | Top =>
      drawRect
        width::lineWeight
        height::((tileCenter.y + distanceFromEdge) - lineEdge.y)
        color::Color.brightYellow
        position::(GCoord {x: tileCenter.x - lineWeight / 2, y: lineEdge.y})
    | Right =>
      drawRect
        width::((tileCenter.x + distanceFromEdge) - lineEdge.x)
        height::lineWeight
        color::Color.brightYellow
        position::(GCoord {x: lineEdge.x, y: tileCenter.y - lineWeight / 2})
    | Bottom =>
      drawRect
        width::lineWeight
        height::(lineEdge.y - (tileCenter.y - distanceFromEdge))
        color::Color.brightYellow
        position::(GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y - distanceFromEdge})
    | Left =>
      drawRect
        width::(lineEdge.x - (tileCenter.x - distanceFromEdge))
        height::lineWeight
        color::Color.brightYellow
        position::(GCoord {x: tileCenter.x - distanceFromEdge, y: tileCenter.y - lineWeight / 2})
    | Center => assert false
    };
    /* This means we're moving away from the center */
    if (prevTileSide != lineEdgeTileSide) {
      drawSomething ();
      drawCircle radius::(lineWeight / 2) color::Color.brightYellow position::(GCoord tileCenter)
    }
  }
};

let maybeToTilePoint puzzle::puzzle position::(GCoord {x, y}) => {
  let puzzleSize = float_of_int (List.length puzzle.grid);
  let uncenteredPoint: floatPointType = {
    x: (float_of_int x +. (puzzleSize *. 3. *. lineWeightf) /. 2.) -. windowSizef /. 2.,
    y: (float_of_int y +. (puzzleSize *. 3. *. lineWeightf) /. 2.) -. windowSizef /. 2.
  };
  let puzzleCoord: floatPointType = {
    x: uncenteredPoint.x /. (3. *. lineWeightf),
    y: uncenteredPoint.y /. (3. *. lineWeightf)
  };
  if (
    puzzleCoord.x < 0. ||
      puzzleCoord.x >= puzzleSize || puzzleCoord.y < 0. || puzzleCoord.y >= puzzleSize
  ) {
    None
  } else {
    Some {
      tile:
        List.nth
          (List.nth (List.rev puzzle.grid) (int_of_float puzzleCoord.y))
          (int_of_float puzzleCoord.x),
      position: PCoord {x: int_of_float puzzleCoord.x, y: int_of_float puzzleCoord.y}
    }
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

let addSide curSide::ret cur::(PCoord cur) other::(PCoord other) =>
  if (other.y < cur.y) {
    {...ret, bottom: true}
  } else if (other.x < cur.x) {
    {...ret, left: true}
  } else if (
    other.y > cur.y
  ) {
    {...ret, top: true}
  } else if (
    other.x > cur.x
  ) {
    {...ret, right: true}
  } else {
    assert false
  };

let min3 a b c => min a (min b c);

let max3 a b c => max a (max b c);

let getOptimalLineEdge
    puzzle::puzzle
    gameState::gameState
    possibleDirs::possibleDirs
    lineEdge::(GCoord {x: lex, y: ley})
    mousePos::(GCoord {x: mx, y: my}) => {
  let mousePos = GCoord {x: mx, y: my};
  let lineEdge = GCoord {x: lex, y: ley};
  let lineEdgeTile =
    switch (maybeToTilePoint puzzle::puzzle position::lineEdge) {
    | None => assert false
    | Some x => x
    };
  let puzzleSize = List.length puzzle.grid;
  let GCoord {x: centerX, y: centerY} = getTileCenter (
    centerPoint puzzleSize::puzzleSize position::(toGameCoord lineEdgeTile.position)
  );
  let minDist = ref (getDistance mousePos lineEdge);
  let minCoord = ref lineEdge;
  if possibleDirs.bottom {
    let potentialLineEdge =
      GCoord {x: centerX, y: min (max3 my centerY (centerY - (3 * lineWeight) / 2)) (ley - 1)};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !minDist) {
      minDist := potentialDistance;
      minCoord := potentialLineEdge
    }
  };
  if possibleDirs.left {
    let potentialLineEdge =
      GCoord {x: min (max3 mx centerX (centerX - (3 * lineWeight) / 2)) (lex - 1), y: centerY};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !minDist) {
      minDist := potentialDistance;
      minCoord := potentialLineEdge
    }
  };
  if possibleDirs.top {
    let potentialLineEdge =
      GCoord {x: centerX, y: max (min3 my centerY (centerY + (3 * lineWeight) / 2)) (ley + 1)};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !minDist) {
      minDist := potentialDistance;
      minCoord := potentialLineEdge
    }
  };
  if possibleDirs.right {
    let potentialLineEdge =
      GCoord {x: max (min3 mx centerX (centerX + (3 * lineWeight) / 2)) (lex + 1), y: centerY};
    let potentialDistance = getDistance potentialLineEdge mousePos;
    if (potentialDistance < !minDist) {
      minDist := potentialDistance;
      minCoord := potentialLineEdge
    }
  };
  let maybeNextTile = maybeToTilePoint puzzle::puzzle position::!minCoord;
  switch maybeNextTile {
  | None => gameState
  | Some nextTile =>
    let getNextPath currentPath tile nextTile =>
      switch currentPath {
      | [] => [
          {
            tile: addSide curSide::B.n cur::tile.position other::nextTile.position,
            position: tile.position
          }
        ]
      | [head, ...tail] when head.position == nextTile.position => tail
      | [head, ...tail] =>
        if (List.length (List.filter (fun x => x.position == nextTile.position) tail) == 0) {
          [
            {
              tile:
                addSide
                  curSide::(addSide curSide::B.n cur::tile.position other::head.position)
                  cur::tile.position
                  other::nextTile.position,
              position: tile.position
            },
            head,
            ...tail
          ]
        } else {
          [head, ...tail]
        }
      };
    let nextLineEdge =
      switch gameState.currentPath {
      | [] => Some !minCoord
      | [head] => Some !minCoord
      | [head, ...tail] =>
        if (List.length (List.filter (fun x => x.position == nextTile.position) tail) == 0) {
          Some !minCoord
        } else {
          Some lineEdge
        }
      };
    if (nextTile == lineEdgeTile) {
      {...gameState, lineEdge: nextLineEdge}
    } else {
      switch (getTileSide puzzle::puzzle tile::nextTile position::!minCoord) {
      | Bottom =>
        if nextTile.tile.bottom {
          {
            currentPath: getNextPath gameState.currentPath lineEdgeTile nextTile,
            lineEdge: nextLineEdge
          }
        } else {
          gameState
        }
      | Left =>
        if nextTile.tile.left {
          {
            currentPath: getNextPath gameState.currentPath lineEdgeTile nextTile,
            lineEdge: nextLineEdge
          }
        } else {
          gameState
        }
      | Top =>
        if nextTile.tile.top {
          {
            currentPath: getNextPath gameState.currentPath lineEdgeTile nextTile,
            lineEdge: nextLineEdge
          }
        } else {
          gameState
        }
      | Right =>
        if nextTile.tile.right {
          {
            currentPath: getNextPath gameState.currentPath lineEdgeTile nextTile,
            lineEdge: nextLineEdge
          }
        } else {
          gameState
        }
      | Center => assert false
      }
    }
  }
};

let didClickOnStartTile puzzle::puzzle mousePos::mousePos => {
  let puzzleSize = List.length puzzle.grid;
  let circleCenter = getTileCenter (
    centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.startTile)
  );
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
          Some (
            getTileCenter (
              centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.startTile)
            )
          )
      | Some lineEdge =>
        let lineEdgeTile =
          switch (maybeToTilePoint puzzle::puzzle position::lineEdge) {
          | None => assert false
          | Some x => x
          };
        let lineTileSide = getTileSide puzzle::puzzle tile::lineEdgeTile position::lineEdge;
        let endTileCenter = getTileCenter (
          centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.endTile.position)
        );
        if (
          lineEdgeTile.position == puzzle.endTile.position &&
            lineTileSide == puzzle.endTile.tileSide &&
            (getDistance lineEdge endTileCenter >= lineWeightf || lineTileSide == Center)
        ) {
          assert false
        } else {
          gameState.lineEdge = None;
          gameState.currentPath = []
        }
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
      switch (maybeToTilePoint puzzle::puzzle position::lineEdge) {
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
  loop 10
};

let render puzzle::puzzle gameState::gameState () => {
  GlClear.clear [`color];
  GlMat.load_identity ();
  drawRect width::windowSize height::windowSize color::Color.grey position::(GCoord {x: 0, y: 0});
  drawRect
    width::(windowSize - frameWidth * 2)
    height::(windowSize - frameWidth * 2)
    color::Color.yellow
    position::(GCoord {x: frameWidth, y: frameWidth});
  drawPuzzle puzzle::puzzle;
  let puzzleSize = List.length puzzle.grid;
  let GCoord endTileCenter = getTileCenter (
    centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.endTile.position)
  );
  switch puzzle.endTile {
  | {tileSide: Bottom} =>
    drawCircle
      radius::(lineWeight / 2)
      color::Color.brown
      position::(GCoord {x: endTileCenter.x, y: endTileCenter.y - (3 * lineWeight) / 2})
  | {tileSide: Left} =>
    drawCircle
      radius::(lineWeight / 2)
      color::Color.brown
      position::(GCoord {x: endTileCenter.x - (3 * lineWeight) / 2, y: endTileCenter.y})
  | {tileSide: Top} =>
    drawCircle
      radius::(lineWeight / 2)
      color::Color.brown
      position::(GCoord {x: endTileCenter.x, y: endTileCenter.y + (3 * lineWeight) / 2})
  | {tileSide: Right} =>
    drawCircle
      radius::(lineWeight / 2)
      color::Color.brown
      position::(GCoord {x: endTileCenter.x + (3 * lineWeight) / 2, y: endTileCenter.y})
  | {tileSide: Center} => ()
  };
  switch gameState.lineEdge {
  | None => ()
  | Some lineEdge =>
    drawCircle
      radius::lineWeight
      color::Color.brightYellow
      position::(
        getTileCenter (centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.startTile))
      );
    ignore @@
      List.map
        (
          fun tilePoint =>
            drawCell
              tile::tilePoint.tile
              color::Color.brightYellow
              position::(
                centerPoint puzzleSize::puzzleSize position::(toGameCoord tilePoint.position)
              )
        )
        gameState.currentPath;
    switch (maybeToTilePoint puzzle::puzzle position::lineEdge) {
    | None => assert false
    | Some curTile =>
      switch gameState.currentPath {
      | [] => drawTip puzzle::puzzle prevTile::None curTile::curTile lineEdge::lineEdge
      | [head, ...tail] =>
        drawTip puzzle::puzzle prevTile::(Some head) curTile::curTile lineEdge::lineEdge
      }
    };
    /*drawCircle
      radius::(lineWeight / 2)
      color::Color.brown
      position::(
        getTileCenter (centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.endTile))
      );*/
    drawCircle radius::(lineWeight / 2) color::Color.brightYellow position::lineEdge
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
  startTile: PCoord {x: 4, y: 2},
  endTile: {position: PCoord {x: 2, y: 6}, tileSide: Top},
  grid: [
    [B.br, B.lr, B.blt, B.br, B.bl, B.br, B.bl],
    [B.bt, B.b, B.tr, B.lt, B.tr, B.lt, B.bt],
    [B.bt, B.btr, B.bl, B.r, B.blr, B.lr, B.blt],
    [B.t, B.bt, B.btr, B.lr, B.lt, B.b, B.bt],
    [B.r, B.blt, B.tr, B.blr, B.blr, B.blt, B.t],
    [B.b, B.t, B.b, B.tr, B.lt, B.tr, B.bl],
    [B.tr, B.lr, B.ltr, B.lr, B.lr, B.lr, B.lt]
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
