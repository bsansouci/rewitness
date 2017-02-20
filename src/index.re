/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Reglinterface;


/**
 * Main functor which takes an implementation of the Gl interface defined in Gl.re.
 * Gl.re is a general enough interface to work on both web and native so far.
 *
 * Scroll down to `drawPackageT` to read a more detailed description of how the rendering happens.
 *
 * At the top are simple rendering related types and utility functions. Right below are the game related
 * types and utility functions.
 */
module Make (Gl: Reglinterface.Gl.t) => {
  /* Setting up the Gl utils functions */
  type glCamera = {projectionMatrix: Gl.Mat4.t, modelViewMatrix: Gl.Mat4.t};
  type glEnv = {camera: glCamera, window: Gl.Window.t, gl: Gl.contextT};

  /**
   * Will mutate the projectionMatrix to be an ortho matrix with the given boundaries.
   * See this link for quick explanation of what this is.
   * https://shearer12345.github.io/graphics/assets/projectionPerspectiveVSOrthographic.png
   */
  let setProjection (window: Gl.Window.t) (camera: glCamera) =>
    Gl.Mat4.ortho
      out::camera.projectionMatrix
      left::0.
      right::(float_of_int (Gl.Window.getWidth window))
      bottom::0.
      top::(float_of_int (Gl.Window.getHeight window))
      near::0.
      far::100.;
  let resetCamera (camera: glCamera) => Gl.Mat4.identity camera.modelViewMatrix;
  let buildGlEnv window::(window: Gl.Window.t) :glEnv => {
    let gl = Gl.Window.getContext window;
    let glCamera = {projectionMatrix: Gl.Mat4.create (), modelViewMatrix: Gl.Mat4.create ()};
    let env = {camera: glCamera, window, gl};
    let canvasWidth = Gl.Window.getWidth window;
    let canvasHeight = Gl.Window.getHeight window;
    Gl.viewport gl 0 0 canvasWidth canvasHeight;
    Gl.clearColor gl 0.0 0.0 0.0 1.0;
    Gl.clear gl (Constants.color_buffer_bit lor Constants.depth_buffer_bit);
    env
  };
  let getProgram
      env::(env: glEnv)
      vertexShader::vertexShaderSource
      fragmentShader::fragmentShaderSource
      :option Gl.programT => {
    let vertexShader = Gl.createShader env.gl Constants.vertex_shader;
    Gl.shaderSource env.gl vertexShader vertexShaderSource;
    Gl.compileShader env.gl vertexShader;
    let compiledCorrectly =
      Gl.getShaderParameter context::env.gl shader::vertexShader paramName::Gl.Compile_status == 1;
    if compiledCorrectly {
      let fragmentShader = Gl.createShader env.gl Constants.fragment_shader;
      Gl.shaderSource env.gl fragmentShader fragmentShaderSource;
      Gl.compileShader env.gl fragmentShader;
      let compiledCorrectly =
        Gl.getShaderParameter context::env.gl shader::fragmentShader paramName::Gl.Compile_status == 1;
      if compiledCorrectly {
        let program = Gl.createProgram env.gl;
        Gl.attachShader context::env.gl ::program shader::vertexShader;
        Gl.deleteShader context::env.gl shader::vertexShader;
        Gl.attachShader context::env.gl ::program shader::fragmentShader;
        Gl.deleteShader context::env.gl shader::fragmentShader;
        Gl.linkProgram env.gl program;
        let linkedCorrectly =
          Gl.getProgramParameter context::env.gl ::program paramName::Gl.Link_status == 1;
        if linkedCorrectly {
          Some program
        } else {
          print_endline @@ "Linking error: " ^ Gl.getProgramInfoLog context::env.gl ::program;
          None
        }
      } else {
        print_endline @@
        "Fragment shader error: " ^ Gl.getShaderInfoLog context::env.gl shader::fragmentShader;
        None
      }
    } else {
      print_endline @@
      "Vertex shader error: " ^ Gl.getShaderInfoLog context::env.gl shader::vertexShader;
      None
    }
  };

  /** Setting up the game's datatypes. **/
  type tileType = {bottom: bool, left: bool, top: bool, right: bool};
  type tileSideType =
    | Bottom
    | Left
    | Top
    | Right
    | Center;
  type floatPointType = {x: float, y: float};
  type intPointType = {x: int, y: int};
  type wCoordType =
    | WCoord floatPointType;
  type gCoordType =
    | GCoord intPointType;
  type pCoordType =
    | PCoord intPointType;
  type tileSidePointType = {position: pCoordType, tileSide: tileSideType};
  type puzzleType = {
    startTile: pCoordType,
    endTile: tileSidePointType,
    grid: list (list tileType)
  };
  type tilePointType = {tile: tileType, position: pCoordType};
  type gameStateType = {
    mutable currentPath: list tilePointType,
    mutable lineEdge: option gCoordType
  };
  module Color = {
    let red = (1., 0., 0.);
    let yellow = (0.97, 0.7, 0.);
    let brightYellow = (0.985, 0.88, 0.3);
    let brown = (0.28, 0.21, 0.02);
    let grey = (0.27, 0.3, 0.32);
  };

  /**
   * This module helps us define the maze.
   * b -> bottom
   * l -> left
   * t -> top
   * r -> right
   *
   * bl -> bottom left
   *
   *  --
   *    |
   *
   * br -> bottom right
   *
   *      --
   *     |
   *
   * lt -> left top
   *
   *     |
   *   --
   *
   * etc...
   */
  module B = {
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

  /**
   * `drawPackage` is defined as a dumb way to package everything needed to render simple shapes in OpenGL.
   * It contains the projection matrix, the model view matrix, all the attributes and uniforms for the
   * shaders and the buffers needed (vertex and color).
   *
   * Here's a rundown of what everything means just so you can understand the code:
   * - shaders are scripts that are compiled, linked and ran on the GPU. They have a set of inputs (
   *   "attribute"s and "uniform"s) and outputs (like "varying" in the vertex shader or predefined ones like
   *   "gl_Position" and "gl_FragColor"). There are two kinds of shaders: vertex shaders and fragment shaders.
   *   Vertex shaders are basically a function that gets called on every vertex that you send to the graphics
   *   card. Fragment shaders are basically called on every pixel of the shape generated after running the
   *   vertex shader. This game uses the two most simple shaders, a vertex shader that set the gl_Position of
   *   every vertex and a fragment shader which set the gl_FragColor (its color) of the pixel.
   *   We pass down the vertex position and color through to the vertex shader through a thing called an
   *   "attribute". Then the color is piped to the fragment shader (and not used in the vertex shader), which
   *   will set the pixel color.
   *   You'd use a vertex shader to move your geometry around (we "move" it by translating it to screen
   *   coordinates) and distort it. You'd use a fragment shader to change the pixel colors, for a tint effect
   *   for example.
   *   A "uniform" is basically a global constant across vertices or pixels. Here it's the projection matrix
   *   and the model view matrix
   *
   * - the projection matrix is a matrix that is passed to the vertex shader which represents a
   *   transformation from a 3D space to a 2D space (ie: from the game world to our screen). For an already
   *   2D game, we simply use a matrix called "Orthogonal". Here's a great picture to discribe it
   *   https://shearer12345.github.io/graphics/assets/projectionPerspectiveVSOrthographic.png
   *
   * - don't worry about the model view matrix (read: I'm not sure why it's there).
   *
   * - the "buffer"s are basically space on the GPU's memory, which is where we actually store the data of
   *   the points.
   */
  type drawPackageT = {
    env: glEnv,
    vertexBuffer: Gl.bufferT,
    colorBuffer: Gl.bufferT,
    aVertexPosition: Gl.attributeT,
    aVertexColor: Gl.attributeT,
    pMatrixUniform: Gl.uniformT,
    mvMatrixUniform: Gl.uniformT
  };
  let drawRect
      drawPackage::{
        env: {gl, camera},
        vertexBuffer,
        colorBuffer,
        aVertexPosition,
        aVertexColor,
        pMatrixUniform,
        mvMatrixUniform
      }
      ::width
      ::height
      color::(r, g, b)
      position::(GCoord {x, y}) => {
    resetCamera camera;

    /** Setup vertices to be sent to the GPU **/
    let square_vertices = [|
      float_of_int @@ x + width,
      float_of_int @@ y + height,
      0.0,
      float_of_int x,
      float_of_int @@ y + height,
      0.0,
      float_of_int @@ x + width,
      float_of_int y,
      0.0,
      float_of_int x,
      float_of_int y,
      0.0
    |];
    Gl.bindBuffer context::gl target::Constants.array_buffer buffer::vertexBuffer;
    Gl.bufferData
      context::gl
      target::Constants.array_buffer
      data::(Gl.Bigarray.of_array Gl.Bigarray.Float32 square_vertices)
      usage::Constants.static_draw;
    Gl.vertexAttribPointer gl aVertexPosition 3 Constants.float_ false 0 0;

    /** Setup colors to be sent to the GPU **/
    let square_colors = [|r, g, b, 1., r, g, b, 1., r, g, b, 1., r, g, b, 1.|];
    Gl.bindBuffer context::gl target::Constants.array_buffer buffer::colorBuffer;
    Gl.bufferData
      context::gl
      target::Constants.array_buffer
      data::(Gl.Bigarray.of_array Gl.Bigarray.Float32 square_colors)
      usage::Constants.static_draw;
    Gl.vertexAttribPointer gl aVertexColor 4 Constants.float_ false 0 0;
    Gl.uniformMatrix4fv gl pMatrixUniform camera.projectionMatrix;
    Gl.uniformMatrix4fv gl mvMatrixUniform camera.modelViewMatrix;
    Gl.drawArrays gl Constants.triangle_strip 0 4
  };
  let drawCircle
      drawPackage::{
        env: {gl, camera},
        vertexBuffer,
        colorBuffer,
        aVertexPosition,
        aVertexColor,
        pMatrixUniform,
        mvMatrixUniform
      }
      ::radius
      color::(r, g, b)
      position::(GCoord {x, y}) => {
    resetCamera camera;

    /** Instantiate a list of points for the circle and bind to the circleBuffer. **/
    let circle_vertex = ref [];
    for i in 0 to 360 {
      let deg2grad = 3.14159 /. 180.;
      let degInGrad = float_of_int i *. deg2grad;
      let floatRadius = float_of_int radius;
      circle_vertex := [
        cos degInGrad *. floatRadius +. float_of_int x,
        sin degInGrad *. floatRadius +. float_of_int y,
        0.,
        ...!circle_vertex
      ]
    };
    Gl.bindBuffer gl Constants.array_buffer vertexBuffer;
    Gl.bufferData
      context::gl
      target::Constants.array_buffer
      data::(Gl.Bigarray.of_array Gl.Bigarray.Float32 (Array.of_list !circle_vertex))
      usage::Constants.static_draw;
    Gl.vertexAttribPointer gl aVertexPosition 3 Constants.float_ false 0 0;

    /** Instantiate color array **/
    let circle_colors = ref [];
    for i in 0 to 360 {
      circle_colors := [r, g, b, 1., ...!circle_colors]
    };
    Gl.bindBuffer gl Constants.array_buffer colorBuffer;
    Gl.bufferData
      context::gl
      target::Constants.array_buffer
      data::(Gl.Bigarray.of_array Gl.Bigarray.Float32 (Array.of_list !circle_colors))
      usage::Constants.static_draw;
    Gl.vertexAttribPointer gl aVertexColor 4 Constants.float_ false 0 0;
    Gl.uniformMatrix4fv gl pMatrixUniform camera.projectionMatrix;
    Gl.uniformMatrix4fv gl mvMatrixUniform camera.modelViewMatrix;
    Gl.drawArrays gl Constants.triangle_fan 0 360
  };
  let vertexShaderSource = {|
       attribute vec3 aVertexPosition;
       attribute vec4 aVertexColor;

       uniform mat4 uMVMatrix;
       uniform mat4 uPMatrix;

       varying vec4 vColor;

       void main(void) {
         gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
         vColor = aVertexColor;
       }|};
  let fragmentShaderSource = {|
       varying vec4 vColor;

       void main(void) {
         gl_FragColor = vColor;
       }
     |};
  let start () => {
    let window = Gl.Window.init argv::Sys.argv;
    Gl.Window.setWindowSize ::window width::windowSize height::windowSize;
    let env = buildGlEnv ::window;
    let program =
      switch (
        getProgram ::env vertexShader::vertexShaderSource fragmentShader::fragmentShaderSource
      ) {
      | None => failwith "Could not create the program and/or the shaders. Aborting."
      | Some program => program
      };
    Gl.useProgram env.gl program;
    let aVertexPosition = Gl.getAttribLocation env.gl program "aVertexPosition";
    Gl.enableVertexAttribArray env.gl aVertexPosition;
    let aVertexColor = Gl.getAttribLocation env.gl program "aVertexColor";
    Gl.enableVertexAttribArray env.gl aVertexColor;
    let pMatrixUniform = Gl.getUniformLocation env.gl program "uPMatrix";
    Gl.uniformMatrix4fv
      context::env.gl location::pMatrixUniform value::env.camera.projectionMatrix;
    let mvMatrixUniform = Gl.getUniformLocation env.gl program "uMVMatrix";
    Gl.uniformMatrix4fv
      context::env.gl location::mvMatrixUniform value::env.camera.modelViewMatrix;
    setProjection env.window env.camera;

    /** Create the drawPackage **/
    let drawPackage = {
      env,
      vertexBuffer: Gl.createBuffer env.gl,
      colorBuffer: Gl.createBuffer env.gl,
      aVertexPosition,
      aVertexColor,
      pMatrixUniform,
      mvMatrixUniform
    };
    /* Curry the drawing functions because we don't care about using them in a different context anyway */
    let myDrawRect = drawRect ::drawPackage;
    let myDrawCircle = drawCircle ::drawPackage;

    /** Bunch of utils function for the game logic **/
    let centerPoint ::puzzleSize position::(GCoord {x, y}) =>
      GCoord {
        x: x + windowSize / 2 - puzzleSize * 3 * lineWeight / 2,
        y: y + windowSize / 2 - puzzleSize * 3 * lineWeight / 2
      };
    let toGameCoord (PCoord {x, y}) => GCoord {x: x * 3 * lineWeight, y: y * 3 * lineWeight};
    let getTileCenter (GCoord {x, y}) =>
      GCoord {x: x + 3 * lineWeight / 2, y: y + 3 * lineWeight / 2};
    let getTileSide ::puzzle ::tile position::(GCoord {x: px, y: py}) => {
      let puzzleSize = List.length puzzle.grid;
      let GCoord {x: tx, y: ty} =
        getTileCenter (centerPoint ::puzzleSize position::(toGameCoord tile.position));
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
    let maybeToTilePoint ::puzzle position::(GCoord {x, y}) => {
      let puzzleSize = float_of_int (List.length puzzle.grid);
      let uncenteredPoint: floatPointType = {
        x: float_of_int x +. puzzleSize *. 3. *. lineWeightf /. 2. -. windowSizef /. 2.,
        y: float_of_int y +. puzzleSize *. 3. *. lineWeightf /. 2. -. windowSizef /. 2.
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
    /* let printTile tile => */
    /*   print_endline @@ */
    /*   "bottom: " ^ */
    /*   string_of_bool tile.bottom ^ */
    /*   ", left: " ^ */
    /*   string_of_bool tile.left ^ */
    /*   ", top: " ^ string_of_bool tile.top ^ ", right: " ^ string_of_bool tile.right; */
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

    /**
     * We check the current tile to see if we can move in the direction of the mouse. If we can, we move by
     * one tile in that direction. We also check that you don't draw over your `currentPath`.
     */
    let getOptimalLineEdge
        ::puzzle
        ::gameState
        ::possibleDirs
        lineEdge::(GCoord {x: lex, y: ley})
        mousePos::(GCoord {x: mx, y: my}) => {
      let mousePos = GCoord {x: mx, y: my};
      let lineEdge = GCoord {x: lex, y: ley};
      let lineEdgeTile =
        switch (maybeToTilePoint ::puzzle position::lineEdge) {
        | None => assert false
        | Some x => x
        };
      let puzzleSize = List.length puzzle.grid;
      let GCoord {x: centerX, y: centerY} =
        getTileCenter (centerPoint ::puzzleSize position::(toGameCoord lineEdgeTile.position));
      let minDist = ref (getDistance mousePos lineEdge);
      let minCoord = ref lineEdge;
      if possibleDirs.bottom {
        let potentialLineEdge =
          GCoord {x: centerX, y: min (max3 my centerY (centerY - 3 * lineWeight / 2)) (ley - 1)};
        let potentialDistance = getDistance potentialLineEdge mousePos;
        if (potentialDistance < !minDist) {
          minDist := potentialDistance;
          minCoord := potentialLineEdge
        }
      };
      if possibleDirs.left {
        let potentialLineEdge =
          GCoord {x: min (max3 mx centerX (centerX - 3 * lineWeight / 2)) (lex - 1), y: centerY};
        let potentialDistance = getDistance potentialLineEdge mousePos;
        if (potentialDistance < !minDist) {
          minDist := potentialDistance;
          minCoord := potentialLineEdge
        }
      };
      if possibleDirs.top {
        let potentialLineEdge =
          GCoord {x: centerX, y: max (min3 my centerY (centerY + 3 * lineWeight / 2)) (ley + 1)};
        let potentialDistance = getDistance potentialLineEdge mousePos;
        if (potentialDistance < !minDist) {
          minDist := potentialDistance;
          minCoord := potentialLineEdge
        }
      };
      if possibleDirs.right {
        let potentialLineEdge =
          GCoord {x: max (min3 mx centerX (centerX + 3 * lineWeight / 2)) (lex + 1), y: centerY};
        let potentialDistance = getDistance potentialLineEdge mousePos;
        if (potentialDistance < !minDist) {
          minDist := potentialDistance;
          minCoord := potentialLineEdge
        }
      };
      let maybeNextTile = maybeToTilePoint ::puzzle position::!minCoord;
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

        /** We check that you can't go on top of your current path. **/
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

        /** If we can't move we stop here and don't update currentPath. **/
        if (nextTile == lineEdgeTile) {
          {...gameState, lineEdge: nextLineEdge}
        } else {
          let nextGameState = {
            currentPath: getNextPath gameState.currentPath lineEdgeTile nextTile,
            lineEdge: nextLineEdge
          };
          switch (getTileSide ::puzzle tile::nextTile position::!minCoord) {
          | Bottom =>
            if nextTile.tile.bottom {
              nextGameState
            } else {
              gameState
            }
          | Left =>
            if nextTile.tile.left {
              nextGameState
            } else {
              gameState
            }
          | Top =>
            if nextTile.tile.top {
              nextGameState
            } else {
              gameState
            }
          | Right =>
            if nextTile.tile.right {
              nextGameState
            } else {
              gameState
            }
          | Center => assert false
          }
        }
      }
    };

    /** Drawing functions... Draw cell draws one of the cells of the grid of the maze. **/
    let drawCell tile::{bottom, left, top, right} ::color position::(GCoord {x, y}) => {
      let numOfSides = ref 0;
      if bottom {
        myDrawRect
          width::lineWeight
          height::(2 * lineWeight - lineWeight / 2)
          ::color
          position::(GCoord {x: x + lineWeight, y});
        numOfSides := !numOfSides + 1
      };
      if left {
        myDrawRect
          width::(2 * lineWeight - lineWeight / 2)
          height::lineWeight
          ::color
          position::(GCoord {x, y: y + lineWeight});
        numOfSides := !numOfSides + 1
      };
      if top {
        myDrawRect
          width::lineWeight
          height::(2 * lineWeight - lineWeight / 2)
          ::color
          position::(GCoord {x: x + lineWeight, y: y + lineWeight + lineWeight / 2});
        numOfSides := !numOfSides + 1
      };
      if right {
        myDrawRect
          width::(2 * lineWeight - lineWeight / 2)
          height::lineWeight
          ::color
          position::(GCoord {x: x + lineWeight + lineWeight / 2, y: y + lineWeight});
        numOfSides := !numOfSides + 1
      };
      if (!numOfSides >= 2) {
        myDrawCircle radius::(lineWeight / 2) ::color position::(getTileCenter (GCoord {x, y}))
      } else {
        let GCoord tileCenter = getTileCenter (GCoord {x, y});
        myDrawRect
          width::lineWeight
          height::lineWeight
          ::color
          position::(GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y - lineWeight / 2})
      }
    };

    /** This is the main loop that draws every cell. **/
    let drawPuzzle ::puzzle => {
      let puzzleSize = List.length puzzle.grid;
      List.iteri
        (
          fun y row =>
            List.iteri
              (
                fun x tile =>
                  drawCell
                    ::tile
                    color::Color.brown
                    position::(centerPoint ::puzzleSize position::(toGameCoord (PCoord {x, y})))
              )
              row
        )
        (List.rev puzzle.grid);
      myDrawCircle
        radius::lineWeight
        color::Color.brown
        position::(
          getTileCenter (centerPoint ::puzzleSize position::(toGameCoord puzzle.startTile))
        )
    };

    /** Draw the tip of the line if any. Just the tip. **/
    let drawTip ::puzzle prevTile::maybePrevTile ::curTile lineEdge::(GCoord lineEdge) => {
      let puzzleSize = List.length puzzle.grid;
      let lineEdgeTileSide = getTileSide ::puzzle tile::curTile position::(GCoord lineEdge);
      let GCoord tileCenter =
        getTileCenter (centerPoint ::puzzleSize position::(toGameCoord curTile.position));
      let drawSomething () =>
        switch lineEdgeTileSide {
        | Bottom =>
          myDrawRect
            width::lineWeight
            height::(tileCenter.y - lineEdge.y)
            color::Color.brightYellow
            position::(GCoord {x: tileCenter.x - lineWeight / 2, y: lineEdge.y})
        | Left =>
          myDrawRect
            width::(tileCenter.x - lineEdge.x)
            height::lineWeight
            color::Color.brightYellow
            position::(GCoord {x: lineEdge.x, y: tileCenter.y - lineWeight / 2})
        | Top =>
          myDrawRect
            width::lineWeight
            height::(lineEdge.y - tileCenter.y)
            color::Color.brightYellow
            position::(GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y})
        | Right =>
          myDrawRect
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
            ::puzzle
            tile::curTile
            position::(
              getTileCenter (centerPoint ::puzzleSize position::(toGameCoord prevTile.position))
            );
        let distanceFromEdge = 3 * lineWeight / 2;
        switch prevTileSide {
        | Top =>
          myDrawRect
            width::lineWeight
            height::(tileCenter.y + distanceFromEdge - lineEdge.y)
            color::Color.brightYellow
            position::(GCoord {x: tileCenter.x - lineWeight / 2, y: lineEdge.y})
        | Right =>
          myDrawRect
            width::(tileCenter.x + distanceFromEdge - lineEdge.x)
            height::lineWeight
            color::Color.brightYellow
            position::(GCoord {x: lineEdge.x, y: tileCenter.y - lineWeight / 2})
        | Bottom =>
          myDrawRect
            width::lineWeight
            height::(lineEdge.y - (tileCenter.y - distanceFromEdge))
            color::Color.brightYellow
            position::(
              GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y - distanceFromEdge}
            )
        | Left =>
          myDrawRect
            width::(lineEdge.x - (tileCenter.x - distanceFromEdge))
            height::lineWeight
            color::Color.brightYellow
            position::(
              GCoord {x: tileCenter.x - distanceFromEdge, y: tileCenter.y - lineWeight / 2}
            )
        | Center => assert false
        };
        /* This means we're moving away from the center */
        if (prevTileSide != lineEdgeTileSide) {
          drawSomething ();
          myDrawCircle
            radius::(lineWeight / 2) color::Color.brightYellow position::(GCoord tileCenter)
        }
      }
    };

    /** Event handlers for click and move. **/
    let didClickOnStartTile ::puzzle ::mousePos => {
      let puzzleSize = List.length puzzle.grid;
      let circleCenter =
        getTileCenter (centerPoint ::puzzleSize position::(toGameCoord puzzle.startTile));
      let distance = getDistance circleCenter mousePos;
      distance <= lineWeightf
    };
    let onMouseDown ::puzzle ::gameState ::button ::state ::x ::y => {
      let mousePos = GCoord {x, y: windowSize - y};
      let puzzleSize = List.length puzzle.grid;
      switch button {
      | Gl.Events.LeftButton =>
        if (state == Gl.Events.MouseDown) {
          switch gameState.lineEdge {
          | None when didClickOnStartTile ::puzzle ::mousePos =>
            gameState.lineEdge =
              Some (
                getTileCenter (centerPoint ::puzzleSize position::(toGameCoord puzzle.startTile))
              )
          | Some lineEdge =>
            let lineEdgeTile =
              switch (maybeToTilePoint ::puzzle position::lineEdge) {
              | None => assert false
              | Some x => x
              };
            let lineTileSide = getTileSide ::puzzle tile::lineEdgeTile position::lineEdge;
            let endTileCenter =
              getTileCenter (
                centerPoint ::puzzleSize position::(toGameCoord puzzle.endTile.position)
              );
            if (
              lineEdgeTile.position == puzzle.endTile.position &&
              lineTileSide == puzzle.endTile.tileSide && (
                getDistance lineEdge endTileCenter >= lineWeightf || lineTileSide == Center
              )
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

    /**
     * Computes the line drawn when the mouse is moved and mutates the gameState.
     * We call `getOptimalLineEdge` in a loop, about... 10 times.
     */
    let onMouseMove ::puzzle ::gameState ::x ::y => {
      let mousePos = GCoord {x, y: windowSize - y};
      let rec loop i =>
        switch gameState.lineEdge {
        | None => ()
        | Some lineEdge =>
          switch (maybeToTilePoint ::puzzle position::lineEdge) {
          | None => assert false
          | Some tilePoint =>
            let nextGameState =
              switch (getTileSide ::puzzle tile::tilePoint position::lineEdge) {
              | Bottom
              | Top =>
                getOptimalLineEdge ::puzzle ::gameState possibleDirs::B.bt ::lineEdge ::mousePos
              | Left
              | Right =>
                getOptimalLineEdge ::puzzle ::gameState possibleDirs::B.lr ::lineEdge ::mousePos
              | Center =>
                getOptimalLineEdge
                  ::puzzle ::gameState possibleDirs::tilePoint.tile ::lineEdge ::mousePos
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

    /**
     * Main render function.
     */
    let render ::puzzle ::gameState time => {
      Gl.clear env.gl (Constants.color_buffer_bit lor Constants.depth_buffer_bit);
      myDrawRect
        width::windowSize height::windowSize color::Color.grey position::(GCoord {x: 0, y: 0});
      myDrawRect
        width::(windowSize - frameWidth * 2)
        height::(windowSize - frameWidth * 2)
        color::Color.yellow
        position::(GCoord {x: frameWidth, y: frameWidth});
      drawPuzzle ::puzzle;
      let puzzleSize = List.length puzzle.grid;
      let GCoord endTileCenter =
        getTileCenter (centerPoint ::puzzleSize position::(toGameCoord puzzle.endTile.position));
      switch puzzle.endTile {
      | {tileSide: Bottom} =>
        myDrawCircle
          radius::(lineWeight / 2)
          color::Color.brown
          position::(GCoord {x: endTileCenter.x, y: endTileCenter.y - 3 * lineWeight / 2})
      | {tileSide: Left} =>
        myDrawCircle
          radius::(lineWeight / 2)
          color::Color.brown
          position::(GCoord {x: endTileCenter.x - 3 * lineWeight / 2, y: endTileCenter.y})
      | {tileSide: Top} =>
        myDrawCircle
          radius::(lineWeight / 2)
          color::Color.brown
          position::(GCoord {x: endTileCenter.x, y: endTileCenter.y + 3 * lineWeight / 2})
      | {tileSide: Right} =>
        myDrawCircle
          radius::(lineWeight / 2)
          color::Color.brown
          position::(GCoord {x: endTileCenter.x + 3 * lineWeight / 2, y: endTileCenter.y})
      | {tileSide: Center} => ()
      };
      switch gameState.lineEdge {
      | None => ()
      | Some lineEdge =>
        myDrawCircle
          radius::lineWeight
          color::Color.brightYellow
          position::(
            getTileCenter (centerPoint ::puzzleSize position::(toGameCoord puzzle.startTile))
          );
        ignore @@
        List.map
          (
            fun tilePoint =>
              drawCell
                tile::tilePoint.tile
                color::Color.brightYellow
                position::(centerPoint ::puzzleSize position::(toGameCoord tilePoint.position))
          )
          gameState.currentPath;
        switch (maybeToTilePoint ::puzzle position::lineEdge) {
        | None => assert false
        | Some curTile =>
          switch gameState.currentPath {
          | [] => drawTip ::puzzle prevTile::None ::curTile ::lineEdge
          | [head, ...tail] => drawTip ::puzzle prevTile::(Some head) ::curTile ::lineEdge
          }
        };
        myDrawCircle radius::(lineWeight / 2) color::Color.brightYellow position::lineEdge
      }
    };
    let gameState = {currentPath: [], lineEdge: None};

    /**
     * Main entry point to the Gl module.
     * This is what starts the rendering loop and the event loop.
     * In the native case they are both synchronous and synchronized. In the web case, they're both async.
     */
    Gl.render
      ::window
      mouseDown::(onMouseDown puzzle::examplePuzzle ::gameState)
      mouseMove::(onMouseMove puzzle::examplePuzzle ::gameState)
      displayFunc::(render puzzle::examplePuzzle ::gameState)
      ()
  };
};
