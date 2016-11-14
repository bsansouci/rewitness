/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
module type GlT = {
  let target: string;
  type contextT;
  module type WindowT = {
    type t;
    let getWidth: t => int;
    let getHeight: t => int;
    let init: argv::array string => t;
    let setWindowSize: window::t => width::int => height::int => unit;
    let initDisplayMode: window::t => double_buffer::bool => unit => unit;
    let getContext: t => contextT;
  };
  let module Window: WindowT;
  module type EventsT = {
    type buttonStateT =
      | LEFT_BUTTON
      | MIDDLE_BUTTON
      | RIGHT_BUTTON;
    type stateT =
      | DOWN
      | UP;
    let onMouseDown:
      window::Window.t =>
      (button::buttonStateT => state::stateT => x::int => y::int => unit) =>
      unit;
    let onMouseMove: window::Window.t => (x::int => y::int => unit) => unit;
  };
  let module Events: EventsT;
  let displayFunc: window::Window.t => cb::(float => unit) => unit;
  type programT;
  type shaderT;
  let clearColor: context::contextT => r::float => g::float => b::float => a::float => unit;
  let createProgram: context::contextT => programT;
  let createShader: context::contextT => shaderType::int => shaderT;
  let attachShader: context::contextT => program::programT => shader::shaderT => unit;
  let shaderSource: context::contextT => shader::shaderT => source::string => unit;
  let compileShader: context::contextT => shader::shaderT => unit;
  let linkProgram: context::contextT => program::programT => unit;
  let useProgram: context::contextT => program::programT => unit;
  type bufferT;
  type attributeT;
  type uniformT;
  type boolT;
  let false_: boolT;
  let true_: boolT;
  type float32Array;
  type uint16Array;
  let createBuffer: context::contextT => bufferT;
  let bindBuffer: context::contextT => target::int => buffer::bufferT => unit;
  /* TODO: unhack this */
  type dataKind =
    | Float32 float32Array
    | UInt16 uint16Array;
  let bufferData: context::contextT => target::int => data::dataKind => usage::int => unit;
  let viewport: context::contextT => x::int => y::int => width::int => height::int => unit;
  let clear: context::contextT => mask::int => unit;
  /* TODO: We'll need to do something about this */
  let createFloat32Array: array float => float32Array;
  let createUint16Array: array int => uint16Array;
  let getUniformLocation: context::contextT => program::programT => name::string => uniformT;
  let getAttribLocation: context::contextT => program::programT => name::string => attributeT;
  let enableVertexAttribArray: context::contextT => attribute::attributeT => unit;
  let vertexAttribPointer:
    context::contextT =>
    attribute::attributeT =>
    size::int =>
    type_::int =>
    normalize::boolT =>
    stride::int =>
    offset::int =>
    unit;
  module type Mat4T = {
    type t;
    let to_array: t => array float;
    let create: unit => t;
    /* let perspective: out::t => fovy::int => aspect::float => near::float => far::float => unit; */
    let identity: out::t => unit;
    let translate: out::t => matrix::t => vec::array float => unit;
    let scale: out::t => matrix::t => vec::array float => unit;
    let rotate: out::t => matrix::t => rad::float => vec::array float => unit;
    let ortho:
      out::t =>
      left::float =>
      right::float =>
      bottom::float =>
      top::float =>
      near::float =>
      far::float =>
      unit;
  };
  let module Mat4: Mat4T;
  let uniformMatrix4fv:
    context::contextT => location::uniformT => transpose::boolT => value::Mat4.t => unit;
  /* let getCompileStatus: context::contextT => shader::shaderT => bool; */
  /* let getLinkStatus: context::contextT => program::programT => bool; */
  /* Can return other value types as well, see https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Types */
  type ret =
    | Bool bool
    | Int int;
  type shaderParamsT =
    | Shader_delete_status
    | Compile_status
    | Shader_type;
  type programParamsT =
    | Program_delete_status
    | Link_status
    | Validate_status;
  let getProgramParameter:
    context::contextT => program::programT => paramName::programParamsT => ret;
  let getShaderParameter: context::contextT => shader::shaderT => paramName::shaderParamsT => ret;
  let getShaderInfoLog: context::contextT => shader::shaderT => string;
  let getProgramInfoLog: context::contextT => program::programT => string;
  let getShaderSource: context::contextT => shader::shaderT => string;
  let drawArrays: context::contextT => mode::int => first::int => count::int => unit;
  let drawElements:
    context::contextT => mode::int => count::int => type_::int => offset::int => unit;
};

/* module type GameDepsT = {let module Gl: GlT; let module Mat4: Mat4T;}; */
let module Make (Gl: GlT) => {
  /* let module Gl = GameDeps.Gl; */
  /* let module Mat4 = GameDeps.Mat4; */
  /* Setting up the Gl utils functions */
  type gl_camera = {projection_matrix: Gl.Mat4.t, model_view_matrix: Gl.Mat4.t};
  type gl_env = {camera: gl_camera, window: Gl.Window.t, gl: Gl.contextT};
  let set_projection (window: Gl.Window.t) (camera: gl_camera) =>
    Gl.Mat4.ortho
      out::camera.projection_matrix
      left::0.
      right::(float_of_int (Gl.Window.getWidth window))
      bottom::0.
      top::(float_of_int (Gl.Window.getHeight window))
      near::0.
      far::100.;
  let translate_camera (camera: gl_camera) (offset: array float) =>
    Gl.Mat4.translate camera.model_view_matrix camera.model_view_matrix offset;
  let reset_mv (camera: gl_camera) => Gl.Mat4.identity camera.model_view_matrix;
  let build_gl_env (window: Gl.Window.t) :gl_env => {
    let gl = Gl.Window.getContext window;
    let gl_camera = {projection_matrix: Gl.Mat4.create (), model_view_matrix: Gl.Mat4.create ()};
    let env = {camera: gl_camera, window, gl};
    let canvas_width = Gl.Window.getWidth window;
    let canvas_height = Gl.Window.getHeight window;
    Gl.viewport gl 0 0 canvas_width canvas_height;
    Gl.clearColor gl 0.0 0.0 0.0 1.0;
    Gl.clear gl (Constants.color_buffer_bit lor Constants.depth_buffer_bit);
    env
  };
  let addProgram (env: gl_env) vertex_shader_source fragment_shader_source :option Gl.programT => {
    let vertex_shader = Gl.createShader env.gl Constants.vertex_shader;
    Gl.shaderSource env.gl vertex_shader vertex_shader_source;
    Gl.compileShader env.gl vertex_shader;
    let compiledCorrectly =
      switch (
        Gl.getShaderParameter context::env.gl shader::vertex_shader paramName::Gl.Compile_status
      ) {
      | Gl.Int _ => assert false
      | Gl.Bool x => x
      };
    if compiledCorrectly {
      let fragment_shader = Gl.createShader env.gl Constants.fragment_shader;
      Gl.shaderSource env.gl fragment_shader fragment_shader_source;
      Gl.compileShader env.gl fragment_shader;
      let compiledCorrectly =
        switch (
          Gl.getShaderParameter
            context::env.gl shader::fragment_shader paramName::Gl.Compile_status
        ) {
        | Gl.Int _ => assert false
        | Gl.Bool x => x
        };
      if compiledCorrectly {
        let program = Gl.createProgram env.gl;
        Gl.attachShader context::env.gl program::program shader::vertex_shader;
        /* Gl.deleteShader */
        Gl.attachShader context::env.gl program::program shader::fragment_shader;
        /* Gl.deleteShader */
        Gl.linkProgram env.gl program;
        let linkedCorrectly =
          switch (
            Gl.getProgramParameter context::env.gl program::program paramName::Gl.Link_status
          ) {
          | Gl.Int _ => assert false
          | Gl.Bool x => x
          };
        if linkedCorrectly {
          Some program
        } else {
          print_endline @@
          "Linking error: " ^ Gl.getProgramInfoLog context::env.gl program::program;
          None
        }
      } else {
        print_endline @@
        "Fragment shader error: " ^ Gl.getShaderInfoLog context::env.gl shader::fragment_shader;
        None
      }
    } else {
      print_endline @@
      "Vertex shader error: " ^ Gl.getShaderInfoLog context::env.gl shader::vertex_shader;
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
  let preprocess target shader =>
    if (target == "web") {
      "#version 100 \n precision highp float; \n" ^ shader
    } else if (
      target == "native"
    ) {
      "#version 120 \n" ^ shader
    } else {
      shader
    };
  let start () => {
    let vertex_shader_source =
      preprocess
        Gl.target
        {|
       attribute vec3 aVertexPosition;
       attribute vec4 aVertexColor;

       uniform mat4 uMVMatrix;
       uniform mat4 uPMatrix;

       varying vec4 vColor;

       void main(void) {
         gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
         vColor = aVertexColor;
       }|};
    let fragment_shader_source =
      preprocess
        Gl.target
        {|
       varying vec4 vColor;

       void main(void) {
         gl_FragColor = vColor;
       }
     |};
    let window = Gl.Window.init argv::Sys.argv;
    Gl.Window.setWindowSize window::window width::windowSize height::windowSize;
    let env = build_gl_env window;
    let program =
      switch (addProgram env vertex_shader_source fragment_shader_source) {
      | None => assert false
      | Some program => program
      };
    Gl.useProgram env.gl program;
    let position_attrib = Gl.getAttribLocation env.gl program "aVertexPosition";
    let color_attrib = Gl.getAttribLocation env.gl program "aVertexColor";
    Gl.enableVertexAttribArray env.gl position_attrib;
    Gl.enableVertexAttribArray env.gl color_attrib;
    let p_matrix_uniform = Gl.getUniformLocation env.gl program "uPMatrix";
    let mv_matrix_uniform = Gl.getUniformLocation env.gl program "uMVMatrix";
    Gl.uniformMatrix4fv
      context::env.gl
      location::p_matrix_uniform
      transpose::Gl.false_
      value::env.camera.projection_matrix;
    Gl.uniformMatrix4fv
      context::env.gl
      location::mv_matrix_uniform
      transpose::Gl.false_
      value::env.camera.model_view_matrix;
    set_projection env.window env.camera;
    let drawRect
        vertex_buffer::vertex_buffer
        color_buffer::color_buffer
        width::width
        height::height
        color::(r, g, b)
        position::(GCoord {x, y}) => {
      reset_mv env.camera;
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
      Gl.bindBuffer context::env.gl target::Constants.array_buffer buffer::vertex_buffer;
      Gl.bufferData
        context::env.gl
        target::Constants.array_buffer
        data::(Gl.Float32 (Gl.createFloat32Array square_vertices))
        usage::Constants.static_draw;
      Gl.vertexAttribPointer env.gl position_attrib 3 Constants.float_ Gl.false_ 0 0;

      /** Colors **/
      let square_colors = ref [];
      for i in 0 to 3 {
        square_colors := [r, g, b, 1., ...!square_colors]
      };
      Gl.bindBuffer context::env.gl target::Constants.array_buffer buffer::color_buffer;
      Gl.bufferData
        context::env.gl
        target::Constants.array_buffer
        data::(Gl.Float32 (Gl.createFloat32Array (Array.of_list !square_colors)))
        usage::Constants.static_draw;
      Gl.vertexAttribPointer env.gl color_attrib 4 Constants.float_ Gl.false_ 0 0;
      Gl.uniformMatrix4fv env.gl p_matrix_uniform Gl.false_ env.camera.projection_matrix;
      Gl.uniformMatrix4fv env.gl mv_matrix_uniform Gl.false_ env.camera.model_view_matrix;
      Gl.drawArrays env.gl Constants.triangle_strip 0 4
    };
    let drawCircle
        vertex_buffer::vertex_buffer
        color_buffer::color_buffer
        radius::radius
        color::(r, g, b)
        position::(GCoord {x, y}) => {
      reset_mv env.camera;

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
      Gl.bindBuffer env.gl Constants.array_buffer vertex_buffer;
      Gl.bufferData
        context::env.gl
        target::Constants.array_buffer
        data::(Gl.Float32 (Gl.createFloat32Array (Array.of_list !circle_vertex)))
        usage::Constants.static_draw;
      Gl.vertexAttribPointer env.gl position_attrib 3 Constants.float_ Gl.false_ 0 0;

      /** Instantiate color array **/
      let circle_colors = ref [];
      for i in 0 to 360 {
        circle_colors := [r, g, b, 1., ...!circle_colors]
      };
      Gl.bindBuffer env.gl Constants.array_buffer color_buffer;
      Gl.bufferData
        context::env.gl
        target::Constants.array_buffer
        data::(Gl.Float32 (Gl.createFloat32Array (Array.of_list !circle_colors)))
        usage::Constants.static_draw;
      Gl.vertexAttribPointer env.gl color_attrib 4 Constants.float_ Gl.false_ 0 0;
      Gl.uniformMatrix4fv env.gl p_matrix_uniform Gl.false_ env.camera.projection_matrix;
      Gl.uniformMatrix4fv env.gl mv_matrix_uniform Gl.false_ env.camera.model_view_matrix;
      Gl.drawArrays env.gl Constants.triangle_fan 0 360
    };
    let myDrawRect =
      drawRect vertex_buffer::(Gl.createBuffer env.gl) color_buffer::(Gl.createBuffer env.gl);
    let myDrawCircle =
      drawCircle vertex_buffer::(Gl.createBuffer env.gl) color_buffer::(Gl.createBuffer env.gl);
    let centerPoint puzzleSize::puzzleSize position::(GCoord {x, y}) =>
      GCoord {
        x: x + windowSize / 2 - puzzleSize * 3 * lineWeight / 2,
        y: y + windowSize / 2 - puzzleSize * 3 * lineWeight / 2
      };
    let toGameCoord (PCoord {x, y}) => GCoord {x: x * 3 * lineWeight, y: y * 3 * lineWeight};
    let getTileCenter (GCoord {x, y}) =>
      GCoord {x: x + 3 * lineWeight / 2, y: y + 3 * lineWeight / 2};
    let drawCell tile::{bottom, left, top, right} color::color position::(GCoord {x, y}) => {
      let numOfSides = ref 0;
      if bottom {
        myDrawRect
          width::lineWeight
          height::(2 * lineWeight - lineWeight / 2)
          color::color
          position::(GCoord {x: x + lineWeight, y});
        numOfSides := !numOfSides + 1
      };
      if left {
        myDrawRect
          width::(2 * lineWeight - lineWeight / 2)
          height::lineWeight
          color::color
          position::(GCoord {x, y: y + lineWeight});
        numOfSides := !numOfSides + 1
      };
      if top {
        myDrawRect
          width::lineWeight
          height::(2 * lineWeight - lineWeight / 2)
          color::color
          position::(GCoord {x: x + lineWeight, y: y + lineWeight + lineWeight / 2});
        numOfSides := !numOfSides + 1
      };
      if right {
        myDrawRect
          width::(2 * lineWeight - lineWeight / 2)
          height::lineWeight
          color::color
          position::(GCoord {x: x + lineWeight + lineWeight / 2, y: y + lineWeight});
        numOfSides := !numOfSides + 1
      };
      if (!numOfSides >= 2) {
        myDrawCircle
          radius::(lineWeight / 2) color::color position::(getTileCenter (GCoord {x, y}))
      } else {
        let GCoord tileCenter = getTileCenter (GCoord {x, y});
        myDrawRect
          width::lineWeight
          height::lineWeight
          color::color
          position::(GCoord {x: tileCenter.x - lineWeight / 2, y: tileCenter.y - lineWeight / 2})
      }
    };
    let drawPuzzle puzzle::puzzle => {
      let puzzleSize = List.length puzzle.grid;
      List.iteri
        (
          fun y row =>
            List.iteri
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
      myDrawCircle
        radius::lineWeight
        color::Color.brown
        position::(
          getTileCenter (
            centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.startTile)
          )
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
            puzzle::puzzle
            tile::curTile
            position::(
              getTileCenter (
                centerPoint puzzleSize::puzzleSize position::(toGameCoord prevTile.position)
              )
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
    let maybeToTilePoint puzzle::puzzle position::(GCoord {x, y}) => {
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
    let print_tile tile =>
      print_endline @@
      "bottom: " ^
      string_of_bool tile.bottom ^
      ", left: " ^
      string_of_bool tile.left ^
      ", top: " ^ string_of_bool tile.top ^ ", right: " ^ string_of_bool tile.right;
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

    /** Warning: cool iterative algorithm below **/
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
    let onMouseDown puzzle::puzzle gameState::gameState button::button state::state x::x y::y => {
      let mousePos = GCoord {x, y: windowSize - y};
      let puzzleSize = List.length puzzle.grid;
      switch button {
      | Gl.Events.LEFT_BUTTON =>
        if (state == Gl.Events.DOWN) {
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
    let onMouseMove puzzle::puzzle gameState::gameState x::x y::y => {
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
    let render puzzle::puzzle gameState::gameState time => {
      Gl.clear env.gl (Constants.color_buffer_bit lor Constants.depth_buffer_bit);
      myDrawRect
        width::windowSize height::windowSize color::Color.grey position::(GCoord {x: 0, y: 0});
      myDrawRect
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
            getTileCenter (
              centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.startTile)
            )
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
        /*myDrawCircle
          radius::(lineWeight / 2)
          color::Color.brown
          position::(
            getTileCenter (centerPoint puzzleSize::puzzleSize position::(toGameCoord puzzle.endTile))
          );*/
        myDrawCircle radius::(lineWeight / 2) color::Color.brightYellow position::lineEdge
      }
    };
    let gameState = {currentPath: [], lineEdge: None};
    let puzzle = examplePuzzle;
    Gl.Events.onMouseDown window (onMouseDown puzzle::puzzle gameState::gameState);
    Gl.Events.onMouseMove window (onMouseMove puzzle::puzzle gameState::gameState);
    Gl.displayFunc window::window cb::(render puzzle::puzzle gameState::gameState)
  };
};
