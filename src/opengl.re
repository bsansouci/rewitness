open Tsdl;

open Tgles2;

open Result;

let (>>=) x f =>
  switch x {
  | Ok v => f v
  | Error _ as e => e
  };

let create_window gl::(maj, min) => {
  let w_atts = Sdl.Window.(opengl + resizable);
  let w_title = Printf.sprintf "OpenGL %d.%d (core profile)" maj min;
  let set a v => Sdl.gl_set_attribute a v;
  set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core >>= (
    fun () =>
      set Sdl.Gl.context_major_version maj >>= (
        fun () =>
          set Sdl.Gl.context_minor_version min >>= (
            fun () =>
              set Sdl.Gl.doublebuffer 1 >>= (
                fun () => Sdl.create_window w::640 h::480 w_title w_atts >>= (fun win => Ok win)
              )
          )
      )
  )
};

let module Gl = {
  type contextT = Sdl.gl_context;
  module type WindowT = {
    type t;
    let getWidth: t => int;
    let getHeight: t => int;
    let init: argv::array string => t;
    let setWindowSize: window::t => width::int => height::int => unit;
    let initDisplayMode: window::t => double_buffer::bool => unit => unit;
    let getContext: t => contextT;
  };
  let module Window = {
    type t = Sdl.window;
    let getWidth (window: t) => {
      let (width, _) = Sdl.get_window_size window;
      width
    };
    let getHeight (window: t) => {
      let (_, height) = Sdl.get_window_size window;
      height
    };
    let init argv::_ =>
      switch (
        Sdl.init Sdl.Init.video >>= (fun () => create_window gl::(2, 0) >>= (fun win => Ok win))
      ) {
      | Ok win => win
      | Error e => assert false
      };
    let setWindowSize window::(window: t) width::width height::height =>
      Sdl.set_window_size window width height;
    let initDisplayMode window::window double_buffer::_ () => ();
    let getContext (window: t) :contextT =>
      switch (
        Sdl.gl_create_context window >>= (
          fun ctx => Sdl.gl_make_current window ctx >>= (fun () => Ok ctx)
        )
      ) {
      | Ok ctx => ctx
      | Error e => assert false
      };
  };
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
  let module Events = {
    type buttonStateT =
      | LEFT_BUTTON
      | MIDDLE_BUTTON
      | RIGHT_BUTTON;
    type stateT =
      | DOWN
      | UP;
    let onMouseDown window::(window: Window.t) cb => ();
    /* Document.addEventListener
       window
       "mousedown"
       (
         fun e => {
           let button =
             switch (getButton e) {
             | 0 => LEFT_BUTTON
             | 1 => MIDDLE_BUTTON
             | 2 => RIGHT_BUTTON
             | _ => assert false
             };
           let state = DOWN;
           let x = getClientX e;
           let y = getClientY e;
           cb button::button state::state x::x y::y
         }
       ); */
    let onMouseMove window::(window: Window.t) cb => ();
    /* Document.addEventListener
       window
       "mousemove"
       (
         fun e => {
           let x = getClientX e;
           let y = getClientY e;
           cb x::x y::y
         }
       ); */
  };
  let displayFunc window::(window: Window.t) cb::cb => {
    let prevTime = ref Int64.zero;
    let rec tick () => {
      let time = Sdl.get_performance_counter ();
      let diff =
        Int64.div
          (Int64.mul (Int64.sub time !prevTime) (Int64.of_float 1000.))
          (Sdl.get_performance_frequency ());
      if (Int64.compare diff (Int64.of_float 16.666) == 1) {
        cb (Int64.to_float time);
        Sdl.gl_swap_window window
      };
      prevTime := time;
      tick ()
    };
    tick ()
  };
  type programT = int;
  type shaderT = int;
  /* HACK */
  /* Context needs to be the current context */
  let clearColor context::context r::r g::g b::b a::a => Gl.clear_color r g b a;
  let createProgram context::(context: contextT) :programT => Gl.create_program ();
  let createShader context::(context: contextT) shaderType::shaderType :shaderT => Gl.create_shader shaderType;
  let attachShader context::context program::program shader::shader =>
    Gl.attach_shader program shader;
  let shaderSource context::context shader::shader source::source =>
    Gl.shader_source shader source;
  let compileShader context::context shader::shader => Gl.compile_shader shader;
  let linkProgram context::context program::program => Gl.link_program program;
  let useProgram context::context program::program => Gl.use_program program;
  type bufferT = Gl.enum;
  /* Sigh, Tgl3 uses a lot of ints everywhere... */
  type attributeT = int;
  type uniformT = int;
  type boolT = bool;
  let false_ = false;
  let true_ = true;
  type float32Array = Bigarray.Array1.t float Bigarray.float32_elt Bigarray.c_layout;
  type uint16Array = Bigarray.Array1.t int Bigarray.int16_unsigned_elt Bigarray.c_layout;
  type dataKind =
    | Float32 float32Array
    | UInt16 uint16Array;
  let createBuffer = {
    let a = Bigarray.Array1.create Bigarray.Int32 Bigarray.C_layout 1;
    fun context::(context: contextT) => {
      Gl.gen_buffers 1 a;
      Int32.to_int a.{0}
    }
  };
  let bindBuffer context::(context: contextT) target::target buffer::buffer =>
    Gl.bind_buffer target buffer;
  let bufferData context::(context: contextT) target::target data::(data: dataKind) usage::usage =>
    switch data {
    | Float32 d => Gl.buffer_data target (Gl.bigarray_byte_size d) (Some d) usage
    | UInt16 d => Gl.buffer_data target (Gl.bigarray_byte_size d) (Some d) usage
    };
  let viewport context::(context: contextT) x::x y::y width::width height::height =>
    Gl.viewport x y width height;
  let clear context::(context: contextT) mask::mask => Gl.clear mask;
  /* TODO: Figure this out...
     Not sure if this is possible

     In module Gl:
             Values do not match:
               val createFloat32Array :
                 float array ->
                 (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
             is not included in
               val createFloat32Array :
                 float array -> (float, 'b) float32arrayOrUint16ArrayT


     */
  let createFloat32Array data => Bigarray.Array1.of_array Bigarray.Float32 Bigarray.c_layout data;
  let createUint16Array data =>
    Bigarray.Array1.of_array Bigarray.Int16_unsigned Bigarray.c_layout data;
  let getUniformLocation
      context::(context: contextT)
      program::(program: programT)
      name::name
      :uniformT =>
    Gl.get_uniform_location program name;
  let getAttribLocation
      context::(context: contextT)
      program::(program: programT)
      name::name
      :attributeT =>
    Gl.get_attrib_location program name;
  let enableVertexAttribArray context::(context: contextT) attribute::attribute => Gl.enable_vertex_attrib_array attribute;
  let vertexAttribPointer
      context::(context: contextT)
      attribute::attribute
      size::size
      type_::type_
      normalize::normalize
      stride::stride
      offset::offset =>
    /* For now `offset` is only going to be an offset (limited by the webgl API?). */
    Gl.vertex_attrib_pointer attribute size type_ normalize stride (`Offset offset);
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
  let module Mat4: Mat4T = {
    type t = array float;
    let to_array a => a;
    let epsilon = 0.00001;
    let create () => [|
      1.0,
      0.0,
      0.0,
      0.0,
      0.0,
      1.0,
      0.0,
      0.0,
      0.0,
      0.0,
      1.0,
      0.0,
      0.0,
      0.0,
      0.0,
      1.0
    |];
    /* let perspective out::(out: t) fovy::int aspect::float near::float far::float => (); */
    let identity out::(out: t) => {
      out.(0) = 1.0;
      out.(1) = 0.0;
      out.(2) = 0.0;
      out.(3) = 0.0;
      out.(4) = 0.0;
      out.(5) = 1.0;
      out.(6) = 0.0;
      out.(7) = 0.0;
      out.(8) = 0.0;
      out.(9) = 0.0;
      out.(10) = 1.0;
      out.(11) = 0.0;
      out.(12) = 0.0;
      out.(13) = 0.0;
      out.(14) = 0.0;
      out.(15) = 1.0
    };
    let translate out::(out: t) matrix::(matrix: t) vec::(vec: array float) => {
      let x = vec.(0);
      let y = vec.(1);
      let z = vec.(2);
      if (matrix === out) {
        out.(12) = matrix.(0) *. x +. matrix.(4) *. y +. matrix.(8) *. z +. matrix.(12);
        out.(13) = matrix.(1) *. x +. matrix.(5) *. y +. matrix.(9) *. z +. matrix.(13);
        out.(14) = matrix.(2) *. x +. matrix.(6) *. y +. matrix.(10) *. z +. matrix.(14);
        out.(15) = matrix.(3) *. x +. matrix.(7) *. y +. matrix.(11) *. z +. matrix.(15)
      } else {
        let a00 = matrix.(0);
        let a01 = matrix.(1);
        let a02 = matrix.(2);
        let a03 = matrix.(3);
        let a10 = matrix.(4);
        let a11 = matrix.(5);
        let a12 = matrix.(6);
        let a13 = matrix.(7);
        let a20 = matrix.(8);
        let a21 = matrix.(9);
        let a22 = matrix.(10);
        let a23 = matrix.(11);
        out.(0) = a00;
        out.(1) = a01;
        out.(2) = a02;
        out.(3) = a03;
        out.(4) = a10;
        out.(5) = a11;
        out.(6) = a12;
        out.(7) = a13;
        out.(8) = a20;
        out.(9) = a21;
        out.(10) = a22;
        out.(11) = a23;
        out.(12) = a00 *. x +. a10 *. y +. a20 *. z +. matrix.(12);
        out.(13) = a01 *. x +. a11 *. y +. a21 *. z +. matrix.(13);
        out.(14) = a02 *. x +. a12 *. y +. a22 *. z +. matrix.(14);
        out.(15) = a03 *. x +. a13 *. y +. a23 *. z +. matrix.(15)
      }
    };
    let scale out::(out: t) matrix::(matrix: t) vec::(vec: array float) => {
      let x = vec.(0);
      let y = vec.(1);
      let z = vec.(2);
      out.(0) = matrix.(0) *. x;
      out.(1) = matrix.(1) *. x;
      out.(2) = matrix.(2) *. x;
      out.(3) = matrix.(3) *. x;
      out.(4) = matrix.(4) *. y;
      out.(5) = matrix.(5) *. y;
      out.(6) = matrix.(6) *. y;
      out.(7) = matrix.(7) *. y;
      out.(8) = matrix.(8) *. z;
      out.(9) = matrix.(9) *. z;
      out.(10) = matrix.(10) *. z;
      out.(11) = matrix.(11) *. z;
      out.(12) = matrix.(12);
      out.(13) = matrix.(13);
      out.(14) = matrix.(14);
      out.(15) = matrix.(15)
    };
    let rotate out::(out: t) matrix::(matrix: t) rad::(rad: float) vec::(vec: array float) => {
      let x = matrix.(0);
      let y = matrix.(1);
      let z = matrix.(2);
      let len = sqrt (x *. x +. y *. y +. z *. z);
      if (abs_float len > epsilon) {
        let len = 1. /. sqrt (x *. x +. y *. y +. z *. z);
        let x = matrix.(0) *. len;
        let y = matrix.(1) *. len;
        let z = matrix.(2) *. len;
        let s = sin rad;
        let c = cos rad;
        let t = 1. -. c;
        let a00 = matrix.(0);
        let a01 = matrix.(1);
        let a02 = matrix.(2);
        let a03 = matrix.(3);
        let a10 = matrix.(4);
        let a11 = matrix.(5);
        let a12 = matrix.(6);
        let a13 = matrix.(7);
        let a20 = matrix.(8);
        let a21 = matrix.(9);
        let a22 = matrix.(10);
        let a23 = matrix.(11);
        let b00 = x *. x *. t +. c;
        let b01 = y *. x *. t +. z *. s;
        let b02 = z *. x *. t -. y *. s;
        let b10 = x *. y *. t -. y *. s;
        let b11 = y *. y *. t -. c;
        let b12 = z *. y *. t +. x *. s;
        let b20 = x *. z *. t +. y *. s;
        let b21 = y *. z *. t -. x *. s;
        let b22 = z *. z *. t +. c;
        matrix.(0) = a00 *. b00 +. a10 *. b01 +. a20 *. b02;
        matrix.(1) = a01 *. b00 +. a11 *. b01 +. a21 *. b02;
        matrix.(2) = a02 *. b00 +. a12 *. b01 +. a22 *. b02;
        matrix.(3) = a03 *. b00 +. a13 *. b01 +. a23 *. b02;
        matrix.(4) = a00 *. b10 +. a10 *. b11 +. a20 *. b12;
        matrix.(5) = a01 *. b10 +. a11 *. b11 +. a21 *. b12;
        matrix.(6) = a02 *. b10 +. a12 *. b11 +. a22 *. b12;
        matrix.(7) = a03 *. b10 +. a13 *. b11 +. a23 *. b12;
        matrix.(8) = a00 *. b20 +. a10 *. b21 +. a20 *. b22;
        matrix.(9) = a01 *. b20 +. a11 *. b21 +. a21 *. b22;
        matrix.(10) = a02 *. b20 +. a12 *. b21 +. a22 *. b22;
        matrix.(11) = a03 *. b20 +. a13 *. b21 +. a23 *. b22
      };
      if (matrix !== out) {
        out.(12) = matrix.(12);
        out.(13) = matrix.(13);
        out.(14) = matrix.(14);
        out.(15) = matrix.(15)
      }
    };
    let ortho
        out::(out: t)
        left::(left: float)
        right::(right: float)
        bottom::(bottom: float)
        top::(top: float)
        near::(near: float)
        far::(far: float) => {
      let lr = 1. /. (left -. right);
      let bt = 1. /. (bottom -. top);
      let nf = 1. /. (near -. far);
      out.(0) = (-1.) *. lr;
      out.(1) = 0.;
      out.(2) = 0.;
      out.(3) = 0.;
      out.(4) = 0.;
      out.(5) = (-2.) *. bt;
      out.(6) = 0.;
      out.(7) = 0.;
      out.(8) = 0.;
      out.(9) = 0.;
      out.(10) = 2. *. nf;
      out.(11) = 0.;
      out.(12) = (left +. right) *. lr;
      out.(13) = (top +. bottom) *. bt;
      out.(14) = (far +. near) *. nf;
      out.(15) = 1.
    };
  };
  /* count = 1 for now https://www.opengl.org/sdk/docs/man/html/glUniform.xhtml */
  let uniformMatrix4fv
      context::(context: contextT)
      location::location
      transpose::transpose
      value::value => {
    let arr =
      Bigarray.Array1.create
        Bigarray.Float32 Bigarray.C_layout (Array.length (Mat4.to_array value));
    Gl.uniform_matrix4fv location 1 transpose arr
  };
  let getProgramParameter = {
    let a = Bigarray.Array1.create Bigarray.Int32 Bigarray.C_layout 1;
    fun context::(context: contextT) program::(program: programT) paramName::paramName => {
      Gl.get_programiv program paramName a;
      Int32.to_int a.{0}
    }
  };
  let getShaderParameter = {
    let a = Bigarray.Array1.create Bigarray.Int32 Bigarray.C_layout 1;
    fun context::(context: contextT) shader::shader paramName::(paramName: int) => {
      Gl.get_shaderiv shader paramName a;
      Int32.to_int a.{0}
    }
  };
  let getCompileStatus context::(context: contextT) shader::shader =>
    getShaderParameter context::context shader::shader paramName::Gl.compile_status == Gl.true_;
  let getShaderInfoLog context::(context: contextT) shader::shader maxLength::maxLength => {
    let len = getShaderParameter context::context shader::shader paramName::Gl.info_log_length;
    let logData = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout len;
    Gl.get_shader_info_log shader len None logData;
    Gl.string_of_bigarray logData
  };
  let getShaderSource context::(context: contextT) shader::(shader: shaderT) => "";
  let drawArrays context::(context: contextT) mode::mode first::first count::count =>
    Gl.draw_arrays mode first count;
  let drawElements context::(context: contextT) mode::mode count::count type_::type_ offset::offset =>
    Gl.draw_elements mode count type_ (`Offset offset);
};
