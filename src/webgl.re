let module Document = {
  type element;
  type window;
  let window: window = [%bs.raw "window"];
  /* external setGlDebug : window => GlT.context => unit = "debugContext" [@@bs.set]; */
  external getElementById : string => element = "document.getElementById" [@@bs.val];
  external getContext : element => string => 'context = "getContext" [@@bs.send];
  external getWidth : element => int = "width" [@@bs.get];
  external getHeight : element => int = "height" [@@bs.get];
  external requestAnimationFrame : (unit => unit) => unit = "window.requestAnimationFrame" [@@bs.val];
  external now : unit => float = "Date.now" [@@bs.val];
  external addEventListener : 'window => string => ('eventT => unit) => unit = "addEventListener" [@@bs.send];
};

external getButton : 'eventT => int = "button" [@@bs.get];

external getClientX : 'eventT => int = "clientX" [@@bs.get];

external getClientY : 'eventT => int = "clientY" [@@bs.get];

external getWidth : 'canvas => int = "width" [@@bs.get];

external getHeight : 'canvas => int = "height" [@@bs.get];

external setWidth : 'canvas => int => unit = "width" [@@bs.set];

external setHeight : 'canvas => int => unit = "height" [@@bs.set];

external createCanvas : string => 'canvas = "document.createElement" [@@bs.val];

let createCanvas () => createCanvas "canvas";

external addToBody : 'canvas => unit = "document.body.appendChild" [@@bs.val];

external getContext : 'canvas => string => 'context = "getContext" [@@bs.send];

let module Gl = {
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
  let module Window = {
    type t;
    let getWidth = getWidth;
    let getHeight = getHeight;
    let init argv::_ => {
      let canvas: t = createCanvas ();
      addToBody canvas;
      canvas
    };
    let setWindowSize window::(window: t) width::width height::height => {
      setWidth window width;
      setHeight window height
    };
    let initDisplayMode window::window double_buffer::_ () => ();
    let getContext (window: t) :contextT => getContext window "webgl";
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
    let onMouseDown window::(window: Window.t) cb =>
      Document.addEventListener
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
        );
    let onMouseMove window::(window: Window.t) cb =>
      Document.addEventListener
        window
        "mousemove"
        (
          fun e => {
            let x = getClientX e;
            let y = getClientY e;
            cb x::x y::y
          }
        );
  };
  let displayFunc window::window cb::cb => {
    let rec tick () => {
      cb (Document.now ());
      Document.requestAnimationFrame tick
    };
    Document.requestAnimationFrame tick
  };
  type programT;
  type shaderT;
  external clearColor : context::contextT => r::float => g::float => b::float => a::float => unit = "clearColor" [@@bs.send];
  external createProgram : context::contextT => programT = "createProgram" [@@bs.send];
  external createShader : context::contextT => shaderType::int => shaderT = "createShader" [@@bs.send];
  external shaderSource : context::contextT => shader::shaderT => source::string => unit = "shaderSource" [@@bs.send];
  external compileShader : context::contextT => shader::shaderT => unit = "compileShader" [@@bs.send];
  external attachShader : context::contextT => program::programT => shader::shaderT => unit = "attachShader" [@@bs.send];
  external linkProgram : context::contextT => program::programT => unit = "linkProgram" [@@bs.send];
  external useProgram : context::contextT => program::programT => unit = "useProgram" [@@bs.send];
  type bufferT;
  type attributeT;
  type uniformT;
  type boolT = Js.boolean;
  let false_: boolT = Js.false_;
  let true_: boolT = Js.true_;
  external createBuffer : context::contextT => bufferT = "createBuffer" [@@bs.send];
  external bindBuffer : context::contextT => target::int => buffer::bufferT => unit = "bindBuffer" [@@bs.send];
  /* HACK */
  type float32Array = array float;
  type uint16Array = array int;
  type dataKind =
    | Float32 float32Array
    | UInt16 uint16Array;
  external bufferData : context::contextT => target::int => data::dataKind => usage::int => unit = "bufferData" [@@bs.send];
  external viewport : context::contextT => x::int => y::int => width::int => height::int => unit = "viewport" [@@bs.send];
  external clear : context::contextT => mask::int => unit = "clear" [@@bs.send];
  /* TODO: We'll need to do something about this */
  external createFloat32Array : array float => float32Array = "Float32Array" [@@bs.new];
  external createUint16Array : array int => uint16Array = "Uint16Array" [@@bs.new];
  external getUniformLocation : context::contextT => program::programT => name::string => uniformT = "getUniformLocation" [@@bs.send];
  external getAttribLocation : context::contextT => program::programT => name::string => attributeT = "getAttribLocation" [@@bs.send];
  external enableVertexAttribArray : context::contextT => attribute::attributeT => unit = "enableVertexAttribArray" [@@bs.send];
  external vertexAttribPointer : context::contextT =>
                                 attribute::attributeT =>
                                 size::int =>
                                 type_::int =>
                                 normalize::boolT =>
                                 stride::int =>
                                 offset::int =>
                                 unit = "vertexAttribPointer" [@@bs.send];
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
    external create : unit => t = "mat4.create" [@@bs.val];
    /* external perspective : out::t => fovy::int => aspect::float => near::float => far::float => unit = "mat4.perspective" [@@bs.val]; */
    external identity : out::t => unit = "mat4.identity" [@@bs.val];
    external translate : out::t => matrix::t => vec::array float => unit = "mat4.translate" [@@bs.val];
    external scale : out::t => matrix::t => vec::array float => unit = "mat4.scale" [@@bs.val];
    external rotate : out::t => matrix::t => rad::float => vec::array float => unit = "mat4.rotate" [@@bs.val];
    external ortho : out::t =>
                     left::float =>
                     right::float =>
                     bottom::float =>
                     top::float =>
                     near::float =>
                     far::float =>
                     unit = "mat4.ortho" [@@bs.val];
  };
  external uniformMatrix4fv : context::contextT =>
                              location::uniformT =>
                              transpose::boolT =>
                              value::Mat4.t =>
                              unit = "uniformMatrix4fv" [@@bs.send];
  /* Can return other value types as well, see https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Types */
  external getProgramParameter : context::contextT => program::programT => paramName::int => int = "getProgramParameter" [@@bs.send];
  external getShaderParameter : context::contextT => shader::shaderT => paramName::int => int = "getShaderParameter" [@@bs.send];
  external compileStatus : context::contextT => int = "COMPILE_STATUS" [@@bs.send];
  let getCompileStatus context::(context: contextT) shader::shader =>
    getShaderParameter context::context shader::shader paramName::(compileStatus context::context) == 1;
  external getShaderInfoLog : context::contextT =>
                              shader::shaderT =>
                              maxLength::int [@bs.ignore] =>
                              string = "getShaderInfoLog" [@@bs.send];
  external getShaderSource : context::contextT => shader::shaderT => string = "getShaderSource" [@@bs.send];
  external drawArrays : context::contextT => mode::int => first::int => count::int => unit = "drawArrays" [@@bs.send];
  external drawElements : context::contextT =>
                          mode::int =>
                          count::int =>
                          type_::int =>
                          offset::int =>
                          unit = "drawElements" [@@bs.send];
};
