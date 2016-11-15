/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
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
  let target = "web";
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
  };
  let module Events = {
    type buttonStateT =
      | LEFT_BUTTON
      | MIDDLE_BUTTON
      | RIGHT_BUTTON;
    type stateT =
      | DOWN
      | UP;
  };
  type mouseDownT =
    button::Events.buttonStateT => state::Events.stateT => x::int => y::int => unit;

  /** See Gl.re for explanation. **/
  let render
      window::(window: Window.t)
      mouseDown::(mouseDown: option mouseDownT)=?
      mouseMove::(mouseMove: option (x::int => y::int => unit))=?
      displayFunc::(displayFunc: float => unit)
      () => {
    switch mouseDown {
    | None => ()
    | Some cb =>
      Document.addEventListener
        window
        "mousedown"
        (
          fun e => {
            let button =
              switch (getButton e) {
              | 0 => Events.LEFT_BUTTON
              | 1 => Events.MIDDLE_BUTTON
              | 2 => Events.RIGHT_BUTTON
              | _ => assert false
              };
            let state = Events.DOWN;
            let x = getClientX e;
            let y = getClientY e;
            cb button::button state::state x::x y::y
          }
        )
    };
    switch mouseMove {
    | None => ()
    | Some cb =>
      Document.addEventListener
        window
        "mousemove"
        (
          fun e => {
            let x = getClientX e;
            let y = getClientY e;
            cb x::x y::y
          }
        )
    };
    let rec tick () => {
      displayFunc (Document.now ());
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
  external deleteShader : context::contextT => shader::shaderT => unit = "deleteShader" [@@bs.send];
  external linkProgram : context::contextT => program::programT => unit = "linkProgram" [@@bs.send];
  external useProgram : context::contextT => program::programT => unit = "useProgram" [@@bs.send];
  type bufferT;
  type attributeT;
  type uniformT;
  external createBuffer : context::contextT => bufferT = "createBuffer" [@@bs.send];
  external bindBuffer : context::contextT => target::int => buffer::bufferT => unit = "bindBuffer" [@@bs.send];

  /** Those types are what allows to come close to some form of ad-hoc polymorphism
   *  See the Bucklescript manual:
   *  https://bloomberg.github.io/bucklescript/Manual.html#_phantom_arguments_and_ad_hoc_polyrmophism
   */
  type float32Array = array float;
  type uint16Array = array int;
  type dataKind =
    | Float32 float32Array
    | UInt16 uint16Array;

  /** Those externals are used for bufferData to instantiate what gl.bufferData actually expects, because JS
   *  doesn't differentiate between float and int but the GL backend needs to know the types precisely.
   **/
  external createFloat32Array : array float => 'float32Array = "Float32Array" [@@bs.new];
  external createUint16Array : array int => 'uint16Array = "Uint16Array" [@@bs.new];
  external _bufferData : context::contextT => target::int => data::array 'a => usage::int => unit = "bufferData" [@@bs.send];
  let bufferData context::context target::target data::data usage::usage =>
    switch data {
    | Float32 x =>
      _bufferData context::context target::target data::(createFloat32Array x) usage::usage
    | UInt16 x =>
      _bufferData context::context target::target data::(createUint16Array x) usage::usage
    };
  external viewport : context::contextT => x::int => y::int => width::int => height::int => unit = "viewport" [@@bs.send];
  external clear : context::contextT => mask::int => unit = "clear" [@@bs.send];
  external getUniformLocation : context::contextT => program::programT => name::string => uniformT = "getUniformLocation" [@@bs.send];
  external getAttribLocation : context::contextT => program::programT => name::string => attributeT = "getAttribLocation" [@@bs.send];
  external enableVertexAttribArray : context::contextT => attribute::attributeT => unit = "enableVertexAttribArray" [@@bs.send];
  external _vertexAttribPointer : context::contextT =>
                                  attribute::attributeT =>
                                  size::int =>
                                  type_::int =>
                                  normalize::Js.boolean =>
                                  stride::int =>
                                  offset::int =>
                                  unit = "vertexAttribPointer" [@@bs.send];
  let vertexAttribPointer
      context::context
      attribute::attribute
      size::size
      type_::type_
      normalize::normalize
      stride::stride
      offset::offset => {
    let normalize = if normalize {Js.true_} else {Js.false_};
    _vertexAttribPointer
      context::context
      attribute::attribute
      size::size
      type_::type_
      normalize::normalize
      stride::stride
      offset::offset
  };
  module type Mat4T = {
    type t;
    let to_array: t => array float;
    let create: unit => t;
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
  external _uniformMatrix4fv : context::contextT =>
                               location::uniformT =>
                               transpose::Js.boolean =>
                               value::Mat4.t =>
                               unit = "uniformMatrix4fv" [@@bs.send];
  let uniformMatrix4fv context::context location::location value::value =>
    _uniformMatrix4fv context::context location::location transpose::Js.false_ value::value;
  /* Can return other value types as well, see https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Types */
  type shaderParamsInternalT 'a =
    | Shader_delete_status_internal :shaderParamsInternalT bool
    | Compile_status_internal :shaderParamsInternalT bool
    | Shader_type_internal :shaderParamsInternalT int;
  type programParamsInternalT 'a =
    | Program_delete_status_internal :programParamsInternalT bool
    | Link_status_internal :programParamsInternalT bool
    | Validate_status_internal :programParamsInternalT bool;
  /* | Attached_shaders_internal :programParamsInternalT int
     | Active_attributes_internal :programParamsInternalT int
     | Active_uniforms_internal :programParamsInternalT int */
  type shaderParamsT =
    | Shader_delete_status
    | Compile_status
    | Shader_type;
  type programParamsT =
    | Program_delete_status
    | Link_status
    | Validate_status;
  /* | Attached_shaders
     | Active_attributes
     | Active_uniforms */
  external deleteStatus : context::contextT => int = "DELETE_STATUS" [@@bs.get];
  external compileStatus : context::contextT => int = "COMPILE_STATUS" [@@bs.get];
  external linkStatus : context::contextT => int = "LINK_STATUS" [@@bs.get];
  external validateStatus : context::contextT => int = "VALIDATE_STATUS" [@@bs.get];
  external shaderType : context::contextT => int = "SHADER_TYPE" [@@bs.get];
  external _getProgramParameter : context::contextT =>
                                  program::programT =>
                                  paramName::int =>
                                  (programParamsInternalT 'a) [@bs.ignore] =>
                                  'a = "getProgramParameter" [@@bs.send];
  let getProgramParameter context::context program::program paramName::paramName =>
    switch paramName {
    | Program_delete_status =>
      if (
        _getProgramParameter
          context::context
          program::program
          paramName::(deleteStatus context::context)
          Program_delete_status_internal
      ) {
        1
      } else {
        0
      }
    | Link_status =>
      if (
        _getProgramParameter
          context::context
          program::program
          paramName::(linkStatus context::context)
          Link_status_internal
      ) {
        1
      } else {
        0
      }
    | Validate_status =>
      if (
        _getProgramParameter
          context::context
          program::program
          paramName::(validateStatus context::context)
          Validate_status_internal
      ) {
        1
      } else {
        0
      }
    };
  external _getShaderParameter : context::contextT =>
                                 shader::shaderT =>
                                 paramName::int =>
                                 (shaderParamsInternalT 'a) [@bs.ignore] =>
                                 'a = "getShaderParameter" [@@bs.send];
  let getShaderParameter context::context shader::shader paramName::paramName =>
    switch paramName {
    | Shader_delete_status =>
      if (
        _getShaderParameter
          context::context
          shader::shader
          paramName::(deleteStatus context::context)
          Shader_delete_status_internal
      ) {
        1
      } else {
        0
      }
    | Compile_status =>
      if (
        _getShaderParameter
          context::context
          shader::shader
          paramName::(compileStatus context::context)
          Compile_status_internal
      ) {
        1
      } else {
        0
      }
    | Shader_type =>
      _getShaderParameter
        context::context
        shader::shader
        paramName::(shaderType context::context)
        Shader_type_internal
    };
  external getShaderInfoLog : context::contextT => shader::shaderT => string = "getShaderInfoLog" [@@bs.send];
  external getProgramInfoLog : context::contextT => program::programT => string = "getProgramInfoLog" [@@bs.send];
  external getShaderSource : context::contextT => shader::shaderT => string = "getShaderSource" [@@bs.send];
  external drawArrays : context::contextT => mode::int => first::int => count::int => unit = "drawArrays" [@@bs.send];
  external drawElements : context::contextT =>
                          mode::int =>
                          count::int =>
                          type_::int =>
                          offset::int =>
                          unit = "drawElements" [@@bs.send];
};
