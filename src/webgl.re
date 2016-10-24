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
  type context;
  module type WindowT = {
    type t;
    let getWidth: t => int;
    let getHeight: t => int;
    let init: argv::array string => t;
    let setWindowSize: t => width::int => height::int => unit;
    let initDisplayMode: t => double_buffer::bool => unit => unit;
    let getContext: t => context;
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
    let setWindowSize (window: t) width::width height::height => {
      setWidth window width;
      setHeight window height
    };
    let initDisplayMode window double_buffer::_ () => ();
    let getContext (window: t) :context => getContext window "webgl";
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
      Window.t => (button::buttonStateT => state::stateT => x::int => y::int => unit) => unit;
    let onMouseMove: Window.t => (x::int => y::int => unit) => unit;
  };
  let module Events = {
    type buttonStateT =
      | LEFT_BUTTON
      | MIDDLE_BUTTON
      | RIGHT_BUTTON;
    type stateT =
      | DOWN
      | UP;
    let onMouseDown (window: Window.t) cb =>
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
    let onMouseMove (window: Window.t) cb =>
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
  let displayFunc cb => {
    let rec tick () => {
      cb (Document.now ());
      Document.requestAnimationFrame tick
    };
    Document.requestAnimationFrame tick
  };
  type program;
  type shader;
  /* HACK */
  type float32arrayOrUint16Array;
  external clearColor : context => float => float => float => float => unit = "clearColor" [@@bs.send];
  external createProgram : context => program = "createProgram" [@@bs.send];
  external createShader : context => int => shader = "createShader" [@@bs.send];
  external attachShader : context => program => shader => unit = "attachShader" [@@bs.send];
  external shaderSource : context => shader => string => unit = "shaderSource" [@@bs.send];
  external compileShader : context => shader => unit = "compileShader" [@@bs.send];
  external attachShader : context => program => shader::shader => unit = "attachShader" [@@bs.send];
  external linkProgram : context => program => unit = "linkProgram" [@@bs.send];
  external useProgram : context => program => unit = "useProgram" [@@bs.send];
  type buffer;
  type attribute;
  type uniform;
  external createBuffer : context => buffer = "createBuffer" [@@bs.send];
  external bindBuffer : context => int => buffer => unit = "bindBuffer" [@@bs.send];
  external bufferData : context => int => float32arrayOrUint16Array => int => unit = "bufferData" [@@bs.send];
  external viewport : context => int => int => int => int => unit = "viewport" [@@bs.send];
  external clear : context => int => unit = "clear" [@@bs.send];
  /* TODO: We'll need to do something about this */
  external createFloat32Array : array float => float32arrayOrUint16Array = "Float32Array" [@@bs.new];
  external createUint16Array : array int => float32arrayOrUint16Array = "Uint16Array" [@@bs.new];
  external getUniformLocation : context => program => string => uniform = "getUniformLocation" [@@bs.send];
  external getAttribLocation : context => program => string => attribute = "getAttribLocation" [@@bs.send];
  external enableVertexAttribArray : context => attribute => unit = "enableVertexAttribArray" [@@bs.send];
  external vertexAttribPointer : context =>
                                 attribute =>
                                 int =>
                                 int =>
                                 Js.boolean =>
                                 int =>
                                 int =>
                                 unit = "vertexAttribPointer" [@@bs.send];
  external uniformMatrix4fv : context => uniform => Js.boolean => 'mat4t => unit = "uniformMatrix4fv" [@@bs.send];
  /* Can return other value types as well, see https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Types */
  external getProgramParameter : context => program => int => Js.boolean = "getProgramParameter" [@@bs.send];
  external getShaderParameter : context => shader => int => Js.boolean = "getShaderParameter" [@@bs.send];
  external getShaderInfoLog : context => shader => string = "getShaderInfoLog" [@@bs.send];
  external getShaderSource : context => shader => string = "getShaderSource" [@@bs.send];
  external drawArrays : context => int => int => int => unit = "drawArrays" [@@bs.send];
  external drawElements : context => int => int => int => int => unit = "drawElements" [@@bs.send];
};
