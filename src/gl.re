module type t = {
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
  };
  let module Events: EventsT;

  /** We're currently mimicking the JS asynchronous event handling allowing the user to register callbacks.
   * Instead of mutating global state in the Events module, we simply force the user to register all events
   * handlers at once, allowing us to use the closure to keep track of the data for us.
   * For native, the easiest way to handle events is in the render loop, so we force the user to also
   * register the draw call `displayFunc` which will effectively do all of the rendering.
   */
  let render:
    window::Window.t =>
    mouseDown::(button::Events.buttonStateT => state::Events.stateT => x::int => y::int => unit)? =>
    mouseMove::(x::int => y::int => unit)? =>
    displayFunc::(float => unit) =>
    unit =>
    unit;
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
  let createBuffer: context::contextT => bufferT;
  let bindBuffer: context::contextT => target::int => buffer::bufferT => unit;
  type float32Array = array float;
  type uint16Array = array int;
  type dataKind =
    | Float32 float32Array
    | UInt16 uint16Array;
  let bufferData: context::contextT => target::int => data::dataKind => usage::int => unit;
  let viewport: context::contextT => x::int => y::int => width::int => height::int => unit;
  let clear: context::contextT => mask::int => unit;
  /* TODO: We'll need to do something about this */
  let getUniformLocation: context::contextT => program::programT => name::string => uniformT;
  let getAttribLocation: context::contextT => program::programT => name::string => attributeT;
  let enableVertexAttribArray: context::contextT => attribute::attributeT => unit;
  let vertexAttribPointer:
    context::contextT =>
    attribute::attributeT =>
    size::int =>
    type_::int =>
    normalize::bool =>
    stride::int =>
    offset::int =>
    unit;
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
  let module Mat4: Mat4T;
  let uniformMatrix4fv: context::contextT => location::uniformT => value::Mat4.t => unit;
  type shaderParamsT =
    | Shader_delete_status
    | Compile_status
    | Shader_type;
  type programParamsT =
    | Program_delete_status
    | Link_status
    | Validate_status;
  let getProgramParameter:
    context::contextT => program::programT => paramName::programParamsT => int;
  let getShaderParameter: context::contextT => shader::shaderT => paramName::shaderParamsT => int;
  let getShaderInfoLog: context::contextT => shader::shaderT => string;
  let getProgramInfoLog: context::contextT => program::programT => string;
  let getShaderSource: context::contextT => shader::shaderT => string;
  let drawArrays: context::contextT => mode::int => first::int => count::int => unit;
  let drawElements:
    context::contextT => mode::int => count::int => type_::int => offset::int => unit;
};
