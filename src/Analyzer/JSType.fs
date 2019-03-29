namespace Analyzer

exception LoadJSTypeException of string

type JSType =
  | PriBoolean
  | Null
  | Undef
  | PriNumber
  | PriString
  | PriSymbol
  | Object
  | Array
  | ArrayBuffer
  | Boolean
  | DataView
  | Date
  | Error
  | EvalError
  | Float32Array
  | Float64Array
  | Function
  | Class
  | Int16Array
  | Int32Array
  | Int8Array
  | IntlCollator
  | IntlDateTimeFormat
  | IntlNumberFormat
  | IntlPluralRules
  | Map
  | Number
  | Promise
  | Proxy
  | RangeError
  | ReferenceError
  | RegExp
  | Set
  | SharedArrayBuffer
  | String
  | Symbol
  | SyntaxError
  | TypeError
  | URIError
  | Uint16Array
  | Uint32Array
  | Uint8Array
  | Uint8ClampedArray
  | WeakMap
  | WeakSet
  (* Expreimental Types *)
  | WebAssemblyModule
  | WebAssemblyInstance
  | WebAssemblyMemory
  | WebAssemblyTable
  | WebAssemblyCompileError
  | WebAssemblyLinkError
  | WebAssemblyRuntimeError
  | Worker
  | BigUint64Array
  | BigInt64Array
  | BigInt
  | Label

module JSType =
  let ofStr = function
    | "boolean" -> PriBoolean
    | "null" -> Null
    | "undefined" -> Undef
    | "number" -> PriNumber
    | "string" -> PriString
    | "symbol" -> PriSymbol
    | "function" -> Function
    | "object" -> Object
    | "bigint" -> BigInt
    | "Array" -> Array
    | "ArrayBuffer" -> ArrayBuffer
    | "Boolean" -> Boolean
    | "BigUint64Array" -> BigUint64Array
    | "BigInt64Array" -> BigInt64Array
    | "DataView" -> DataView
    | "Date" -> Date
    | "Error" -> Error
    | "EvalError" -> EvalError
    | "Float32Array" -> Float32Array
    | "Float64Array" -> Float64Array
    | "Function" -> Function
    | "Int16Array" -> Int16Array
    | "Int32Array" -> Int32Array
    | "Int8Array" -> Int8Array
    | "IntlCollator" -> IntlCollator
    | "Intl.Collator" -> IntlCollator
    | "Intl.DateTimeFormat" -> IntlDateTimeFormat
    | "Intl.NumberFormat" -> IntlNumberFormat
    | "Map" -> Map
    | "Number" -> Number
    | "Promise" -> Promise
    | "Proxy" -> Proxy
    | "RangeError" -> RangeError
    | "ReferenceError" -> ReferenceError
    | "RegExp" -> RegExp
    | "Set" -> Set
    | "SharedArrayBuffer" -> SharedArrayBuffer
    | "String" -> String
    | "Symbol" -> Symbol
    | "SyntaxError" -> SyntaxError
    | "TypeError" -> TypeError
    | "URIError" -> URIError
    | "Uint16Array" -> Uint16Array
    | "Uint32Array" -> Uint32Array
    | "Uint8Array" -> Uint8Array
    | "Uint8ClampedArray" -> Uint8ClampedArray
    | "WeakMap" -> WeakMap
    | "WeakSet" -> WeakSet
    (* Expreimental Types *)
    | "WebAssembly.Module" -> WebAssemblyModule
    | "WebAssembly.Instance" -> WebAssemblyInstance
    | "WebAssembly.Memory" -> WebAssemblyMemory
    | "WebAssembly.Table" -> WebAssemblyTable
    | "WebAssembly.CompileError" -> WebAssemblyCompileError
    | "WebAssembly.LinkError" -> WebAssemblyLinkError
    | "WebAssembly.RuntimeError" -> WebAssemblyRuntimeError
    | "Class" -> Class
    | "Worker" -> Worker
    | e -> LoadJSTypeException e |> raise

  let isUndef = function
    | Undef -> true
    | _ -> false
