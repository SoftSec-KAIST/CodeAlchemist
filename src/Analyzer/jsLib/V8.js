var codealchemist_types = {
  "Function" : Function,
  "Boolean" : Boolean,
  "Symbol" : Symbol,
  "Error" : Error,
  "EvalError" : EvalError,
  "RangeError" : RangeError,
  "ReferenceError" : ReferenceError,
  "SyntaxError" : SyntaxError,
  "TypeError" : TypeError,
  "URIError" : URIError,
  "Number" : Number,
  "Date" : Date,
  "String" : String,
  "RegExp" : RegExp,
  "Array" : Array,
  "Int8Array" : Int8Array,
  "Uint8Array" : Uint8Array,
  "Uint8ClampedArray" : Uint8ClampedArray,
  "Int16Array" : Int16Array,
  "Uint16Array" : Uint16Array,
  "Int32Array" : Int32Array,
  "Uint32Array" : Uint32Array,
  "Float32Array" : Float32Array,
  "Float64Array" : Float64Array,
  "BigUint64Array" : BigUint64Array,
  "BigInt64Array" : BigInt64Array,
  "Map" : Map,
  "Set" : Set,
  "WeakMap" : WeakMap,
  "WeakSet" : WeakSet,
  "ArrayBuffer" : ArrayBuffer,
  "SharedArrayBuffer" : SharedArrayBuffer,
  "DataView" : DataView,
  "Promise" : Promise,
  "Intl.Collator" : Intl.Collator,
  "Intl.DateTimeFormat" : Intl.DateTimeFormat,
  "Intl.NumberFormat" : Intl.NumberFormat, 
  "WebAssembly.Module" : WebAssembly.Module,
  "WebAssembly.Instance" : WebAssembly.Instance,
  "WebAssembly.Memory" : WebAssembly.Memory,
  "WebAssembly.Table" : WebAssembly.Table,
  "WebAssembly.CompileError" : WebAssembly.CompileError,
  "WebAssembly.LinkError" : WebAssembly.LinkError,
  "WebAssembly.RuntimeError" : WebAssembly.RuntimeError,
  "Worker" : Worker
};

var codealchemist_logs;
if (typeof codealchemist_logs === "undefined") codealchemist_logs = {};

var codealchemist_tmps;
if (typeof codealchemist_tmps === "undefined") codealchemist_tmps = {};

function codealchemist_get_type (target) {
  let ty = typeof target;
  switch (ty){
    case "object":
      if (target === null) return "null";
      for (let name in codealchemist_types){
        try {
          if(target instanceof codealchemist_types[name]) return name;
        } catch (e) {}
      }
      return ty;
    case "function":
      if (target.toString().startsWith("class")) return "Class";
      return "function";
    default:
      return ty;
  }
}

function codealchemist_get_types (vars){
  let ret = {};
  for (let name in vars){
    if (name[0] === 'v') {
      let ty = codealchemist_get_type (vars[name]);
      ret[name] = ty;
    }
  }
  return JSON.stringify (ret);
}

function codealchemist_print (hval, pre, post) {
  let print = console.log
  let s1 = "{ \"hval\": " + hval + ",";
  let s2 = "  \"pre\": " + pre + ",";
  let s3 = "  \"post\": " + post + "}";
  print ("=== CodeAlchemist Start ===");
  print (s1);
  print (s2);
  print (s3);
  print ("=== CodeAlchemist End ===");
}

function codealchemist_log_type_pre (hval, vars){
  let pre = codealchemist_get_types (vars);
  if (!(hval in codealchemist_tmps)) codealchemist_tmps[hval] = [];
  codealchemist_tmps[hval].push (pre);
}

function codealchemist_log_type_post (hval, vars){
  let post = codealchemist_get_types (vars);
  let pre = codealchemist_tmps[hval].pop ();
  let key = pre + post;
  if (pre === undefined || post === undefined) return;
  if (!(hval in codealchemist_logs)) codealchemist_logs[hval] = {};
  if (!(key in codealchemist_logs[hval])) {
    codealchemist_logs[hval][key] = true;
    codealchemist_print (hval, pre, post);
  }
}
