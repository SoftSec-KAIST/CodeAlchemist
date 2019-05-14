const esprima = require('esprima');
const fs = require('fs');
const path = require('path');
const print = console.log;
const exit = process.exit;
const PRE = "CodeAlchemist_V8Native"
const RE = new RegExp (PRE, "g");

function resolveV8Native (src) {
    src = src.replace(/%[a-zA-Z]+/g, function(src){ return PRE + src.slice(1)});
    return esprima.parse(src);
}

function parse(fname, isV8){
    try{
        var src = fs.readFileSync(fname, 'utf8');
        return esprima.parse(src);
    }catch(e){
        if (isV8) {
            try { return resolveV8Native (src) }
            catch (e) { print (fname + ' : parse error! ' + e); }
        } else print(fname + ' : parse error! ' + e);
    }
}

function astToJson(ast, isV8){
    try{
        var ret = JSON.stringify(ast, null, 2);
    }catch(e){
        print('astsToJson error!');
    }
    if (isV8 && ret) ret = ret.replace(RE, "%")
    return ret
}

function listDir(dir){
  return fs.readdirSync(dir)
}

function main(){
  var engine = process.argv[2];
  var isV8 = (engine === "V8");
  var js_dir = process.argv[3];
  var json_dir = process.argv[4];
  var js_files = listDir(js_dir);
  for (let name of js_files){
    if(name.endsWith('.js')){
      let json = astToJson(parse(path.join(js_dir, name), isV8), isV8);
      if(json !== undefined){
        fs.writeFileSync(path.join(json_dir, name), json);
      }
    }

  }
}
main()
