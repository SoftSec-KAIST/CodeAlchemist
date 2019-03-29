const esprima = require('esprima');
const fs = require('fs');
const path = require('path');
const print = console.log;
const exit = process.exit;

function parse(fname){
    try{
        var str = fs.readFileSync(fname, 'utf8');
        return esprima.parse(str);
    }catch(e){
        print(fname + ' : parse error! ' + e);
    }
}

function astToJson(ast){
    try{
        var ret = JSON.stringify(ast, null, 2);
    }catch(e){
        print('astsToJson error!');
    }
    return ret
}

function listdir(dir){
  return fs.readdirSync(dir)
}

function main(){
  var js_dir = process.argv[2];
  var json_dir = process.argv[3];
  var js_files = listdir(js_dir);
  for (let name of js_files){
    if(name.endsWith('.js')){
      let json = astToJson(parse(path.join(js_dir, name)));
      if(json !== undefined){
        fs.writeFileSync(path.join(json_dir, name), json);
      }
    }

  }
}
main()
