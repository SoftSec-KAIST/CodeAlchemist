A configuration file is json file with following fields. And you can find
example configuration files for 4 JS engines.
- `engine`: Type of JS engine ("Charka", "JSC", "MOZ", "V8").
- `engine_path`: ABSPATH to engine
- `timeout`: Timeout for executing a JS code.
- `argv`: Additional arguments for executing a JS engine.
- `env`: Additional environment variables for executing a JS engine.
- `seed_path`: ABSPATH to seed.
- `preproc_dir`: ABSPATH for saving preprocessing results.
- `tmp_dir`: ABSPATH for temporarily saving generated JS code.
- `bug_dir`: ABSPATH for saving JS code which triggered some crash.
- `built-ins`: List of built-ins symbols.
- `filters`: List of symbols to exclude from the code brick pool.
- `jobs`: The number of jobs (cores) to use for fuzzing.