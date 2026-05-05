# monkey-wasm

``` shell
wasm-pack build --target web
python -m http.server
```

Then visit [http://localhost:8000](http://localhost:8000)

`MonkeyState` accepts an optional plain object or `Map` of host functions:

```js
const stateBuilder = new MonkeyStateBuilder()
stateBuilder.registerFunction("js_upper", (value) => String(value).toUpperCase());
stateBuilder.registerFunction("js_sum", (...values) =>
values.reduce((sum, value) => sum + Number(value), 0))
stateBuilder.registerFunction("greeter", (name) => `hello ${name} from js`)
const state = stateBuilder.build()
```
