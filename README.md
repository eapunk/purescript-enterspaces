# purescript-enterspaces

Pretty debug trace.

See [web app](https://github.com/eapunk/enterspaces).

Build js
```sh
pulp browserify --optimise --main Text.Show.Pretty --standalone Enterspaces | uglifyjs -c > enterspaces.min.js
```
