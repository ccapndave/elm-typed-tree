import * as fs from "fs"
import * as path from "path"
import { generate } from "./generate"

fs.writeFileSync(
  path.resolve(__dirname, "..", "src", "TreePath", "Data.elm"),
  generate(path.resolve(__dirname, "templates", "Data.elm.ejs"))
);

for (let n = 2; n <= 10; n++) {
  fs.writeFileSync(
    path.resolve(__dirname, "..", "src", "TreePath", `Tree${n}.elm`),
    generate(path.resolve(__dirname, "templates", "TreeN.elm.ejs"), n)
  );
}
