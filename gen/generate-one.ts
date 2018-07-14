import * as path from "path"
import { generate } from "./generate"

const height = process.argv[2];

if (isNaN(parseInt(height)) || parseInt(height) < 2) {
  console.error("You must pass a height of at least 2");
} else {
  const result = generate(path.resolve(__dirname, "templates", "TreeN.elm.ejs"), parseInt(height));
  if (result) console.log(result);
}
