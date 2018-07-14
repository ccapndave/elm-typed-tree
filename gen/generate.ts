import * as fs from "fs"
import * as path from "path"
import { execSync } from "child_process"
import * as ejs from "ejs"

export function generate(templatePath: string, height: number = null): string {
  const alphabet = (times, startOffset = 0) => {
    const startCharCode = "a".charCodeAt(0) + startOffset;
    let letters = [];
    for (let n = startCharCode; n < startCharCode + times; n++) {
      letters.push(String.fromCharCode(n));
    }
    return letters.join(" ");
  };

  const source = fs.readFileSync(templatePath, 'utf8');
  const rendered = ejs.render(source, { height, alphabet });

  try {
    const formatted = execSync(path.resolve(__dirname, "..", "node_modules", ".bin", "elm-format --stdin"), { input: rendered }).toString();
    return formatted;
  } catch (err) {
    console.error(err.toString());
    console.log("---");
    console.log(rendered);
  }
}
