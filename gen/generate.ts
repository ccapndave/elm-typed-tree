import * as fs from "fs"
import * as path from "path"
import { spawnSync } from "child_process"
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

  const treePathDestructurer = (n: number) => {
    if (n === height) {
      return `((TreePath${n} tree${n} _ _) as treePath${n})`;
    } else {
      return `((TreePath${n} tree${n} idx${n} ${treePathDestructurer(n + 1)}) as treePath${n})`;
    }
  }

  const source = fs.readFileSync(templatePath, 'utf8');
  const rendered = ejs.render(source, { height, alphabet, treePathDestructurer });

  const { stdout, stderr, status } = spawnSync(path.resolve(__dirname, "..", "node_modules", ".bin", "elm-format"), [ "--stdin" ], { input: rendered });

  if (status === 0) {
    return stdout.toString();
  } else {
    console.error(stderr.toString());
    return rendered;
  }
}
