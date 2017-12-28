const { exec } = require('child_process')
const path = require('path')
const fs = require('fs')

function parseArgs() {
  return {
    docsInput: process.argv[2],
    docsOutput: process.argv[3],
    debug: process.argv.includes('--debug')
  }
}

function indexHtml(json) {
  return `\
<!DOCTYPE HTML>
<html>
  <head>
    <title>Docs</title>
    <script src="app.js"></script>
    <script src="docs.js"></script>
  </head>

  <body>
    <script type="text/javascript">
      Elm.Main.fullscreen({
        docsJson: ${json}
      })
    </script>
  </body>
</html>
`
}

function handleError(err) {
  console.log(err)
}

function main() {
  const { docsInput, docsOutput, debug } = parseArgs()
  if (!docsInput || !docsOutput) {
    console.log('Must pass input json and output dir')
    return
  }
  const outputDir = path.resolve(docsOutput)
  const inputFile = path.resolve(docsInput)
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir)
  }
  process.chdir(__dirname)
  exec(`elm make src\\Main.elm --output=${outputDir}\\app.js --yes ${debug ? '--debug' : ''}`)
  fs.readFile(inputFile, 'utf8', (err, data) => {
    if (err) {
      handleError(err)
    } else {
      fs.writeFile(`${outputDir}\\index.html`, indexHtml(data), handleError)
    }
  })
}

main()
