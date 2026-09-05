// PostToolUse hook: after Claude edits or writes a file in this repo, ask it
// to review the file against the matching style skill:
//
//   R source and tests (.R, .Rmd, .qmd)         -> tidyverse-style
//   prose (.md, .Rmd, .qmd, .Rd, .txt, NEWS,
//          DESCRIPTION, roxygen in .R files)     -> plain-language
//
// Claude Code feeds a PostToolUse hook's stderr back to Claude when the hook
// exits with code 2, so that is the channel used here. Other files exit 0.
let input = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => (input += chunk));
process.stdin.on("end", () => {
  let filePath = "";
  try {
    const payload = JSON.parse(input);
    filePath = (payload.tool_input && payload.tool_input.file_path) || "";
  } catch (e) {
    process.exit(0);
  }
  const name = filePath.split(/[\\/]/).pop() || "";
  const isCode = /\.(R|r|Rmd|rmd|qmd)$/.test(name);
  const isProse =
    /\.(md|Rmd|rmd|qmd|Rd|txt)$/i.test(name) ||
    /^(DESCRIPTION|NEWS|README|CITATION)/.test(name) ||
    // roxygen blocks live in R sources
    /\.R$/.test(name);

  const asks = [];
  if (isCode) {
    asks.push(
      "the tidyverse-style skill (Skill tool, skill: \"tidyverse-style\") for " +
        "the code: naming, spacing, pipes, braces, return(), comments, and " +
        "roxygen or testthat conventions"
    );
  }
  if (isProse) {
    asks.push(
      "the plain-language skill (Skill tool, skill: \"plain-language\") for " +
        "any prose in it (documentation, roxygen text, messages, NEWS): main " +
        "point first, short sentences, active voice, terms defined once, " +
        "no slashes between words"
    );
  }
  if (asks.length === 0) process.exit(0);

  process.stderr.write(
    `You just wrote to ${filePath}. Before continuing, review that file ` +
      `against ${asks.join(", and ")}. Fix any deviations in that file ` +
      "only, then carry on with the task.\n"
  );
  process.exit(2);
});
