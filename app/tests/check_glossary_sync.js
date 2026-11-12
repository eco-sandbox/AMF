// Checks that the glossary embedded in the v3 HTML equals shiny/glossary.json (the single source).
const fs = require('fs'), path = require('path');
const root = path.join(__dirname, '..', '..');
const h = fs.readFileSync(path.join(root, 'amf_spectrum_app.html'), 'utf8');
const g = h.match(/<script id="glossary">([\s\S]*?)<\/script>/)[1];
const { GLOSSARY, TERMS } = new Function(g + ';return {GLOSSARY,TERMS};')();
const J = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'glossary.json'), 'utf8'));
let diff = 0;
const F = ['sym', 'md', 'name', 'notes', 'units', 'tabs', 'src', 'sourceSymbol'];
if (GLOSSARY.length !== J.symbols.length) { diff++; console.log('row count differs'); }
GLOSSARY.forEach((x, i) => { for (const f of F) if ((x[f] ?? '') !== (J.symbols[i][f === 'sourceSymbol' ? 'source_symbol' : f] ?? '')) { diff++; console.log(i, f, x[f], '≠', J.symbols[i][f]); }
  if (JSON.stringify(x.keys || []) !== JSON.stringify(J.symbols[i].keys)) { diff++; console.log(i, 'keys'); } });
TERMS.forEach(([t, d], i) => { if (t !== J.terms[i].term || d !== J.terms[i].definition) { diff++; console.log('term', i); } });
console.log(`glossary sync HTML v3 vs glossary.json: ${GLOSSARY.length} symbols, ${TERMS.length} terms, ${diff} differences`);
process.exit(diff ? 1 : 0);
