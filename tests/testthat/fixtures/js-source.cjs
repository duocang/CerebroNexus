'use strict';

// Isolated tests execute real declarations. Use the JavaScript parser so braces
// in strings/comments, indentation and declaration order cannot change what is
// loaded. Integration tests separately exercise the assembled browser scripts.
const {parse} = require('acorn');

function walk(node, visit, parents = []) {
  if (!node || typeof node !== 'object') return;
  if (node.type) visit(node, parents);
  for (const value of Object.values(node)) {
    if (Array.isArray(value)) value.forEach(child => walk(child, visit, [...parents, node]));
    else if (value && typeof value === 'object') walk(value, visit, [...parents, node]);
  }
}

function javascript(source) {
  const ast = parse(source, {ecmaVersion: 'latest', sourceType: 'script'});
  function one(predicate, description) {
    const matches = [];
    walk(ast, (node, parents) => { if (predicate(node, parents)) matches.push(node); });
    if (matches.length !== 1) throw Error(`${description}: expected one declaration, found ${matches.length}`);
    return matches[0];
  }
  return {
    declarations(names, within) {
      return names.map(name => {
        const node = one((n, parents) =>
          ((n.type === 'FunctionDeclaration' || n.type === 'VariableDeclarator') && n.id?.name === name) &&
          (!within || parents.some(p => p.type === 'FunctionDeclaration' && p.id?.name === within)), name);
        const code = source.slice(node.start, node.end);
        return node.type === 'VariableDeclarator' ? `var ${code};` : code;
      }).join('\n');
    },
    handler(name) {
      const node = one(n => n.type === 'CallExpression' &&
        n.callee.type === 'MemberExpression' && n.callee.property.name === 'addCustomMessageHandler' &&
        n.arguments[0]?.value === name, `message handler ${name}`);
      return source.slice(node.start, node.end) + ';';
    },
    readyFor(id) {
      const node = one(n => {
        if (n.type !== 'CallExpression' || n.callee.name !== 'ready') return false;
        let found = false;
        walk(n, child => { if (child.type === 'Literal' && child.value === id) found = true; });
        return found;
      }, `ready callback for ${id}`);
      return source.slice(node.start, node.end) + ';';
    }
  };
}
module.exports = javascript;
