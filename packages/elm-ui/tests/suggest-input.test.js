// What a suggesting text field offers, and where its cursor goes — checked
// without a browser.
//
// Two rules are worth a guard, and neither is visible to elm-test or elm-review:
//
//   The filter. It is the existing wizard's — a case-insensitive "contains",
//   not a prefix match — so that the same typing finds the same names as it did
//   on the sign-up screen. A prefix match would quietly stop finding
//   "University of Amsterdam" for "amsterdam".
//
//   The cursor. It wraps, and it starts outside the list: the field is free
//   text, so opening the list must not put a value under Enter that nobody
//   asked for.

const assert = require('assert')
const { matches, nextIndex } = require('../js/suggest-input')

const suggestions = ['Charles University', 'Czech Technical University', 'Masaryk University', 'ELIXIR CZ']

// Nothing typed: everything offered, and a copy — the caller must not be able
// to sort or splice the field's own list.
assert.deepStrictEqual(matches('', suggestions), suggestions)
assert.notStrictEqual(matches('', suggestions), suggestions)

// Anywhere in the name, in any case, and whitespace either side is not typing.
assert.deepStrictEqual(matches('university', suggestions), ['Charles University', 'Czech Technical University', 'Masaryk University'])
assert.deepStrictEqual(matches('CZECH', suggestions), ['Czech Technical University'])
assert.deepStrictEqual(matches('  elixir  ', suggestions), ['ELIXIR CZ'])
assert.deepStrictEqual(matches('nothing here', suggestions), [])

// The cursor starts outside the list, so the first Down is the first option and
// the first Up is the last one.
assert.strictEqual(nextIndex(-1, 1, 4), 0)
assert.strictEqual(nextIndex(-1, -1, 4), 3)

// And it wraps at both ends.
assert.strictEqual(nextIndex(3, 1, 4), 0)
assert.strictEqual(nextIndex(0, -1, 4), 3)
assert.strictEqual(nextIndex(1, 1, 4), 2)

// An empty list has nowhere to go, rather than an index into nothing.
assert.strictEqual(nextIndex(-1, 1, 0), -1)

console.log('ui suggest input contract OK')
