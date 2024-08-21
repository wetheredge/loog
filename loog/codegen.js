#!/usr/bin/env bun

import assert from 'node:assert/strict'
import data from './macros.toml'

const outputPath = 'src/macros.rs'

await Bun.$`rm -f ${outputPath}`
const output = Bun.file(outputPath).writer()
const write = (s = '') => output.write(s)
const writeln = (s = '') => write(`${s}\n`)

for (const [rawName, definitions] of Object.entries(data)) {
	const names = rawName.includes('{')
		? cartesian(...rawName.split(/[{}]/).map(s => s.split(','))).map(a => a.join(''))
		: [rawName];

	for (const name of names) {
		for (const { defmt, log, std, alloc, ...arms } of definitions) {
			const features = Object.entries({ defmt, log, std, alloc })
			const featureCfg = ([name,]) => `feature = "${name}"`
			const allEnabled = features.filter(([_, x]) => x === true).map(featureCfg)
			const disabled = features.filter(([_, x]) => x === false).map(featureCfg)
			assert(allEnabled.length <= 1, 'max 1 enabled feature')
			const enabled = allEnabled[0];
			assert((allEnabled.length + disabled.length) >= 1, 'at least 1 feature enabled or disabled')
			const both = enabled != null && disabled.length > 0

			write('#[cfg(')
			if (both) {
				write('all(')
			}
			if (disabled.length === 1) {
				write(`not(${disabled[0]}),`)
			} else if (disabled.length >= 2) {
				write(`not(any(${disabled.join(', ')})),`)
			}
			if (enabled != null) write(enabled)
			if (both) write(')')
			writeln(')]')

			writeln('#[macro_export]')
			writeln(`macro_rules! ${name} {`)

			for (const [matcher, body] of Object.entries(arms)) {
				write('\t(')
				write(matcher)
				if (matcher !== '') {
					write(' $(,)?')
				}
				write(`) => {{ `)
				write(body.replaceAll(rawName, name))
				writeln(' }};')
			}

			writeln('}')
			writeln()
		}
	}
}

output.end()

// https://stackoverflow.com/a/43053803
function cartesian(...a) {
	return a.reduce((a, b) => a.flatMap(d => b.map(e => [d, e].flat())))
}
