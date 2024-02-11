import * as readline from 'readline';
import * as util from 'util';

import Parser from "./frontend/parser";
import { evaluate } from './runtime/interpreter';

repl();

function repl() {
    const parser = new Parser();

    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: '>>> '
    });

    console.log("\nOIL REPL v0.1");

    rl.prompt();

    rl.on('line', (input: string) => {
        if (!input || input.includes("exit")) {
            rl.close();
        }

        const program = parser.produceAST(input);

        const result = evaluate(program);
        console.log(util.inspect(result, { depth: null }));

        rl.prompt();
    }).on('close', () => {
        console.log('Exiting REPL...');
        process.exit(0);
    });
}