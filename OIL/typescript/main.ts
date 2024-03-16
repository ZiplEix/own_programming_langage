import * as readline from 'readline';
import * as util from 'util';

import Parser from "./frontend/parser";
import { evaluate } from './runtime/interpreter';
import Environment, { createGlobalEnv } from './runtime/environment';
import { MK_NULL, MK_NUM, MK_BOOL } from './runtime/values';

import { readFileSync } from 'fs';

// repl();
run("./test.txt");

function run(filename: string) {
    const parser = new Parser();
    const env = createGlobalEnv();

    // get the content of a file
    const input = readFileSync(filename, 'utf8');
    const program = parser.produceAST(input);
    const result = evaluate(program, env);
    // console.log(util.inspect(result, { depth: null, showHidden: true }));
}

function repl() {
    const parser = new Parser();
    const env = createGlobalEnv();

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

        const result = evaluate(program, env);
        console.log(util.inspect(result, { depth: null }));

        rl.prompt();
    }).on('close', () => {
        console.log('Exiting REPL...');
        process.exit(0);
    });
}
