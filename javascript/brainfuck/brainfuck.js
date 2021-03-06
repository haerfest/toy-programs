/**
 * Runs a Brainfuck program against certain input, returning its output.
 *
 * @param {string} program  The Brainfuck program to run.
 * @param {string} input    Any input for the program.
 *
 * @return {string} Its output.
 */
function brainfuck(program, input) {
    'use strict';

    // Memory is 30,000 bytes, as per Brainfuck spec.
    var mem    = new Array(30000),
        ip     = 0,
        dp     = 0,
        output = '',
        i;

    /**
     * Executes one instruction, returning whether it succeeded or not.
     *
     * @return {boolean} True upon success, false otherwise.
     */
    function step() {
        if (ip >= mem.length) {
            // We've literally run outside of memory.
            return false;
        }

        switch (program[ip]) {
        case '>':
            // Increment data pointer.
            dp += 1;
            break;

        case '<':
            // Decrement data pointer.
            dp -= 1;
            break;

        case '+':
            // Increment memory.
            mem[dp] += 1;
            break;

        case '-':
            // Decrement memory.
            mem[dp] -= 1;
            break;

        case '.':
            // Output memory.
            output += String.fromCharCode(mem[dp]);
            break;

        case ',':
            // Store input in memory.
            mem[dp] = input.substr(0, 1).charCodeAt(0);
            input = input.substr(1);
            break;

        case '[':
            // Jump forward if memory is zero.
            if (mem[dp] === 0) {
                ip = program.indexOf(']', ip + 1);
            }
            break;

        case ']':
            // Jump backward unless memory is zero.
            if (mem[dp] !== 0) {
                ip = program.lastIndexOf('[', ip - 1);
            }
            break;

        default:
            // Illegal instruction.
            return false;
        }

        ip += 1;
        return true;
    }

    // Initialize all memory to zero, as per Brainfuck spec.
    for (i = 0; i < mem.length; i += 1) {
        mem[i] = 0;
    }

    // Step until no more instructions can be executed.
    for (;;) {
        if (!step()) {
            break;
        }
    }

    return output;
}


/**
 * Executes a Brainfuck program that outputs "Hello World!\n", and returns
 * that output.
 *
 * @return {string} "Hello World!\n"
 */
function helloWorld() {
    'use strict';

    var program = '++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+'
                + '++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.',
        input   = '';

    return brainfuck(program, input);
}
