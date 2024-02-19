# Raqiya RetroCipher

**RetroCipher** is a programming language designed for reverse engineering. It is an imperative and procedural programming language influenced by C and Rust. It has the following features:

- **Static typing**. The type of every variable is known at compile time.
- **Minimal** Due to requirement for correspondence with diassembled code, the language is kept minimal for ease of use and so that higher-level abstractions do not make it unnecessarily difficult to reason about compiled code.
- **Implement decompilation in pieces** For example, you can decompile a single function at a time. You don't have to decompile the entire binary at once.
- **Verify compiled code against original assembly** Compiled code is checked against the original disassembly to ensure no logic was changed.
- **Link the way you want** You can link compiled code with various options.

Currently, it is intended to support compilation to MIPS assembly only.

The compiler has a particular use case: to help out reverse engineering Spyro the Dragon and then port it to PC and Linux. See [OpenSpyro](https://github.com/Henri-Mikael-Korpela/open-spyro) for more information about the project.

## About the documentation

This documentation is a work in progress. It may contain information about how the language works, which may not be implemented yet.

## Built-in types

- Boolean: `Bool` - With values `true` and `false`.
- Character: `Charu8` - "u8" indicates a character that can be stored in an unsigned 8-bit integer (basically ASCII-compatible character).
- Integer: `I8 I16 I32 U8 U16 U32` - Each number stands for number of bits they can hold.
- String: `StrNull` - "null" indicates that a string is null-terminated (character `\0` at the end of string value).

Currently, there is no support for 64-bit integers, because 64-bit integers are not (probably) needed for dealing with assembly on MIPS R3000 CPU.