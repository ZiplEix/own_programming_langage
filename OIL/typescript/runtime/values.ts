import { Stmt } from "../frontend/ast";
import Environment from "./environment";

export type ValueType = "null" | "number" | "boolean" | "object" | "nativeFunction" | "function";

export interface RuntimeValue {
    type: ValueType
}

/**
 * Define a value of indefined meaning
 */
export interface NullValue extends RuntimeValue {
    type: "null";
    value: null;
}

export function MK_NULL(): NullValue {
    return { type: "null", value: null } as NullValue;
}

/**
 * Define a value of boolean type
 */
export interface BooleanValue extends RuntimeValue {
    type: "boolean";
    value: boolean;
}

export function MK_BOOL(b: boolean = true): BooleanValue {
    return { type: "boolean", value: b } as BooleanValue;
}

/**
 * Define a value of number type
 */
export interface NumberValue extends RuntimeValue {
    type: "number";
    value: number;
}

export function MK_NUM(n: number = 0): NumberValue {
    return { type: "number", value: n } as NumberValue;
}

/**
 * Define a value of object type
 */
export interface ObjectValue extends RuntimeValue {
    type: "object";
    properties: Map<string, RuntimeValue>;
}

export type FunctionCall = (args: RuntimeValue[], env: Environment) => RuntimeValue;

/**
 * Define a value of native function type
 */
export interface NativeFunctionValue extends RuntimeValue {
    type: "nativeFunction";
    call: FunctionCall
}

export function MK_NATIVE_FUNCTION(call: FunctionCall): NativeFunctionValue {
    return { type: "nativeFunction", call } as NativeFunctionValue;
}

/**
 * Define a value of native function type
 */
export interface FunctionValue extends RuntimeValue {
    type: "function";
    name: string,
    params: string[],
    declarationEnv: Environment,
    body: Stmt[]
}
