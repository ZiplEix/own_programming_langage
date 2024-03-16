import { MK_BOOL, MK_NATIVE_FUNCTION, MK_NULL, MK_NUM, RuntimeValue } from "./values";

export function createGlobalEnv() {
    const env = new Environment();
    // Define some built-in variables
    env.declareVar("true", MK_BOOL(true), true);
    env.declareVar("false", MK_BOOL(false), true);
    env.declareVar("null", MK_NULL(), true);

    // define a native builtin methode
    env.declareVar(
        "print",
        MK_NATIVE_FUNCTION((args, scope) => {
            console.log(...args);
            return MK_NULL();
        }),
        true
    );

    // en function that get the current time
    const timeFunction = (args: RuntimeValue[], scope: Environment) => {
        return MK_NUM(Date.now());
    };
    env.declareVar("time", MK_NATIVE_FUNCTION(timeFunction), true);

    return env;
}

export default class Environment {

    constructor(parentEnv?: Environment) {
        const global = parentEnv ? true : false
        this.parent = parentEnv;
        this.variables = new Map();
        this.constants = new Set();
    }

    private parent?: Environment;
    private variables: Map<string, RuntimeValue>;
    private constants: Set<string>;

    public declareVar(varName: string, value: RuntimeValue, isConstant: boolean): RuntimeValue {
        if (this.variables.has(varName)) {
            throw `Variable "${varName}" already declared`;
        }

        this.variables.set(varName, value);

        if (isConstant) {
            this.constants.add(varName);
        }

        return value;
    }

    public assignVar(varName: string, value: RuntimeValue): RuntimeValue {
        const env = this.resolve(varName);

        if (env.constants.has(varName)) {
            throw `Can not reassign const constant "${varName}"`;
        }

        env.variables.set(varName, value);

        return value;
    }

    public lookUpVar(varName: string): RuntimeValue {
        const env = this.resolve(varName);
        return env.variables.get(varName) as RuntimeValue;
    }

    public resolve(varName: string): Environment {
        if (this.variables.has(varName)) {
            return this;
        }

        if (this.parent == undefined) {
            throw `Can not resolve, "${varName}" not declared`;
        }

        return this.parent.resolve(varName);
    }
}
