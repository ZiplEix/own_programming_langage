import { FunctionDeclaration, Program, VariableDeclaration } from "../../frontend/ast";
import Environment from "../environment";
import { evaluate } from "../interpreter";
import { RuntimeValue, MK_NULL, FunctionValue } from "../values";

export function evalProgramm(program: Program, env: Environment): RuntimeValue {
    let lastEvaluated: RuntimeValue = MK_NULL();

    for (const stmt of program.body) {
        lastEvaluated = evaluate(stmt, env);
    }

    return lastEvaluated;
}

export function evalVaraibleDeclaration(declaraation: VariableDeclaration, env: Environment): RuntimeValue {
    const value = declaraation.value ? evaluate(declaraation.value, env) : MK_NULL();
    return env.declareVar(declaraation.identifier, value, declaraation.constant);
}

export function evalFunctionDeclaration(declaraation: FunctionDeclaration, env: Environment): RuntimeValue {
    const func = {
        type: "function",
        name: declaraation.name,
        params: declaraation.params,
        declarationEnv: env,
        body: declaraation.body
    } as FunctionValue;

    return env.declareVar(declaraation.name, func, true);
}
