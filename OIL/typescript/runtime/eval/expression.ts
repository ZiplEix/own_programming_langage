import { AssignmentExpr, BinaryExpr, CallExpr, Identifier, ObjectLiteral } from "../../frontend/ast";
import Environment from "../environment";
import { evaluate } from "../interpreter";
import { NumberValue, RuntimeValue, MK_NULL, ObjectValue, NativeFunctionValue, FunctionValue } from "../values";

export function evalNumericBinaryExpression(lhs: NumberValue, rhs: NumberValue, operator: string): RuntimeValue {
    let result = 0;

    if (operator === "+") {
        result = lhs.value + rhs.value;
    } else if (operator === "-") {
        result = lhs.value - rhs.value;
    } else if (operator === "*") {
        result = lhs.value * rhs.value;
    } else if (operator === "/") {
        // TODO: handle division by zero
        result = lhs.value / rhs.value;
    } else if (operator === "%") {
        result = lhs.value % rhs.value;
    } else {
        console.error("Unknown binary operator: ", operator);
        process.exit(1);
    }

    return { type: "number", value: result } as NumberValue;
}

export function evalBinaryExpression(binop: BinaryExpr, env: Environment): RuntimeValue {
    const lhs = evaluate(binop.left, env);
    const rhs = evaluate(binop.right, env);

    if (lhs.type === "number" && rhs.type === "number") {
        return evalNumericBinaryExpression(lhs as NumberValue, rhs as NumberValue, binop.operator);
    }

    return MK_NULL();
}

export function evalIdentifier(ident: Identifier, env: Environment): RuntimeValue {
    const val = env.lookUpVar(ident.symbol);
    return val;
}

export function evalAssignment(node: AssignmentExpr, env: Environment): RuntimeValue {
    if (node.assigne.kind != "Identifier") {
        throw `Invalid assignment target inside expression ${JSON.stringify(node.assigne)}`;
    }

    const varname = (node.assigne as Identifier).symbol;
    return env.assignVar(varname, evaluate(node.value, env));
}

export function evalObjectExpr(obj: ObjectLiteral, env: Environment): RuntimeValue {
    const object = {
        type: "object",
        properties: new Map()
    } as ObjectValue;

    for (const {key, value} of obj.properties) {
        const runtimeValue = (value == undefined) ? env.lookUpVar(key) : evaluate(value, env);

        object.properties.set(key, runtimeValue);
    }

    return object;
}

export function evalCallExpr(expr: CallExpr, env: Environment): RuntimeValue {
    const args = expr.args.map(arg => evaluate(arg, env));
    const func = evaluate(expr.caller, env);

    if (func.type === "nativeFunction") {
        const result = (func as NativeFunctionValue).call(args, env);

        return result;
    }

    if (func.type === "function") {
        const fn = func as FunctionValue;
        const scope = new Environment(fn.declarationEnv);

        // create the variables for the parameters list
        for (let i = 0; i < fn.params.length; i++) {
            // TODO: check the bounds here
            // verify arity of function
            const varName = fn.params[i];
            scope.declareVar(varName, args[i], true);
        }

        let result: RuntimeValue = MK_NULL();

        for (const stmt of fn.body) {
            result = (evaluate(stmt, scope));
        }

        return result;
    }

    throw `Invalid function call: ${func}`;
}