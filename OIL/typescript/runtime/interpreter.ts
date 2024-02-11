import { ValueType, RuntimeValue, NumberValue, NullValue } from "./values";
import { BinaryExpr, NodeType, NumericLiteral, Program, Stmt } from "../frontend/ast";

function evalProgramm(program: Program): RuntimeValue {
    let lastEvaluated: RuntimeValue = { type: "null", value: "null" } as NullValue;

    for (const stmt of program.body) {
        lastEvaluated = evaluate(stmt);
    }

    return lastEvaluated;
}

function evalNumericBinaryExpression(lhs: NumberValue, rhs: NumberValue, operator: string): RuntimeValue {
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

function evalBinaryExpression(binop: BinaryExpr): RuntimeValue {
    const lhs = evaluate(binop.left);
    const rhs = evaluate(binop.right);

    if (lhs.type === "number" && rhs.type === "number") {
        return evalNumericBinaryExpression(lhs as NumberValue, rhs as NumberValue, binop.operator);
    }

    return { type: "null", value: "null" } as NullValue;
}

export function evaluate(astNode: Stmt): RuntimeValue {
    switch (astNode.kind) {
        case "NumericLiteral":
            return { type: "number", value: ((astNode as NumericLiteral).value) } as NumberValue;

        case "NullLiteral":
            return { type: "null", value: "null" } as NullValue;

        case "BinaryExpr":
            return evalBinaryExpression(astNode as BinaryExpr);

        case "Program":
            return evalProgramm(astNode as Program);

        default:
            console.error("Unknown AST node: ", astNode);
            process.exit(1);
            return { type: "null", value: "null" } as NullValue;
    }
}
