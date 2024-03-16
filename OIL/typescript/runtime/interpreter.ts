import * as util from 'util';

import { RuntimeValue, NumberValue, NullValue, MK_NULL } from "./values";
import { BinaryExpr, NumericLiteral, Program, Stmt, Identifier, VariableDeclaration, AssignmentExpr, ObjectLiteral, CallExpr, FunctionDeclaration } from "../frontend/ast";
import Environment from "./environment";
import { evalIdentifier, evalBinaryExpression, evalAssignment, evalObjectExpr, evalCallExpr } from "./eval/expression";
import { evalFunctionDeclaration, evalProgramm, evalVaraibleDeclaration } from "./eval/statement";

export function evaluate(astNode: Stmt, env: Environment): RuntimeValue {
    switch (astNode.kind) {
        case "NumericLiteral":
            return { type: "number", value: ((astNode as NumericLiteral).value) } as NumberValue;

        case "Identifier":
            return evalIdentifier(astNode as Identifier, env);

        case "ObjectLiteral":
            return evalObjectExpr(astNode as ObjectLiteral, env);

        case "CallExpr":
            return evalCallExpr(astNode as CallExpr, env);

        case "AssignmentExpr":
            return evalAssignment(astNode as AssignmentExpr, env);

        case "BinaryExpr":
            return evalBinaryExpression(astNode as BinaryExpr, env);

        case "Program":
            return evalProgramm(astNode as Program, env);

        case "variableDeclaration":
            return evalVaraibleDeclaration(astNode as VariableDeclaration, env);

        case "FunctionDeclaration":
            return evalFunctionDeclaration(astNode as FunctionDeclaration, env);

        default:
            console.error("Unknown AST node: ", util.inspect(astNode, { depth: null, showHidden: true }));
            process.exit(1);
    }
}
