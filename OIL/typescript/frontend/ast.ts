export type NodeType =
    // STATEMENTS
    | "Program"
    | "variableDeclaration"
    | "FunctionDeclaration"

    // EXPRESSIONS
    | "AssignmentExpr"
    | "MemberExpr"
    | "CallExpr"

    // LITERALS
    | "Property"
    | "ObjectLiteral"
    | "NumericLiteral"
    | "Identifier"
    | "BinaryExpr"

/**
 * Statement do not result in a value at runtime
 * They contain one or more expression internaly
 */
export interface Stmt {
    kind: NodeType
}

/**
 * Define a block whitch contains many statements
 * - Only one program per file
 */
export interface Program extends Stmt {
    kind: "Program"
    body: Stmt[]
}

export interface VariableDeclaration extends Stmt {
    kind: "variableDeclaration"
    constant: boolean,
    identifier: string,
    value?: Expr
}

export interface FunctionDeclaration extends Stmt {
    kind: "FunctionDeclaration"
    name: string,
    params: string[],
    body: Stmt[]
}

/**
 * Expression will result in a value at runtime unlike statements
 */
export interface Expr extends Stmt {}

export interface AssignmentExpr extends Expr {
    kind: "AssignmentExpr"
    assigne: Expr,
    value: Expr
}

/**
 * An operation with two side separated by an operator
 * Both side can be any expression
 * - suported operators : +, -, *, /, %
 */
export interface BinaryExpr extends Expr {
    kind: "BinaryExpr"
    left: Expr
    right: Expr
    operator: string
}


export interface CallExpr extends Expr {
    kind: "CallExpr"
    args: Expr[]
    caller: Expr
}


export interface MemberExpr extends Expr {
    kind: "MemberExpr"
    object: Expr
    property: Expr
    computed: boolean
}

// PRIMARY EXPRESSIONS TYPES

/**
 * Represent a user-defined variable or a symbol
 */
export interface Identifier extends Expr {
    kind: "Identifier"
    symbol: string
}

/**
 * Represent a number
 */
export interface NumericLiteral extends Expr {
    kind: "NumericLiteral"
    value: number
}


export interface Property extends Expr {
    kind: "Property"
    key: string
    value?: Expr
}


export interface ObjectLiteral extends Expr {
    kind: "ObjectLiteral"
    properties: Property[]
}