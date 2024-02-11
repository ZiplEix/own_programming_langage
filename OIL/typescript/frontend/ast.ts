export type NodeType =
    | "Program"
    | "NumericLiteral"
    | "NullLiteral"
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

/**
 * Expression will result in a value at runtime unlike statements
 */
export interface Expr extends Stmt {}

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

/**
 * Represent a null value
 */
export interface NullLiteral extends Expr {
    kind: "NullLiteral"
    value: "null"
}
