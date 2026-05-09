package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

import static dev.capylang.compiler.PrimitiveLinkedType.ANY;
import static dev.capylang.compiler.PrimitiveLinkedType.DATA;
import static dev.capylang.compiler.PrimitiveLinkedType.ENUM;
import static dev.capylang.compiler.PrimitiveLinkedType.NOTHING;

public class CapybaraTypeFinder {
    public static CompiledType findHigherType(CompiledType left, CompiledType right) {
        if (left.equals(right)) return left;
        if (left == NOTHING) return right;
        if (right == NOTHING) return left;
        if (left == ANY || right == ANY) return ANY;
        if (left == DATA && !(right instanceof PrimitiveLinkedType)) return DATA;
        if (right == DATA && !(left instanceof PrimitiveLinkedType)) return DATA;

        if (left instanceof PrimitiveLinkedType leftPt) {
            if (right instanceof PrimitiveLinkedType rightPt) {
                return findHigherType(leftPt, rightPt);
            }
            // left is primitive but right is not primitive
            // there definitely is not a common type
            return ANY;
        }
        if (right instanceof PrimitiveLinkedType rightPt) {
            // right is primitive but left is not primitive
            // there definitely is not a common type
            return ANY;
        }

        // both are not primitive types
        // for now returning ANY
        // todo: get data types and find common types there
        return ANY;
    }

    /// `int`:
    /// - `int + int -> int`
    /// - `int + float -> float`
    /// - `int + string -> string`
    /// - `int + bool -> bool`
    /// - `int + any -> any`
    ///
    /// `float`:
    ///  - `float + int -> float`
    ///  - `float + float -> float`
    ///  - `float + string -> string`
    ///  - `float + bool -> bool`
    ///
    /// `string`:
    ///  - `string + int -> string`
    ///  - `string + float -> string`
    ///  - `string + string -> string`
    ///  - `string + bool -> bool`
    ///  - `string + any -> any`
    ///
    /// `bool`:
    ///  - `bool + int -> bool`
    ///  - `bool + float -> bool`
    ///  - `bool + string -> bool`
    ///  - `bool + bool -> bool`
    ///  - `bool + any -> any`
    ///
    /// `any`:
    ///  - `any + int -> any`
    ///  - `any + float -> any`
    ///  - `any + string -> any`
    ///  - `any + bool -> any`
    ///  - `any + any -> any`
    ///
    /// @param left  left type
    /// @param right right type
    /// @return higher type
    ///
    public static CompiledType findHigherType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (left == NOTHING) {
            return right;
        }
        if (right == NOTHING) {
            return left;
        }
        if (left == ANY || right == ANY) {
            return ANY;
        }
        if (left == DATA || right == DATA) {
            return ((left == DATA && right == DATA) || left == ENUM || right == ENUM) ? DATA : ANY;
        }
        if (left == ENUM || right == ENUM) {
            return left == ENUM && right == ENUM ? ENUM : ANY;
        }
        if (left.ordinal() < right.ordinal()) {
            return right;
        }
        return left;
    }
}
