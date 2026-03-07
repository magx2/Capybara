package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.NOTHING;

public class CapybaraTypeFinder {
    public static LinkedType findHigherType(LinkedType left, LinkedType right) {
        if (left == right) return left;
        if (left == NOTHING) return right;
        if (right == NOTHING) return left;
        if (left == ANY || right == ANY) return ANY;

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
    public static LinkedType findHigherType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (left == NOTHING) {
            return right;
        }
        if (right == NOTHING) {
            return left;
        }
        if (left == ANY || right == ANY) {
            return ANY;
        }
        if (left.ordinal() < right.ordinal()) {
            return right;
        }
        return left;
    }
}
