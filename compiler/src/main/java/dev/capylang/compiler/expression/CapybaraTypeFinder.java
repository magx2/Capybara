package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledIrModule;

import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

import static dev.capylang.compiler.CompiledIrModule.ANY;
import static dev.capylang.compiler.CompiledIrModule.DATA;
import static dev.capylang.compiler.CompiledIrModule.ENUM;
import static dev.capylang.compiler.CompiledIrModule.NOTHING;

public class CapybaraTypeFinder {
    private static final String EXPRESSION_PASS_PROPERTY = "capybara.compiler.useCapybaraExpressionCompilationPass";

    public static CompiledType findHigherType(CompiledType left, CompiledType right) {
        if (useCapybaraExpressionCompilationPass()) {
            return findHigherTypeWithCapybaraPass(left, right);
        }
        return findHigherTypeLegacy(left, right);
    }

    private static CompiledType findHigherTypeWithCapybaraPass(CompiledType left, CompiledType right) {
        var decision = expressionCompilationPassHigherTypeDecision(
                typeDecisionName(left),
                left instanceof PrimitiveLinkedType,
                isEnumLikeType(left),
                typeDecisionName(right),
                right instanceof PrimitiveLinkedType,
                isEnumLikeType(right)
        );
        if ("LEFT".equals(decision)) {
            return left;
        }
        if ("RIGHT".equals(decision)) {
            return right;
        }
        return CompiledIrModule.findPrimitiveLinkedType(decision).map(type -> (CompiledType) type).orElse(ANY);
    }

    private static String typeDecisionName(CompiledType type) {
        if (type instanceof PrimitiveLinkedType primitive) {
            return primitive.name();
        }
        return type.name();
    }

    private static CompiledType findHigherTypeLegacy(CompiledType left, CompiledType right) {
        if (left.equals(right)) return left;
        if (left == NOTHING) return right;
        if (right == NOTHING) return left;
        if (left == ANY || right == ANY) return ANY;
        if (left == ENUM && isEnumLikeType(right)) return ENUM;
        if (right == ENUM && isEnumLikeType(left)) return ENUM;
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

    private static boolean isEnumLikeType(CompiledType type) {
        if (type == ENUM) {
            return true;
        }
        if (type instanceof CompiledDataParentType parentType) {
            return parentType.enumType();
        }
        return type instanceof CompiledDataType dataType && dataType.enumValue();
    }

    /// `int`:
    /// - `int + int -> int`
    /// - `int + float -> float`
    /// - `int + String -> String`
    /// - `int + bool -> bool`
    /// - `int + any -> any`
    ///
    /// `float`:
    ///  - `float + int -> float`
    ///  - `float + float -> float`
    ///  - `float + String -> String`
    ///  - `float + bool -> bool`
    ///
    /// `String`:
    ///  - `String + int -> String`
    ///  - `String + float -> String`
    ///  - `String + String -> String`
    ///  - `String + bool -> bool`
    ///  - `String + any -> any`
    ///
    /// `bool`:
    ///  - `bool + int -> bool`
    ///  - `bool + float -> bool`
    ///  - `bool + String -> bool`
    ///  - `bool + bool -> bool`
    ///  - `bool + any -> any`
    ///
    /// `any`:
    ///  - `any + int -> any`
    ///  - `any + float -> any`
    ///  - `any + String -> any`
    ///  - `any + bool -> any`
    ///  - `any + any -> any`
    ///
    /// @param left  left type
    /// @param right right type
    /// @return higher type
    ///
    public static CompiledType findHigherType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (useCapybaraExpressionCompilationPass()) {
            var decision = expressionCompilationPassHigherTypeDecision(
                    left.name(),
                    true,
                    isEnumLikeType(left),
                    right.name(),
                    true,
                    isEnumLikeType(right)
            );
            if ("LEFT".equals(decision)) {
                return left;
            }
            if ("RIGHT".equals(decision)) {
                return right;
            }
            return CompiledIrModule.findPrimitiveLinkedType(decision).map(type -> (CompiledType) type).orElse(ANY);
        }
        return findHigherPrimitiveTypeLegacy(left, right);
    }

    private static CompiledType findHigherPrimitiveTypeLegacy(PrimitiveLinkedType left, PrimitiveLinkedType right) {
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

    private static boolean useCapybaraExpressionCompilationPass() {
        return Boolean.parseBoolean(System.getProperty(EXPRESSION_PASS_PROPERTY, "true"))
               && expressionCompilationPassAvailable();
    }

    private static boolean expressionCompilationPassAvailable() {
        try {
            Class.forName("dev.capylang.compiler.expression.ExpressionCompilationPass");
            return true;
        } catch (ClassNotFoundException ignored) {
            return false;
        }
    }

    private static String expressionCompilationPassHigherTypeDecision(
            String left,
            boolean leftPrimitive,
            boolean leftEnumLike,
            String right,
            boolean rightPrimitive,
            boolean rightEnumLike
    ) {
        try {
            var passClass = Class.forName("dev.capylang.compiler.expression.ExpressionCompilationPass");
            var method = passClass.getMethod(
                    "higherTypeDecision",
                    String.class,
                    boolean.class,
                    boolean.class,
                    String.class,
                    boolean.class,
                    boolean.class
            );
            return (String) method.invoke(null, left, leftPrimitive, leftEnumLike, right, rightPrimitive, rightEnumLike);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Unable to call Capybara expression compilation pass method `higherTypeDecision`", e);
        }
    }
}
