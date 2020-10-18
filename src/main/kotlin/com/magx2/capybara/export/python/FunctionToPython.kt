package com.magx2.capybara.export.python

import com.magx2.capybara.MethodToRewrite
import com.magx2.capybara.UnionWithType

fun functionToPython(function: FunctionToExport,
                     assertions: Boolean,
                     unions: Set<UnionWithType>,
                     methodsToRewrite: Set<MethodToRewrite>,
                     packageName: String): String = "def ${function.name}:\n\tpass"
//        functionToPython(
//                function.name,
//                function.returnExpression,
//                function.parameters,
//                function.assignments,
//                generateLongLambdas(function),
//                assertions,
//                unions,
//                methodsToRewrite,
//                packageName,
//        )