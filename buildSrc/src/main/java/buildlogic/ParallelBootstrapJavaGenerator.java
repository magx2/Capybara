package buildlogic;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/** Parallelizes the module-level work exposed by the released bootstrap Java generator. */
public final class ParallelBootstrapJavaGenerator {
    private static final int PARALLELISM = Math.min(2, Runtime.getRuntime().availableProcessors());

    private ParallelBootstrapJavaGenerator() {
    }

    public static void compileAndGenerate(ClassLoader classLoader, Path sourceInput, Path generatedOutput) {
        var program = compile(classLoader, sourceInput);
        // Release parser and linker intermediates before parallel generation allocates its large source buffers.
        System.gc();
        generate(classLoader, program, generatedOutput);
    }

    private static Object compile(ClassLoader classLoader, Path sourceInput) {
        try {
            var pathModule = Class.forName("capy.io.PathModule", true, classLoader);
            var capyInput = pathModule.getMethod("fromString", String.class)
                    .invoke(null, sourceInput.toString());
            var manifestType = Class.forName("dev.capylang.compiler.NativeProviderManifest", true, classLoader);
            var manifest = manifestType.getConstructor(List.class).newInstance(List.of());
            var configurationType = Class.forName("dev.capylang.compiler.CompileConfiguration", true, classLoader);
            var configuration = configurationType.getConstructor(Set.class, manifestType)
                    .newInstance(Set.of(), manifest);
            var capy = Class.forName("dev.capylang.cli.Capy", true, classLoader);
            var compile = method(capy, "compileSourceDirectory", 3);
            var effect = invoke(compile, capyInput, sourceInput.toString(), configuration);
            var compilation = effect.getClass().getMethod("unsafeRun").invoke(effect);
            var compilationType = compilation.getClass();
            var value = compilationType.getMethod("value").invoke(compilation);
            if (!compilationType.getName().equals("capy.lang.Either$Left")) {
                throw new IllegalStateException((String) invoke(method(capy, "compilerErrorsText", 1), value));
            }
            return value;
        } catch (ClassNotFoundException | IllegalAccessException | InstantiationException | NoSuchMethodException exception) {
            throw new IllegalStateException("Unable to compile the bootstrap program.", exception);
        } catch (InvocationTargetException exception) {
            throw failure(exception.getCause());
        }
    }

    private static void generate(ClassLoader classLoader, Object program, Path generatedOutput) {
        try {
            var programType = program.getClass();
            var modules = list(programType.getMethod("modules").invoke(program));
            var objectOrientedModules = list(programType.getMethod("objectOrientedModules").invoke(program));
            var nativeProviderCatalog = programType.getMethod("nativeProviderCatalog").invoke(program);

            var generator = Class.forName("dev.capylang.generator.JavaGenerator", true, classLoader);
            var objectIndex = invoke(method(generator, "javaObjectOrientedUnitIndex", 1), objectOrientedModules);
            var nativeIndex = invoke(method(generator, "javaNativeProviderIndex", 1), nativeProviderCatalog);

            var compiledObjects = method(generator, "javaCompiledObjectsForModule", 2);
            var interfaceModules = method(generator, "javaObjectInterfaceModulesForModule", 3);
            var modulePath = method(generator, "javaModuleRelativePath", 2);
            var moduleCode = method(generator, "javaModuleCodeWithObjects", 4);
            var testRuntimeModules = method(generator, "testRuntimeModules", 1);

            Files.createDirectories(generatedOutput);
            var tasks = new ArrayList<Callable<Void>>(modules.size());
            for (var module : modules) {
                tasks.add(() -> {
                    var objects = invoke(compiledObjects, module, objectIndex);
                    var interfaces = list(objects.getClass().getMethod("interfaces").invoke(objects));
                    writeModules(generatedOutput, list(invoke(interfaceModules, interfaces, module, modules)));
                    writeModule(
                            generatedOutput,
                            (String) invoke(modulePath, module, ".java"),
                            (String) invoke(moduleCode, module, modules, nativeIndex, objects)
                    );
                    return null;
                });
            }

            ExecutorService executor = Executors.newFixedThreadPool(PARALLELISM);
            try {
                for (var result : executor.invokeAll(tasks)) {
                    result.get();
                }
            } finally {
                executor.shutdownNow();
            }
            writeModules(generatedOutput, list(invoke(testRuntimeModules, modules)));
        } catch (ClassNotFoundException | IOException | IllegalAccessException | NoSuchMethodException exception) {
            throw new IllegalStateException("Unable to generate Java from the linked bootstrap program.", exception);
        } catch (InvocationTargetException exception) {
            throw failure(exception.getCause());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Parallel bootstrap Java generation was interrupted.", exception);
        } catch (ExecutionException exception) {
            throw failure(exception.getCause());
        }
    }

    private static Method method(Class<?> owner, String name, int parameterCount) {
        for (var method : owner.getDeclaredMethods()) {
            if (method.getName().equals(name) && method.getParameterCount() == parameterCount) {
                method.setAccessible(true);
                return method;
            }
        }
        throw new IllegalStateException("Unable to find bootstrap generator method `" + name + "`.");
    }

    private static Object invoke(Method method, Object... arguments) {
        try {
            return method.invoke(null, arguments);
        } catch (IllegalAccessException exception) {
            throw new IllegalStateException("Unable to invoke bootstrap generator method `" + method.getName() + "`.", exception);
        } catch (InvocationTargetException exception) {
            throw failure(exception.getCause());
        }
    }

    private static void writeModules(Path output, List<?> modules) throws IOException {
        for (var module : modules) {
            try {
                var type = module.getClass();
                writeModule(
                        output,
                        (String) type.getMethod("relativePath").invoke(module),
                        (String) type.getMethod("code").invoke(module)
                );
            } catch (ReflectiveOperationException exception) {
                throw new IllegalStateException("Unable to read a generated bootstrap module.", exception);
            }
        }
    }

    private static void writeModule(Path output, String relativePath, String code) throws IOException {
        var target = output.resolve(relativePath);
        Files.createDirectories(target.getParent());
        Files.writeString(target, code, StandardCharsets.UTF_8);
    }

    @SuppressWarnings("unchecked")
    private static List<Object> list(Object value) {
        return (List<Object>) value;
    }

    private static RuntimeException failure(Throwable failure) {
        if (failure instanceof RuntimeException runtimeException) {
            return runtimeException;
        }
        if (failure instanceof Error error) {
            throw error;
        }
        return new IllegalStateException("Parallel bootstrap Java generation failed.", failure);
    }
}
