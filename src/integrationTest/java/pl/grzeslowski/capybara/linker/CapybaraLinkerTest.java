package pl.grzeslowski.capybara.linker;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.util.List;

class CapybaraLinkerTest {
    @Test
    void algebraicTypes() {
        String input = """
                // algebraic type
                type Shape = Circle | Rectangle
                data Circle { radius: float }
                data Rectangle { width: float, height: float }
                
                fun area(shape: Shape): float =
                    match shape with
                    | Circle { radius } => 3.14 * radius ^ 2
                    | Rectangle { width, height } => width * height
                
                fun da_vinci(shape: Shape): Shape =
                    match shape with
                    | Circle { radius } => Rectangle { width : radius * 2, height : radius * 2 }
                    | Rectangle { width, height } => Circle { radius : (width + height) / 4 }
                
                // type with common value
                type Person { name: string, age: int } = Student | Teacher
                data Student { grade: int }
                data Teacher { subject: string }
                """;

        var functional = CapybaraParser.INSTANCE.parseFunctional(input);
        var module = new Module("Main", "/capybara", functional);
        var program = new Program(List.of(module));
        var link = CapybaraLinker.INSTANCE.link(program);
        System.out.println(link);
    }
}
