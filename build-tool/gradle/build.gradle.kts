plugins {
    `java-gradle-plugin`
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(project(":capy"))
    implementation(project(":compiler"))
}

gradlePlugin {
    plugins {
        create("capybaraPlugin") {
            id = "pl.grzeslowski.capybara"
            implementationClass = "pl.grzeslowski.capybara.CapybaraPlugin"
        }
    }
}
