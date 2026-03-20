plugins {
    `java-gradle-plugin`
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(project(":compiler"))
    implementation("com.fasterxml.jackson.core:jackson-databind:2.19.0")
    implementation("com.fasterxml.jackson.datatype:jackson-datatype-jdk8:2.19.0")
}

gradlePlugin {
    plugins {
        create("capybaraPlugin") {
            id = "pl.grzeslowski.capybara"
            implementationClass = "pl.grzeslowski.capybara.CapybaraPlugin"
        }
    }
}
