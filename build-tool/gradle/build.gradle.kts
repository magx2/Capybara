plugins {
    `java-gradle-plugin`
}

repositories {
    mavenCentral()
}

gradlePlugin {
    plugins {
        create("capybaraPlugin") {
            id = group.toString()
            implementationClass = "$group.CapybaraPlugin"
        }
    }
}
