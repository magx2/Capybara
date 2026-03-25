plugins {
    `java-gradle-plugin`
    `maven-publish`
}

group = "dev.capylang"
version = rootProject.version

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
            id = "dev.capylang"
            implementationClass = "dev.capylang.CapybaraPlugin"
        }
    }
}

publishing {
    repositories {
        maven {
            name = "GitHubPackages"
            url = uri("https://maven.pkg.github.com/grzeslowski/capybara")
            credentials {
                username = System.getenv("GITHUB_ACTOR")
                    ?: findProperty("gpr.user") as String?
                password = System.getenv("GITHUB_TOKEN")
                    ?: findProperty("gpr.key") as String?
            }
        }
    }
}
