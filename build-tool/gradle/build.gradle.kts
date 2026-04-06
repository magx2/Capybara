plugins {
    `java-gradle-plugin`
    `maven-publish`
}

group = "dev.capylang"
version = rootProject.version

val githubPackagesUser = providers.environmentVariable("GITHUB_ACTOR")
    .orElse(providers.gradleProperty("gpr.user"))
val githubPackagesKey = providers.environmentVariable("GITHUB_TOKEN")
    .orElse(providers.gradleProperty("gpr.key"))

repositories {
    mavenCentral()
}

dependencies {
    implementation(project(":capy"))
    implementation(project(":compiler"))
    testImplementation(gradleTestKit())
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
                username = githubPackagesUser.orNull
                password = githubPackagesKey.orNull
            }
        }
    }
}
