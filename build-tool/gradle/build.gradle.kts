plugins {
    `java-gradle-plugin`
    `maven-publish`
}

group = "dev.capylang"
version = rootProject.version

base {
    archivesName.set("capy-gradle")
}

val capybaraVersion = providers.gradleProperty("capybaraVersion")
val githubPackagesUser = providers.environmentVariable("GITHUB_ACTOR")
    .orElse(providers.gradleProperty("gpr.user"))
val githubPackagesKey = providers.environmentVariable("GITHUB_TOKEN")
    .orElse(providers.gradleProperty("gpr.key"))
val githubPackagesRepository = providers.environmentVariable("GITHUB_REPOSITORY")
    .orElse(providers.gradleProperty("gpr.repository"))
    .orElse("magx2/Capybara")

repositories {
    mavenCentral()
    ivy {
        name = "CapybaraRelease"
        url = uri("https://github.com/magx2/Capybara/releases/download/release/${capybaraVersion.get()}")
        patternLayout {
            artifact("[artifact]-[revision].[ext]")
        }
        metadataSources {
            artifact()
        }
    }
}

dependencies {
    implementation(project(":capy"))
    compileOnly("dev.capylang:capybara-lib:${capybaraVersion.get()}@jar")
    testImplementation(gradleTestKit())
    testImplementation(platform("org.junit:junit-bom:5.13.4"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    testImplementation("org.junit.jupiter:junit-jupiter-params")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.test {
    useJUnitPlatform()
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
            url = uri("https://maven.pkg.github.com/${githubPackagesRepository.get()}")
            credentials {
                username = githubPackagesUser.orNull
                password = githubPackagesKey.orNull
            }
        }
    }
}
