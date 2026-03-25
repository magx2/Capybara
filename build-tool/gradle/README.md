# Capybara Gradle Plugin

This module publishes the Gradle plugin `pl.grzeslowski.capybara`.

## Publish To GitHub Packages

The plugin is published as Maven artifacts to GitHub Packages for this repository:

- repository: `https://maven.pkg.github.com/grzeslowski/capybara`
- plugin id: `pl.grzeslowski.capybara`

Local publishing requires credentials with `write:packages`.

You can provide them either as environment variables:

```text
GITHUB_ACTOR=<github-user>
GITHUB_TOKEN=<github-token>
```

or in `~/.gradle/gradle.properties`:

```properties
gpr.user=<github-user>
gpr.key=<github-token>
```

Then publish with:

```powershell
./gradlew.bat :build-tool:gradle:publish
```

## Use In Another Gradle Project

Because the plugin is published to GitHub Packages instead of the Gradle Plugin Portal, consumers need to add the repository in `settings.gradle`.

`settings.gradle`

```groovy
pluginManagement {
    repositories {
        gradlePluginPortal()
        maven {
            url = uri("https://maven.pkg.github.com/grzeslowski/capybara")
            credentials {
                username = providers.gradleProperty("gpr.user").get()
                password = providers.gradleProperty("gpr.key").get()
            }
        }
    }
}
```

`build.gradle`

```groovy
plugins {
    id 'pl.grzeslowski.capybara' version '0.2.0'
}
```

Consumer credentials can be stored in `~/.gradle/gradle.properties`:

```properties
gpr.user=<github-user>
gpr.key=<github-token>
```

`read:packages` is enough for consuming the plugin.

## Release Workflow

The GitHub release workflow publishes the plugin automatically for release tags.
