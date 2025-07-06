## Expanded Usage

Assuming you are using Scala 3.3 or above, add the following to your **build.sbt** file:

```sbt
resolvers += "Intervalidus" at "https://maven.pkg.github.com/rremple/intervalidus"
libraryDependencies += "rremple" %% "intervalidus" % "<version>"
```

### Other Artifacts

You probably aren't going to need them, but just in case you do, there are also these artifacts:

```sbt
libraryDependencies ++= Seq(
  "rremple" %% "intervalidus-upickle" % "<version>", // JSON transformers using com.lihaoyi:upickle
  "rremple" %% "intervalidus-weepickle" % "<version>", // JSON transformers using com.rallyhealth:weepickle-v1
  "rremple" %% "intervalidus-tinyrule" % "<version>" // sidequest rules engine used in one example (explained later)
)
```

### Scala Versions

This library is meant to be consumed by other Scala 3 applications. Though it is _theoretically_ possible to consume a
library like this from a Scala 2.13 application by using `CrossVersion.for2_13Use3` (as described in
["The Book of sbt"](https://www.scala-sbt.org/2.x/docs/en/reference/cross-building-setup.html)), it doesn't seem to work
well here. (Not even when using the scalac option `-Ytasty-reader`.) The compatibility seems to get broken by the
extensive use of Scala 3 features like opaque types, extension methods, givens (some of which don't seem to translate
well to Scala 2 implicits), inlines (for tuple processing) and some use of macros 
(which are very lightly used in core `intervalidus`, only for the
auto-derivation of a discrete value type class from an `enum`, but are more heavily used in the `intervalidus-tinyrule`
subproject). You really ought to upgrade to Scala 3!

### GitHub Package Registry

As indicated by the resolver above, I'm using the GitHub Package Registry for this. A decision I immediately regretted
when I learned that it does not, and probably never will, support unauthenticated access! Sorry for the extra friction,
but you'll need the resolver above plus some credentials for GitHub that can read packages. For this, you will need to
use a Personal Access Token (PAT) with at least the ability to read packages. For more information on PATs, see the
[GitHub docs](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).

A good way is to configure this is by using an environment variable for your Personal Access Token. `GITHUB_TOKEN` is a
good choice, as a secret with the same name is defined automatically in GitHub Actions CI builds. (Note that, although
the `Credentials` class requires a `user` argument, it is apparently ignored in GitHub authentication when using a 
PAT as the password, so we can just pass in `"_"` along with the PAT.) For example, add the following to your
**build.sbt** file:

```sbt
credentials += Credentials("GitHub Package Registry", "maven.pkg.github.com", "_", System.getenv("GITHUB_TOKEN"))
```

Locally, you will need to define the `GITHUB_TOKEN` environment variable with the value of your PAT. In GitHub Actions,
you'll need to have an `env` section that defines the environment variable based on the automatically generated secret.
For example:

```
env:
  GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

Another way, as described in the [sbt docs](https://www.scala-sbt.org/1.x/docs/Publishing.html), which is admittedly
not very portable to CI builds, is to use a local file. Add this to your **build.sbt** file:

```sbt
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
```

And then store/protect your credentials locally in **.sbt/.credentials** using the same Personal Access Token:

```
realm=GitHub Package Registry
host=maven.pkg.github.com
user=<your_github_username>
password=<your_github_personal_access_token>
```

