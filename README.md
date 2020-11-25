# servant-polysemy

`servant-polysemy` is a Haskell library that makes it easier to use [Servant](https://hackage.haskell.org/package/servant) and [Polysemy](https://hackage.haskell.org/package/polysemy) together.

## Examples

Check out these examples for how to use it:

- [Server](example/Server.hs)
- [Server with Swagger docs](example/ServerWithSwagger.hs)
- [Client](example/Client.hs)

The client will connect to either version of the server and interact with its endpoint, which simply serves up the package version.
