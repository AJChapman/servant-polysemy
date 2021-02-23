# servant-polysemy

`servant-polysemy` is a Haskell library that makes it easier to use [Servant](https://hackage.haskell.org/package/servant) and [Polysemy](https://hackage.haskell.org/package/polysemy) together.

## Examples

Check out these examples for how to use it:

- [Server](https://github.com/AJChapman/servant-polysemy/blob/master/example/Server.hs)
- [Server using Servant.API.Generic](https://github.com/AJChapman/servant-polysemy/blob/master/example/ServerGeneric.hs)
- [Server with Swagger docs](https://github.com/AJChapman/servant-polysemy/blob/master/example/ServerWithSwagger.hs)
- [Client](https://github.com/AJChapman/servant-polysemy/blob/master/example/Client.hs)

The client will connect to either version of the server and interact with its endpoint, which simply serves up the package version.
