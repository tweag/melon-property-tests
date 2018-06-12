# Property based random testing for Melon smart contract

This project defines commands and invariants for the Melon fund smart-contract.
The test-cases will issue a random sequence of commands and check the specified
invariants in between. A test-case will fail if any of the commands or
invariants fails unexpectedly. In that case a failure report will be generated
that contains information about the sequence of commands and the contract's
state at that point, as well as references to lines in the property-test's
source code at which the failure occurred.


## Dependencies

This project uses the [Nix package manager][nix] to manage its dependencies.
If you have Nix installed it will fetch all further dependencies automatically.

[nix]: https://nixos.org/nix/

The property-tests are written in Haskell. The bindings against the Melon fund
smart-contract are generated automatically from the binary ABI files and the
binary contract files. These binaries can either be provided externally, or
generated from this repository. The Melon fund smart-contract is contained as a
git submodule.

If the set of dependencies changes, e.g. due to new node dependencies in the
smart-contract or due to new haskell dependencies in the property-tests then
the Nix shell can be updated by issuing the following commands.

``` shell
$ nix-shell --run update-node-deps
$ nix-shell --run update-haskell-deps
```


## Clone

Clone the repository and all the git submodules. E.g.

``` shell
$ git clone git@github.com:tweag/melon-property-tests.git ./melon-property-tests
$ cd ./melon-property-tests
$ git submodule update --init --recursive
```


## Build

### Building the smart-contract

If the contract binaries are provided externally, then this step can be
skipped. Otherwise execute the following command to build the smart-contracts
in the `smart-contracts/out` subdirectory.

``` shell
$ nix-shell --run build-smart-contracts
```

### Building the property-tests

The property-tests require the contract binaries in order to build. By default
the build assumes that the included smart-contracts are used and have been
built. If the contract binaries are provided externally, then their location
must be specified using the `MELONPORT_SMARTCONTRACTS_DIR` environment
variable.

E.g. if the path `/melon/binaries` contains `Fund.abi`, `Fund.bin`,
`version/Version.abi`, etc., then the property-tests can be build by issuing
the following command.

``` shell
$ nix-shell --run '
    export MELONPORT_SMARTCONTRACTS_DIR=/melon/binaries;
    cabal new-build melon-property-tests;
    '
```


### Cleaning the build

Building against a new set of contract binaries may require to clean the
previous build products. This can be done by simply deleting the generated
`dist-newstyle` subdirectory in the top-level directory of this repository.

``` shell
$ rm -rf dist-newstyle
```


## Execution

Running the property-tests requires an available Web3 provider such as parity.
The provider URI (`host:port`) can be specified using the `WEB3_PROVIDER`
environment variable. If unspecified, the default is `http://localhost:8545`.

Assuming a Web3 provider listening on `http://localhost:9999` the
property-tests can be executed by issuing the following command. Note, that it
is necessary to change into the `melon` subdirectory.

``` shell
$ nix-shell --run '
    export WEB3_PROVIDER=http://localhost:9999
    cd melon;
    cabal new-exec melon-property-tests check;
    '
```

Additional parameters, like the number of tests to execute, can be passed as
follows.

``` shell
$ nix-shell --run '
    cd melon;
    cabal new-exec -- melon-property-tests --limit 100 --commands 500 check
    '
```

If a failure is encountered, it will print out the parameters required to
re-run the same test-case. E.g.

```
> recheck (Size 9) (Seed 1362669552689423001 (-6519104247039517787)) prop_melonport
```

The test-case can be re-checked as follows.

``` shell
$ nix-shell --run '
    cd melon;
    cabal new-exec -- \
      melon-property-tests --limit 100 --commands 500 \
      recheck-simple -- 9 1362669552689423001 -6519104247039517787
    '
```


## REPL

If it becomes necessary to iterate on a particular test-case this can be more
efficient to do within a REPL. The property-tests can be executed from within a
REPL as follows:

``` shell
$ cabal new-repl melon
ghci> :load Melon.Test
ghci> -- Run all tests with 20 repetitions and up to 200 commands.
ghci> cfg = defaultTestConfig { _tcTestLimit = 20, _tcNumCommands = 200 }
ghci> tests cfg
ghci> -- Re-check a particular test-case failure in the simple tests.
ghci> recheck_prop_melonport cfg (Size 8) (Seed 908... (-562...))
ghci> -- Re-check a particular test-case failure in the model tests.
ghci> recheck_prop_melonport_model cfg (Size 8) (Seed 908... (-562...))
ghci> -- Reload if you made changes to the test-cases.
ghci> :reload
```


## Source code

The property tests are implemented in the `melon` subdirectory of this
repository. Other subdirectories are, `nix` containing all the Nix dependency
definitions, `smart-contracts` containing the Melon fund repository, and
`hs-web3` containing the library used for Web3 and smart-contract Haskell
bindings. At this point a git subdirectory referring to a fork of the project,
since some of the required functionality is not upstream, yet.

The `melon` subdirectory contains the following notable items:

- `app/Main.hs`
    Defines the executable `melon-property-tests`. Parses command-line
    arguments and then calls into the `melon` property-tests library.
- `lib/Melon/Test.hs`
    Defines the Melon fund initialization and the random generation of
    commands. Contains the entry-point that is called from `app/Main.hs`.
- `lib/Melon/Test/Commands.hs`
    Defines the commands and invariants that are tested on the contract.
- `lib/Melon/Model/`
    Defines the property-test's model of the Melon fund.
- `lib/Melon/Context.hs`
    Defines the defaults for the Web3 provider and the call gas settings.
- `lib/Melon/TH.hs`
    Defines the defaults for the smart-contract binary location.
- `lib/Melon/ABI/`
    Defines the ABI bindings to the smart-contract. These are automatically
    generated using template-Haskell.
- `lib/Melon/Contract/`
    Defines wrappers around some of the contract's raw ABI.
- `lib/Melon/Contract/PriceFeed.hs`
    Defines the price-feed update.
- `lib/Melon/Contract/Version.hs`
    Defines the asset configuration, which assets are available, their name and
    the number of decimals.


## Invariants

The following specified invariants are implemented and tested.

- Invariant of Sum of asset balances times their price (according to price
    feed) a fund holds divided by the amount of total shares in existence of
    this fund (`= totalSupply`) is equal to its `sharePrice`
    ```
    sumForAllAssets(assetBalance * assetPrice) / totalShares == sharePrice
    ```

    Tested in `checkSharePrice`, and `simpleCheckSharePrice`.

    Found to only hold up to a certain precision. For realistic asset prices
    and mild variations over time the property holds to a precision of eight
    decimal places. In case of large differences between asset prices (relative
    to `max(uint256)`) and price variations on the order of 100%, differences
    in the expected and actual share-price on the order of 10% can be observed.

- Allocation of management, performance and governance fees to manager does not
    alter the `sharePrice`

    Tested in `checkFeeAllocation`.

- Sending of assets to an exchange using a `makeOrder` does not alter the
    `sharePrice`

    Tested in `checkMakeOrder`.

- Investment and redemption by regular means (`request` and `execute`, not
    `emergencyRedeem`) do not immediately alter share price

    Tested directly in `CheckRequestInvestment`.
    
    Found to not be the case for `executeRequest` of an investment request.

    Tested indirectly in `requestValidInvestment`, and `executeValidInvestment`
    and the expected `sharePrice` according to the model.

- Fails to execute if not in line with current Risk Management module
    `function callOnExchange ...`

    Tested in `checkMakeOrder` for `makeOrder` on `MatchingMarket`.


## Discovered issues

Some instances where the specified invariants do not always hold on the Melon
fund were discovered during the development of these property-tests. The tests
have been adapted to accept the current behaviour and succeed on the Melon fund
in its current form. To allow to proceed with the development of the
property-tests. The discovered issues are due to round-off errors in the
share-price calculation, or other places, within the Melon fund, or due to
`uint256` overflows during price-calculations in case of very large asset
prices in the price-feed.

Failing test-cases or parameters causing test-case failure have been commented
out and market with `FAILURE`. These can be uncommented one-by-one to observe
the failure and test if future work on the fund has fixed the issue. The REPL
workflow described above might be a good fit in this case.
