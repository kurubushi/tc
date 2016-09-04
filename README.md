# typecheck

TDTTの型検査器．

## build

[Stack](https://docs.haskellstack.org/en/stable/README/)というツールを用いる．
[各OS毎にビルド済みのバイナリ](https://docs.haskellstack.org/en/stable/install_and_upgrade/)も配付されている．
Stackは，`$HOME/.stack`のみをいじるので，不要になった場合はこのディレクトリを削除する．

次でビルドされる．

```bash
stack setup
stack build
```

次で，型検査器tcが`$HOME/.local/bin`にインストールされる．

```bash
stack install
```




## usage

`./test`にいくつか型検査のテストを設置した．
次のように型検査できる．

```bash
$HOME/.local/bin/tc ./test/test1.tc
```

型検査が真の場合はTrue，
偽の場合にはFalseと反例の入力木を示す．




## uninstall

次で全てを削除する．

```bash
rm -rf $HOME/.stack
rm -rf $HOME/.local/bin/tc
```




## profiling

次のようにビルドするとプロファイリング機能が付加される．
ただし型検査は遅くなる．

```bahs
stack clean
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"
stack install
```

`./tc.prof`にプロファイリング結果が残る．

```bash
$HOME/.local/bin/tc +RTS -P -RTS test/test1.tc
```
