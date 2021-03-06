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




## source

次のような構成である．

* src ライブラリが詰っている．
    * src/Atom 状態や木など基本的な型とその関数．
    * src/Parser 型検査のための外部DSLのパーサの実装．
    * src/Set 集合を意味するStateSetの定義とその関数．
    * src/Ta 木オートマトンに関する定義
        * src/Ta/Ata Alternating Tree Automatonに関する型と関数．
        * src/Ta/Nd Tree Automatonに関する型と関数．
    * src/Tt 木トランスデューサに関する定義
* app メイン関数が詰っている．
* test 型検査器tcに与えられるサンプルプログラム．
