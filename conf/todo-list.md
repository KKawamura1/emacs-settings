# todo list

## 真似したいもの

* https://qiita.com/yynozk/items/7dce94f770e6f3f0b26c
* http://www.shigemk2.com/entry/emacs_resudo
* https://qiita.com/kawabata@github/items/ac503ea104eac3eea602

## 改善したいもの

* https://groups.google.com/forum/#!topic/gnu.emacs.help/4UWFG7NiplQ

ここにあるとおり，変数は`customize-set-variable`で変更するのが良さげ．

## 実装したいもの

### ipythonのconfig自動生成

現状，まっさらな環境でこの.emacs.dを用いる場合，先にいくつかのコードを実行する必要がある．
これをlispで自動化したい．

実行しなければならないコード (気づいたらここに追記する)

```shell
# pipとpythonのinstall
pip install ...
```

### numpy with openblas on MacOSX
numpyはmacosではopenblasを付けてインストールしなければならない
[参考](https://docs.chainer.org/en/stable/tips.html#mnist-example-does-not-converge-in-cpu-mode-on-mac-os-x "参考")
[これ](https://www.fukudat.com/wiki/ja/Chainer/Mac "chainer on mac") に従って`site.cfg`を書き換え，`~/.numpy-site.cfg`に配置して`pip install --no-cache --no-binary :all: numpy`する
動作確認は[ここ](https://qiita.com/unnonouno/items/8ab453a1868d77a93679 "動作確認") を参照
