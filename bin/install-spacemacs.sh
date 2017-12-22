#!/usr/bin/env bash
##########################################################################
# install emacs config automatically.
# by Apple Shan (apple.shan@gmail.com)
##########################################################################

# sudo pacman -S emacs

cd ~/projects/

# clone syl20bnr/spacemacs repo
git clone -b develop https://github.com/syl20bnr/spacemacs.git spacemacs-develop

# clone appleshan/my-spacemacs-config and submodule
git clone --recursive https://github.com/appleshan/my-spacemacs-config.git spacemacs-private

# git submodule
# 如果已經抓下來才發現 submodule 是空的，可以用以下指令去抓
cd spacemacs-private
git submodule update --init --recursive

rm ~/.emacs.d
rm ~/.spacemacs.d

# 建立软链接
# ln -s <源文件> <目标> 创建一个符号链接
ln -s ~/projects/spacemacs-develop ~/.emacs.d
ln -s ~/projects/spacemacs-private ~/.spacemacs.d

# 验证软链接建立成功
la ~/ |grep emacs

################################################################################
# 按照其他 package

# all-the-icons
# Installing Fonts
M-x all-the-icons-install-fonts

# dired-k
cd ~/.spacemacs.d/local/
git clone https://github.com/appleshan/emacs-dired-k dired-k

# terminal-here
cd ~/.spacemacs.d/local/
git clone https://github.com/appleshan/terminal-here terminal-here

# pyim-bigdict.pyim
wget http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz

################################################################################
# 安装外部程序

# ditaa
mkdir -p ~/bin/java-lib/ditaa0_9
cd ~/bin/java-lib/ditaa0_9
wget https://downloads.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip
unzip ditaa0_9.zip

# graphviz

# for python
pip install rope        # a python refactoring library
pip install jedi        # Python 自动补全库
pip install flake8      # flake8 用来检查语法错误
pip install importmagic # importmagic 用来自动引入需要的包
pip install autopep8    # autopep8 用来检查 PEP8 规范
pip install yapf        # yapf 用来格式化代码
