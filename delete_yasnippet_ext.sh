#!/usr/bin/env bash

# 批量删除文件的后缀名
for file in `find . -name "*.yasnippet"`
do
    mv $file ${file%.*}
done
