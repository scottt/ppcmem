#! /bin/sh
set +e
echo "let code_version =\""
svn info | grep -v Repository | grep -v Node |grep -v Schedule |grep -v Path|grep -v URL
svn status | grep -v "\?"
echo "Build:"
date
#date --rfc-3339=seconds >> version.ml
echo -e "\n"
echo -e "\"\n"
