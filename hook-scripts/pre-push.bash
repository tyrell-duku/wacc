#!/usr/bin/env bash


echo "--- Running pre-push hook ---"
echo "Stashing unstaged changes."
git stash --keep-index -u > /dev/null
make

if [ $? -eq 0 ]; then
  echo -e "\e[0;92mPre-push hook finished successfully.\e[0m"
  echo "-------------------------------"
  git stash pop > /dev/null
  exit 0
fi

echo -e "\e[1;31mfatal: code does not compile!\e[0m"
echo "-------------------------------"
git stash pop > /dev/null
exit 1