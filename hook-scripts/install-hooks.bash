#!/usr/bin/env bash

# Run the command `chmod +x .` in the scripts directory

GIT_DIR=$(git rev-parse --git-dir)

echo "Installing hooks..."
# symlinks to the pre-commit script
ln -sf $GIT_DIR/../hook-scripts/commit-msg.py $GIT_DIR/hooks/commit-msg
echo "Done!"
