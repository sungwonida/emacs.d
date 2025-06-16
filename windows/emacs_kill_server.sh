#!/bin/bash

ps -ef | grep -P ".*[0-9]+\ emacs --daemon" | head -n1 | awk '{print $2}' | xargs kill 2>/dev/null
