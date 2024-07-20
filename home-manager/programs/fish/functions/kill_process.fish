function kill_process
    ps -ef | grep $argv
    ps -ef | grep $argv | awk '{print $2}' | xargs kill -9  > /dev/null 2>&1
end
