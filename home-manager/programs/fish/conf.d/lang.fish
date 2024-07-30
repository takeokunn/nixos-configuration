if type -q ros
    fish_add_path $HOME/.roswell/bin
end

if type -q composer
    set -x COMPOSER_MEMORY_LIMIT 4G
end
