function pass
    # Reset scdaemon if YubiKey card is inaccessible (handles reconnect/sleep-wake)
    if not gpg-connect-agent "scd serialno" /bye >/dev/null 2>&1
        gpgconf --kill scdaemon >/dev/null 2>&1
    end
    command pass $argv
end
