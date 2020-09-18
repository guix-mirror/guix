" This appends all of the vim plugins to the end of Vim's runtimepath.
for directory in ["/run/current-system/profile", $HOME . "/.guix-profile", $GUIX_PROFILE, $GUIX_ENVIRONMENT]
    let vimplugins = directory . "/share/vim/vimfiles"
    if isdirectory(vimplugins)
        let &rtp = join([&rtp,vimplugins], ',')
    endif
endfor
