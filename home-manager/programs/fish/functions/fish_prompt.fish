function fish_prompt
    if [ $status -eq 0 ]
        set status_face (set_color 50fa7b)"(*´ω｀*) < "
    else
        set status_face (set_color ff5555)"｡+ﾟ(∩´﹏'∩)ﾟ+｡ < "
    end
    printf '%s %s' (set_color f1fa8c)(prompt_pwd) $status_face
end
