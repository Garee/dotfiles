function fish_prompt --description 'Write out the prompt'
    set -l last_status $status
    set -l git_branch (git branch ^/dev/null | sed -n '/\* /s///p')

    # User
    set_color $fish_color_user
    echo -n (whoami)
    set_color normal

    echo -n ' at '

    # Host
    set_color $fish_color_host
    echo -n (prompt_hostname)
    set_color normal

    echo -n ' in '

    # PWD
    set_color $fish_color_cwd
    echo -n (prompt_pwd)
    set_color normal

    echo -n ' '

    if test $git_branch
        set_color $fish_color_param
        echo -n '('
        echo -n $git_branch
        echo -n ')'
        set_color normal
    end

    if test $VIRTUAL_ENV
        set_color $fish_color_operator
        echo -n ' ('
        echo -n (echo -n $VIRTUAL_ENV | rev | cut -d/ -f1 | rev)
        echo -n ')'
        set_color normal
    end

    echo

    if not test $last_status -eq 0
        set_color $fish_color_error
    end

    echo -n '$ '
    set_color normal
end
