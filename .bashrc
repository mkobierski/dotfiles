# ------------------------
# path
# ------------------------


# Some shells will use /cygdrive/c, others will just use /c
root_path=/c
cygdrive=/cygdrive/c
if [ -d $cygdrive ]; then
    root_path=$cygdrive
fi
#PATH=
#PATH=$root_path/openmodelica1.9.1/mingw/bin
#PATH=$PATH:$root_path/MinGW/msys/1.0/bin
#PATH="$PATH:$root_path/Program Files (x86)/Git/bin"

PATH=$root_path/cygwin64/bin

PATH=$PATH:/usr/x11R6/bin
PATH=$PATH:/usr/bin
PATH=$PATH:`cygpath -S`
PATH=$PATH:`cygpath -W`
PATH=$PATH:$root_path/usersoftware
#PATH="$root_path/MinGW/bin:$root_path/MinGW/msys/1.0/bin:$PATH"
PATH="$root_path/Program Files (x86)/CMake/bin:$PATH"
PATH="$PATH:$root_path/Python27/Scripts"

# replace any cygdrive paths with the root path
PATH=$(echo $PATH | sed -e s^$cygdrive^$root_path^g)


#export PS1='\u@\h:\[\033[1;34m\]\w\[\033[0;33m\]$(git branch 2> /dev/null | sed
#-e '/^[^*]/d' -e "s/* \(.*\)/[\1$([[ $(git status 2> /dev/null | tail -n1) !=
#"nothing to commit, working directory clean" ]] && echo "*")]/")\[\e[0m\]$ '

export PS1="\e[0;32m\u@\h\e[m \e[0;33m\w\e[m\n\$ "


export PATH;

# need quotes in the following around path in case it contains special characters
alias mingw_mode="PATH=\"$root_path/MinGW/bin:$root_path/MinGW/msys/1.0/bin:$PATH\""
alias grep="/usr/bin/grep $GREP_OPTIONS"
alias git="$root_path/Program\ Files\ \(x86\)/Git/bin/git.exe"
alias firefox="$root_path/Program\ Files\ \(x86\)/Mozilla\ Firefox/firefox.exe"

# Unlimited log decorated and graphed
alias guldg="git log --decorate --graph --color"
# Limited to 20 entries
alias gldg="guldg -20"

alias guldgo="guldg --oneline --format=\"%C(auto)%h %d %ar - %an: %s\""
alias gldgo="gldg --oneline --format=\"%C(auto)%h %d %ar - %an: %s\""

alias gstat="git status -uno"
alias gfetch="git fetch --progress -v"
alias gwhere="gstat && gldgo"
alias gsupdate="git submodule update --recursive --init"

# We always want the /usr/bin versions of these
alias ls="/usr/bin/ls"
alias which="/usr/bin/which"
unset GREP_OPTIONS

# Change/List directory
cld () { cd $1 && ls; }

showdiff1() {
    fname=$1
    case "x$2" in
        "xworking")
            git diff -w > $fname
            ;;
        "xcached")
            git diff -w --cached > $fname
            ;;
        *)
            git show -w $2 > $fname
            ;;
    esac
    return 0
}
showdiff2() {
    fname=$1
    git diff -w $2 $3 > $fname
    return 0
}
showdiff() {
    fname=temp$RANDOM.diff
    retval=0
    case "x$#" in
        "x1")
            showdiff1 $fname $1
            retval=$?
            ;;
        "x2")
            showdiff2 $fname $1 $2
            retval=$?
            ;;
        *)
            echo "showdiff: Too many arguments provided"
            return 1
            ;;
    esac
    if test $retval -ne 0; then
        echo "showdiff: Something went wrong"
        return $retval
    fi
    (emacsclient $fname && rm $fname) &
    return 0
}

grebase() {
    gecho() {
        echo "grebase: $*"
        return 0;
    }

    col_num() {
        echo $2 | awk '{ print $'$1' }'
    }
    local orig_branch_str=`git status -uno | grep "On branch"`
    local orig_branch_name=`col_num 3 "$orig_branch_str"`

    git fetch
    if test $? -ne 0; then
        gecho "Error fetching from git"
        return 1
    fi

    stash_msg=$(git stash save)
    if test $? -ne 0; then
        gecho "Stash failed"
        return 1
    fi
    gecho $stash_msg
    stashed=1
    if [[ x$stash_msg == "xNo local changes to save" ]]; then
        stashed=0
    fi

    local branches=${*:-irts_dev1 irts_dev2}
    for branch in $branches; do
        echo $branch
        git checkout $branch && git rebase
        if test $? -ne 0; then
            gecho "Error rebasing branch $branch"
            return 1
        fi
    done
    gecho "Checking-out original branch $orig_branch_name"
    git checkout $orig_branch_name
    if test $? -ne 0; then
        gecho "Error checking out original branch: $orig_branch_name"
        gecho "Reapply stashed changes manually"
        return 1
    fi

    stash_pop_res=0
    if [[ $stashed == 1 ]]; then
        echo
        gecho "Popping from previously saved stash"
        git stash pop -q
        stash_pop_res=$?
        if [[ $stash_pop_res -ne 0 ]]; then
            gecho "Failed to apply stash."
        fi
    else
        echo
        gecho "No stash pop required"
    fi

    gwhere $branches
    return $stash_pop_res
}
