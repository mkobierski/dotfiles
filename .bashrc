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
PATH=$PATH:"$root_path/Program Files/PuTTY"
PATH="$root_path/Program Files (x86)/CMake/bin:$PATH"
PATH="$PATH:$root_path/Python27/Scripts"
PATH="$PATH:$root_path/Program Files (x86)/GNU/GnuPG"
# replace any cygdrive paths with the root path
PATH=$(echo $PATH | sed -e s^$cygdrive^$root_path^g)


#export PS1='\u@\h:\[\033[1;34m\]\w\[\033[0;33m\]$(git branch 2> /dev/null | sed
#-e '/^[^*]/d' -e "s/* \(.*\)/[\1$([[ $(git status 2> /dev/null | tail -n1) !=
#"nothing to commit, working directory clean" ]] && echo "*")]/")\[\e[0m\]$ '

export PS1="\e[0;32m\u@\h\e[m \e[0;33m\w\e[m\n\$ "


export PATH;
export GIT="$root_path/Program Files (x86)/Git/bin/git.exe"

# need quotes in the following around path in case it contains special characters
alias mingw_mode="PATH=\"$root_path/MinGW/bin:$root_path/MinGW/msys/1.0/bin:$PATH\""
alias grep="/usr/bin/grep $GREP_OPTIONS"
alias git="\"$GIT\""
alias firefox="$root_path/Program\ Files\ \(x86\)/Mozilla\ Firefox/firefox.exe"

# Unlimited log decorated and graphed
alias guldg="git log --decorate --graph --color --date-order"
# Limited to 20 entries
alias gldg="guldg -20"

alias guldgo="guldg --oneline --format=\"%C(auto)%h %d %ar - %an: %s\""
alias gldgo="gldg --oneline --format=\"%C(auto)%h %d %ar - %an: %s\""
alias gldgot="gldgo --topo-order"
alias gl="gldgo"

alias gstat="git status -uno"
alias gs="gstat"
alias gfetch="git fetch --progress"
alias gf="gfetch"
alias gwhere="gstat && gldgo"
alias gsupdate="git submodule update --recursive --init"
alias gpushm="git push --recurse-submodules=on-demand"

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
            git show -m -w $2 > $fname
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
        "x0")
            showdiff1 $fname "working"
            ;;
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
    (emacsclient -n $fname > /dev/null 2>&1 && rm $fname) &
    return 0
}
find_duplicate_branches() {
    func=${FUNCNAME[0]}
    if [ $# -ne 1 ]; then
        echo "Usage: $func branch_on_remote"
        echo "    $func origin/master"
        return 1
    fi
    target_branch=$1
    all_branches=
    for branch in `git branch --all | grep --invert "\(\*\|\->\)"`; do
        on_target=`git branch --all --contains $branch | grep "$target_branch"`
        if [ "x$on_target" != "x" ]; then
            echo "$func: Found fully-merged branch: $branch"
            echo "$func: It is also on the following branches..."
            git branch --remote --contains $branch
            if [ -z "$all_branches" ]; then
                all_branches="$branch"
            else
                all_branches=$(printf '%s\n%s' "$all_branches" "$branch")
            fi
            echo
        fi
    done
    echo "$func: Summary of fully merged branches:"
    printf '%s\n' "$all_branches"
    return 0;
}
git_oldest_ancestor() {
    diff --old-line-format='' --new-line-format='' \
         <(git rev-list --since=1.year --first-parent "${1:-master}") \
         <(git rev-list --since=1.year --first-parent "${2:-HEAD}") | \
        head -1
    return 0;
}
