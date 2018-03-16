# Custom completion settings
zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' expand suffix
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]} r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt ''
zstyle :compinstall filename '/home/stefan/.zshrc'

setopt nomatch
autoload -Uz compinit
compinit

# History settings
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt incappendhistory sharehistory

# Miscellaneous
setopt dotglob autocd nomatch notify
unsetopt beep alwayslastprompt
bindkey -e

# Custom aliases
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
alias vim='nvim'
alias startVPN='systemctl start openvpn-client@ovpn.service && sudo ~/.ts3VPNFix'
alias stopVPN='systemctl stop openvpn-client@ovpn.service'
alias startCUPS='systemctl start org.cups.cupsd.service'
alias stopCUPS='systemctl stop org.cups.cupsd.service'
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias diff='colordiff'
alias mountanime='sudo mount -o uid=stefan,fmask=117,umask=007,noatime /dev/sda2 /mnt/anime'

alias grep='grep -i --color=auto'
alias fgrep='fgrep -i --color=auto'
alias egrep='egrep -i --color=auto'

alias lt='ls --sort=time -r'
alias la='ls -a'
alias ll='ls -al'

alias streambob='mpv https://www.twitch.tv/guardsmanbob'
alias streamqt='mpv https://www.twitch.tv/imaqtpie'

alias mpn='mpv -profile normalize'
alias mpa='mpv -profile audio'
alias mpr='mpv -profile reverse'
alias mprn='mpv -profile reverse --no-resume-playback'
alias mpra='mpv -profile reverseAudio'
alias mpran='mpv -profile reverseAudio --no-resume-playback'

alias monkmakro='sudo evrouter /dev/input/event* -c ~/.evrouterrc &> /dev/null'
alias barbmakro='sudo evrouter /dev/input/event* -c ~/.evrouterrc2 &> /dev/null'

#source ~/.aliases
# Git prompt
source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
setopt promptsubst

autoload -U colors && colors
local lb='%{$fg_no_bold[default]%}[%{$reset_color%}'
      rb='%{$fg_no_bold[default]%}]%{$reset_color%}'

PS1="$lb%{$fg_bold[yellow]%}%D{%H:%M}$rb"\
"$lb%{$fg_bold[green]%}%n@%m$rb"\
"$lb%{$fg_bold[blue]%}%~$rb"\
$'$(__git_ps1 "[%%{\e[38;5;70m%%}%s%%{$reset_color%%}]")'\
"%(?..$lb%{$fg_bold[red]%}%?$rb)"\
$'\n%{%(?.$fg[green].$fg[red])%}λ%{$reset_color%} '

PS2="... "

__git_files () {
    # git tab completion is really slow otherwise
    _wanted files expl 'local files' _files
}

# Don't blow up on URLs
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# historySearch
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey '^[[A' up-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
setopt HIST_FIND_NO_DUPS
if [[ -o HIST_FIND_NO_DUPS ]]; then
    local -A unique_matches
    for n in $_history_substring_search_matches; do
        unique_matches[${history[$n]}]="$n"
    done
    _history_substring_search_matches=(${(@no)unique_matches})
fi

# Less greedy word boundaries
WORDCHARS=${WORDCHARS/\/}

# Environment vars
export PATH=$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH
export PATH=$PATH:$HOME/.local/bin
export EDITOR=nvim
export BROWSER=qutebrowser
export DARCS_DO_COLOR_LINES=1
export GCC_COLORS=1
export __GL_SYNC_DISPLAY_DEVICE="DP-0"


# syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
