:- bundle(chat80).
version('1.0').
depends([core]).
alias_paths([
    chat80 = 'src'
]).
%
cmd('chat80', [main='cmds/chat']).
%
lib('src').
%
manual('chat80', [main='doc/SETTINGS.pl']).


