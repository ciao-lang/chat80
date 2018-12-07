:- module(_, [], [doccfg]).

%! \title Configuration for chat80 manual
%  \author Jose F. Morales

filepath := '../src'.
filepath := '../cmds'.

doc_structure := 'chat80_doc'-[
	'chat', % (command)
	'name',
	'db/border',
	'db/cities',
	'db/contai',
	'db/countr',
	'db/ndtabl',
	'db/rivers',
	'db/world0',
	'nl/aggreg',
	'nl/clotab',
	'nl/newdic',
	'nl/newerg',
	'nl/qplan',
	'nl/scopes',
	'nl/slots',
	'nl/talkr',
	'nl/templa',
	'nl/xgrun',
	'top/ptree',
	'top/readin',
	'top/top',
	'undefine'].
