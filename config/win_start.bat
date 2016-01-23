set DOMAIN=127.0.0.1
erl +P 1024000 -smp auto -name demo@%DOMAIN% -setcookie gameserver -boot start_sasl -config gameserver -pa ../ebin -s gameserver start -extra 12001