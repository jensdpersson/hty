mkdir -p ebin && erlc -o ebin src/*.erl && ct_run -logdir test/logs -dir test -pz ebin
