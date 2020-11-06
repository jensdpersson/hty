erlc -o ebin src/*.erl
ct_run -logdir ct/logs -dir ct -pz ebin
