if [ ! -d "test/logs" ]; then
  mkdir test/logs
fi
mkdir -p ebin && erlc -o ebin -I include src/*.erl  && ct_run -include include src/*.erl -logdir test/logs -dir test -pz ebin
