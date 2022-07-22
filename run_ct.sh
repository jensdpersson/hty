if [ ! -d "ct/logs" ]; then
  mkdir ct/logs
fi
mkdir -p ebin && erlc -o ebin src/*.erl && ct_run -logdir ct/logs -dir ct -pz ebin
