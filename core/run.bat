
del examples\logs\*.mcts


for /L %%n in (1, 1, 10) do swipl -l FragPL.pl -g frag('../examples/worker/worker') -g halt




