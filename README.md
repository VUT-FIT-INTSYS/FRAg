# FRAg: Flexibly Reasoning BDI Agent

This repository contains the FRAg system - an AgentSpeak(L)[[2]](#2) interpreter using the late variable bindings strategy 
implemented in [SWI Prolog](https://www.swi-prolog.org/).

The system is still under development, the current solution is in "alpha" version. 
More detailed information can be found in our paper[[1]](#1) 
published at the [ICCART 2024](https://icaart.scitevents.org/) conference.

## How to set up and run FRAg

### Linux

Install SWI Prolog. The exact installation process can vary depending on your distro.

* For RedHat-based systems (Fedora/RHEL/CentOS), run:

  ```
  $ sudo dnf install pl
  ```
  
* Ubuntu does not have SWI Prolog in base repositories. First, you need to add a SWI Prolog repository:

  ```
  $ sudo add-apt-repository ppa:swi-prolog/stable
  $ sudo apt-get update
  ```

  Then, you can install SWI Prolog:

  ```
  $ sudo apt-get install swi-prolog
  ```

Navigate to the place you want to have your FRAg repository and clone it: 

```
cd <your-desired-location>
git clone https://github.com/VUT-FIT-INTSYS/FRAg.git
```

To run FRAg example, navigate to the repository cloned in the prevous step and run:

```
$ swipl core\FRAgPL.pl
```

Then consult:

```
?- frag('../examples/trader/trader').
```

Output is located in  `examples\trader\*.out` files. Filename depends on setting of the trader 
multiagent system in `examples\trader\trader.mas2fp`

### Windows
Currently, Windows OS is not fully supported. However, if you want to run FRAg on Windows, you can use [WSL](https://learn.microsoft.com/en-us/windows/wsl/install).
Once you have WSL and your preferred linux distro installed, you can proceed with the Linux installation and execution steps above (tested on Windows 11 and Ubuntu in WSL).

## Program description

The program consists of agent code and a multiagent system setting.

### Agent code (examples\trader\trader.fap)

This file contains a procedure to run and analyse the examples we presented at the 
[ICCART 2024](https://icaart.scitevents.org/) conference.
The program on which we tested the parameters of its execution with early or late binfings involved
a souvenir (card) shop. The shopkeeper sold cards to interested buyer if there were
a matching seller offering the desired card at a price acceptable to the buyer. More details are given 
in the article[[1]](#1).

The code of such an agent in AgentSpeak(L) would be as follows:

```
+!sell : wants(Buyer, CD, MAX_Price)
    <- ?offers(Seller, CD, Price);
       Price<=Max_Price;
       sell(Seller, Buyer, CD, Price);
       !sell.
```

In FRAg, this program is written in the AgentSpeak(L) dialect suitable for interpretation in PROLOG. 
Specifically, similar code with a few modifications is created as follows:

```          
plan(ach,
     sell,                        
     [not(closed),buyer(Buyer, CD, Buy_Price)], 
     [
	test(seller(Seller, CD, Sel_Price)),
       	rel(Buy_Price > Sel_Price),
        act(card_shop, sell(Seller, Buyer, CD)),
        act(printfg(
            '~w selling ~w CD ~w for ~w', 
            [Seller, Buyer, CD, Sel_Price])),
        ach(sell)
     ]).
```

The first change is in the contextual conditions of the plan. Because of the trading termination at some 
point, we use the query not(closed). Atom `closed` is add to the agent's belief base when a deal is closed. 
The agent receives this atom from the environment it is assigned to at initialization. Therefore, also included 
in the code is  a second plan definition for the same `sell` event, which will tell the agent if the `closed` 
atom exists in its belief base will terminate.

```
plan(ach,
     sell,                        
     [closed], [
       act(printfg('It is closed, finish'))]).
```


### Multiagent system settings (examples\trader\trader.mas2fp)

The settings for the multi-agent card trading system can be found in the `trader.mas2fp` file. This extension is used 
by FRAg as the default for metafiles representing a population of agents, binding  them to environments along with 
setting parameters for both types of these elements, which together form the multiagent system.
The code is listed below

```
% a,
include_environment("/environments/shop/shop.pl").

% b,
set_agents([(control, terminate(timeout, 1000)),
             (bindings, early),
             (reasoning, random_reasoning),
             (environment, card_shop)]).		

% c,
set_environment(card_shop, [(closing, 750), (s_lambda, 0.1), (b_lambda, 0.1), 
                            (b_price,[0.4, 0.2]), (s_price, [0.6, 0.2]),
                            (products, [8, 0.3, 0.1]), 
                    %       (episoding, (real_time, 0.0001)) ]).
                            (episoding, sim_time) ]).

% d,
load("paul","../examples/trader/trader",1,[(debug, systemdbg), (debug, reasoningdbg)]).
load("peter","../examples/trader/trader",1,[(debug, systemdbg), (bindings, late), (debug, reasoningdbg)]).
```

This code is interpreted by the FRAg system and the following is done:

### Load environment (a)
The environment is loaded from the file `core/environments/shop/shop.pl`. This file is a PROLOG module 
called `card_shop` and the name of this module still represents it in the FRAg system. 
This environment will simulate everything needed for a trading agent and is described later in this text.

### Sets the default agent parameters (b)
Each subsequent agent loaded, if these parameters are not predefined in its definition
below, will have them set as specified in the parameter list in the atom `set_agents(List_Of_Parameters)`. 
Specifically, the following is set here:

1. The agent terminates after 1000 iterations in the control loop.
    The strategy for binding variables during agent execution is `early`.
2. The choice of a plan as a means of event processing, intention for execution, and substitutions when multiple
substitutions are possible is random (the last choice only makes sense in the case of early bindings).
3. Each agent is placed in the `card_shop` environment.

For initial experimentation with this example, we recommend not changing these parameters.

### Sets the parameters of the environment (c)
In this case the `card_shop` environment, to which agents will be deployed. 
What parameters can be set depends on the implementation of the environment and can be different for each environment. 
The following are set here.

### Load agents (d)
Two agents will be uploaded (since the second term of the `load` atom is 1) named Peter and Paul. If the second term 
is greater than one, more agents would be loaded with the prefix *Peter* or *Paul* followed by a number from 1 upwards.
The debug statements (as pairs (key, value) ) are set in the list, both system and reasoning progress when interpreting 
the agent, these pairs can be removed and then the listing will be brief and will only contain what the agents send 
to the console for printing. Agent *Peter* has overridden the setting of working with variables to 'late' 
and therefore does not use the default `early`.

##  Output description

Each agent has its own `.out` file into which the agent's entire state is logged for each iteration of 
the interpreter loop. 

```
[RSNDBG] STATE IN LOOP 2

:: vvvvvvvvvvvvvvvvvvvvvvvvvv
:: Name:paul
:: LOOP 2
:: INTENTIONS: No intentions
:: EVENTS {
event(1,ach,sell,null,[[]],active,[]);
}
:: BELIEFS {
fact(product(cd1,100));
fact(product(cd2,80));
fact(product(cd3,68));
fact(product(cd4,110));
fact(product(cd5,50));
fact(product(cd6,90));
fact(product(cd7,110));
fact(product(cd8,55));
fact(product(cd9,150));
fact(stats_([sold([sold(peter,0),sold(paul,0)]),buyers(0),sellers(0)]));
fact(episode(3));
fact(seller(seller1,cd1,3));
}
:: ^^^^^^^^^^^^^^^^^^^^^^^^^^
```

In the above example, we can see that agent *paul* in the second iteration has no intention yet. 
However, an event has occurred - the agent has been given an achievement goal `sell`. 
In the agent's belief base, there is information about the products and their prices, what the current episode is 
and that a seller named `seller1` wants to buy `cd1` for a price of 3.

```
...
fact(closed);
fact(episode(751));
}
:: ^^^^^^^^^^^^^^^^^^^^^^^^^^

[SYSDBG] Agent paul finished in 458 steps. 

```

The file contains logs in the same format until atom `closed` appears in the belief base and 
the simulation is terminated.


## References
<a id="1">[1]</a>
VÍDEŇSKÝ František, ZBOŘIL František, BERAN Jan, KOČÍ Radek a ZBOŘIL František V.. Comparing Variable Handling Strategies in BDI Agents: Experimental Study. In: Proceedings of the 16th International Conference on Agents and Artificial Intelligence - Volume 1. Rome, 2024, s. 25-36. ISBN 978-989-758-680-4.

<a id="2">[2]</a>
Anand S. Rao, 1996. AgentSpeak(L): BDI Agents Speak Out in a Logical Computable Language. Proceedings of Seventh European Workshop on Modelling Autonomous Agents in a Multi-Agent World (MAAMAW-96).
